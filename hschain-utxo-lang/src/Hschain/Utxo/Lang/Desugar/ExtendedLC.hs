-- | Converts our language to extended lambda-calculus
-- defined in the module @Hschain.Utxo.Compile.LambdaLifting.Expr@
module Hschain.Utxo.Lang.Desugar.ExtendedLC(
    toExtendedLC
  , exprToExtendedLC
  , fromType
) where

import Hex.Common.Text (showt)

import Control.Arrow (first)

import Data.Fix
import Data.Maybe
import Data.Text (Text)

import HSChain.Crypto (ByteRepr(..), encodeBase58)

import Hschain.Utxo.Lang.Expr hiding (Expr)
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Build
import Hschain.Utxo.Lang.Desugar.Lambda
import Hschain.Utxo.Lang.Desugar (bindBodyToExpr)
import Hschain.Utxo.Lang.Desugar.Case
import Hschain.Utxo.Lang.Desugar.PatternCompiler
import Hschain.Utxo.Lang.Desugar.Records
import Hschain.Utxo.Lang.Core.Compile.Expr (PrimCon(..))
import Hschain.Utxo.Lang.Core.Types (Name)
import Hschain.Utxo.Lang.Sigma (mapPkM)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Core.Types as P
import qualified Hschain.Utxo.Lang.Const as Const

import qualified Type.Check.HM as H
import qualified Data.Text as T

-- | Transforms script-language programms so that they are defined in terms of the  limited lambda-calculus.
-- Desugars syntax in many ways (like elimination of records, guards, pattern-matchings)
toExtendedLC :: MonadLang m => Module -> m LamProg
toExtendedLC = toExtendedLC' <=< desugarModule


toExtendedLC' :: MonadLang m => Module -> m LamProg
toExtendedLC' Module{..} =
  fmap (removeTopLevelLambdas . LamProg) $ mapM toDef module'binds
  where
    toDef bind = do
      body <- exprToExtendedLC module'userTypes =<< bindBodyToExpr bind
      return $ Def
        { def'name = bind'name bind
        , def'args = []
        , def'body = body
        }

exprToExtendedLC :: MonadLang m => UserTypeCtx -> Lang -> m (ExprLam Text)
exprToExtendedLC typeCtx = cataM $ \case
  Var loc v               -> fromVar loc v
  Apply loc a b           -> fromApply loc a b
  LamList loc ps a        -> fromLamList loc ps a
  PrimLet loc bs a        -> fromLet loc bs a
  Cons loc cons args      -> fromCons loc cons args
  CaseOf loc expr alts    -> fromCaseOf loc expr alts
  AltE loc a b            -> fromAlt loc a b
  FailCase loc            -> fromFailCase loc
  PrimE loc p             -> fromPrim loc p
  If loc a b c            -> fromIf loc a b c
  Tuple loc args          -> fromTuple loc args
  List loc args           -> fromList loc args
  NegApp loc a            -> fromNegApp loc a
  Ascr loc e t            -> fromAscr loc e t
  Let _ _ _               -> failedToEliminate "Complex let-expression"
  InfixApply _ _ _ _      -> failedToEliminate "InfixApply"
  Lam _ _ _               -> failedToEliminate "Single argument Lam"
  RecConstr _ _ _         -> failedToEliminate "RecordConstr"
  RecUpdate _ _ _         -> failedToEliminate "RecUpdate"
  AntiQuote loc _ _       -> throwError $ ParseError loc "AntiQuote encountered"
  where
    fromVar loc VarName{..} = pure $ Fix $ EVar loc varName'name

    fromApply loc a b = pure $ Fix $ EAp loc a b

    fromLamList loc pats body =fmap (\args -> Fix $ ELam loc args body) $ mapM fromPat pats

    fromPat = \case
      PVar _ name -> pure $ varName'name name
      _           -> failedToEliminate "Non-variable pattern-cases"

    fromLet loc binds body = pure $ Fix $ ELet loc (fmap (first varName'name) binds) body

    fromCons loc name args = fmap (\constr -> fun loc constr $ V.toList args) (fromConstrName loc name)

    fromAscr loc e t = pure $ Fix $ EAssertType loc  e (fromType $ H.stripSignature t)

    fromConstrName loc name
      | consName'name name == "Nothing" = pure $ Fix $ EConstr loc $ ConNothing (H.varT () "a")
      | consName'name name == "Just"    = pure $ Fix $ EConstr loc $ ConJust    (H.varT () "a")
      | otherwise = do
          coreDef <- fmap consInfo'coreDef $ getConsInfo typeCtx name
          return $ mkConstr loc coreDef

    mkConstr loc CoreConsDef{..}
      | arity == 0 = sumCon $ Fix $ EConstr loc ConUnit
      | arity == 1 = Fix $ ELam loc ["x"] $ sumCon $ Fix $ EVar loc "x"
      | otherwise  = Fix $ ELam loc args $  sumCon $ fun loc (Fix $ EConstr loc (ConTuple coreConsDef'tuple)) (fmap (Fix . EVar loc) args)
      where
        arity = V.length coreConsDef'tuple

        args = fmap v [1 .. arity]
        v n = T.pack $ "v" <> show n

        sumCon = case coreConsDef'sum of
          Nothing      -> id
          Just (n, ts) -> \x -> Fix $ EAp loc (Fix $ EConstr loc (ConSum n ts)) x

    fromCaseOf loc expr alts =
      case alts of
        [alt] -> case caseExpr'lhs alt of
          PCons _ cons [p] -> do
            coreDef <- fmap consInfo'coreDef $ getConsInfo typeCtx cons
            if isSingleCons coreDef
              then do
                arg <- fromPat p
                return $ Fix $ ELet loc [(arg, expr)] $ caseExpr'rhs alt
              else fromCaseOfMultiAlts loc expr alts
          _ -> fromCaseOfMultiAlts loc expr alts
        _ -> fromCaseOfMultiAlts loc expr alts

    isSingleCons CoreConsDef{..} = V.length coreConsDef'tuple == 1 && isNothing coreConsDef'sum

    fromCaseOfMultiAlts loc expr alts = fmap (Fix . ECase loc expr) $ mapM fromCaseAlt alts

    fromCaseAlt CaseExpr{..} = case caseExpr'lhs of
      PCons loc cons _ | consName'name cons == "Nothing" ->
        return $ CaseAlt
                  { caseAlt'loc  = loc
                  , caseAlt'tag  = ConNothing (H.varT () "a")
                  , caseAlt'args = []
                  , caseAlt'rhs  = caseExpr'rhs
                  }
      PCons loc cons ps | consName'name cons == "Just"    -> do
        args <- mapM fromPat ps
        return $ CaseAlt
                  { caseAlt'loc  = loc
                  , caseAlt'tag  = ConJust (H.varT () "a")
                  , caseAlt'args = args
                  , caseAlt'rhs  = caseExpr'rhs
                  }
      PCons loc cons ps -> do
        coreDef <- fmap consInfo'coreDef $ getConsInfo typeCtx cons
        args  <- mapM fromPat ps
        fromCoreCase loc coreDef args caseExpr'rhs
      PTuple loc [] -> do
        return $ CaseAlt
                  { caseAlt'loc  = loc
                  , caseAlt'tag  = ConUnit
                  , caseAlt'args = []
                  , caseAlt'rhs  = caseExpr'rhs
                  }
      PTuple loc ps -> do
        args <- mapM fromPat ps
        let arity = length ps
        return $ CaseAlt
                  { caseAlt'loc        = loc
                  , caseAlt'tag        = ConTuple $ V.fromList $ tupleArgsT arity
                  , caseAlt'args       = args
                  , caseAlt'rhs        = caseExpr'rhs
                  }
      _ -> failedToEliminate "Non-constructor case in case alternative"
      where
        tupleArgsT   arity = vs arity
        vs arity = fmap (H.varT () . mappend "v" . showt) [1 .. arity]

    fromCoreCase :: MonadLang m => Loc -> CoreConsDef -> [Name] -> ExprLam Text -> m (CaseAlt Text (ExprLam Text))
    fromCoreCase loc CoreConsDef{..} args rhs = case coreConsDef'sum of
      Nothing  | arity == 0 -> pure unitAlt
      Nothing  | arity == 1 -> unexpected "Newtype wrapper should be single element in the list"
      Nothing               -> pure tupleAlt
      Just (n, ts) -> do
        v <- getFreshVarName
        return $ CaseAlt
                   { caseAlt'loc  = loc
                   , caseAlt'tag  = ConSum n ts
                   , caseAlt'args = [v]
                   , caseAlt'rhs  = tupleCase $ Fix $ EVar loc v
                   }
      where
        tupleCase expr
          | arity == 0 = Fix $ ECase loc expr $ pure unitAlt
          | arity == 1 = rhs
          | otherwise  = Fix $ ECase loc expr $ pure $ tupleAlt

        unitAlt = CaseAlt
                    { caseAlt'loc  = loc
                    , caseAlt'tag  = ConUnit
                    , caseAlt'args = []
                    , caseAlt'rhs  = rhs
                    }
        tupleAlt = CaseAlt
                    { caseAlt'loc  = loc
                    , caseAlt'tag  = ConTuple coreConsDef'tuple
                    , caseAlt'args = args
                    , caseAlt'rhs  = rhs
                    }

        arity = V.length coreConsDef'tuple

    fromAlt _ _ _ = failedToEliminate "AltE expression. It should not be there (we need it only for type-inference check)"

    fromFailCase loc = pure $ Fix $ EBottom loc

    fromPrim loc p = fmap (Fix . EPrim loc . PrimLoc loc) $ case p of
      PrimInt n       -> pure $ P.PrimInt n
      PrimString txt  -> pure $ P.PrimText txt
      PrimBool b      -> pure $ P.PrimBool b
      PrimSigma sigma -> fmap P.PrimSigma $ mapPkM (\bs -> maybe (notKey bs) pure $ decodeFromBS bs) sigma
      PrimBytes bs    -> pure $ P.PrimBytes bs
      where
        notKey bs = throwError $ ParseError loc $ T.unwords ["Failed to decode public key:", encodeBase58 bs]

    fromIf loc c t e = pure $ Fix $ EIf loc c t e

    fromTuple loc args = pure $ fun loc (Fix $ EConstr loc (ConTuple ts)) $ V.toList args
      where
        arity = V.length args
        ts    = V.fromList $ fmap (varT . mappend "a" . showt) [1 .. arity]

    fromNegApp loc a = pure $ ap1 loc (var loc Const.negate) a

    fromList loc args = pure $ V.foldr cons nil args
      where
        cons a as = ap2 loc (Fix $ EConstr loc (ConCons (varT "a"))) a as
        nil = Fix $ EConstr loc (ConNil (varT "a"))

getConsInfo :: MonadLang m => UserTypeCtx -> ConsName -> m ConsInfo
getConsInfo typeCtx name = case M.lookup name $ userTypeCtx'constrs typeCtx of
      Just info -> pure info
      Nothing   -> throwError $ ExecError $ UnboundVariables [consToVarName name]

fromType :: H.Type loc v -> H.Type () v
fromType = H.mapLoc (const ())

desugarModule :: MonadLang m => Module -> m Module
desugarModule =
      liftToModule simplifyLet
  <=< desugarCase
  <=< liftToModuleWithCtx desugarSyntaxExpr
  <=< altGroupToTupleModule
  <=< substWildcards

desugarSyntaxExpr :: MonadLang m => UserTypeCtx -> Lang -> m Lang
desugarSyntaxExpr ctx = removeRecords ctx <=< desugarLambdaCalculus

substWildcards :: MonadLang m => Module -> m Module
substWildcards m = do
  binds <- mapM substBind $ module'binds m
  return $ m { module'binds = binds }
  where
    substBind b = do
      alts <- mapM substAlt $ bind'alts b
      return $ b { bind'alts = alts }

    substAlt a = do
      pats   <- mapM substPat $ alt'pats a
      rhs    <- mapM substExpr $ alt'expr a
      wheres <- mapM substWheres $ alt'where a
      return $ Alt
        { alt'pats  = pats
        , alt'expr  = rhs
        , alt'where = wheres
        }

    substWheres = mapM substBind

    substExpr = cataM $ \case
      Lam loc pat e      -> fmap (\p -> Fix $ Lam loc p e) $ substPat pat
      LamList loc pats e -> fmap (\ps -> Fix $ LamList loc ps e) $ mapM substPat pats
      CaseOf loc e alts  -> fmap (\as -> Fix $ CaseOf loc e as) $ mapM substCaseExpr alts
      other              -> return $ Fix $ other

    substPat = \case
      PWildCard loc     -> fmap (PVar loc) $ getFreshVar loc
      PCons loc name ps -> fmap (PCons loc name) $ mapM substPat ps
      PTuple loc ps     -> fmap (PTuple loc) $ mapM substPat ps
      other             -> return other

    substCaseExpr a = do
      lhs <- substPat $ caseExpr'lhs a
      return $ a { caseExpr'lhs = lhs }

removeTopLevelLambdas :: LamProg -> LamProg
removeTopLevelLambdas (LamProg defs) = LamProg $ fmap removeTopLevelLambdasDef defs

removeTopLevelLambdasDef :: Comb Name -> Comb Name
removeTopLevelLambdasDef def@Def{..} =
  case def'body of
    Fix (ELam _ args body) -> removeTopLevelLambdasDef $
                                    def { def'args = def'args ++ args
                                        , def'body = body
                                        }
    _                      -> def



