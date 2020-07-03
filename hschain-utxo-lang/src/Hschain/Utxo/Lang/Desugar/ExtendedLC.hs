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
import Data.Text (Text)

import Hschain.Utxo.Lang.Expr hiding (Expr, tupleT, intT, boxT, textT)
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Types (scriptToText)
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Build
import Hschain.Utxo.Lang.Core.Compile.TypeCheck(arrowT, varT, tupleT, listT, intT, boxT, textT)
import Hschain.Utxo.Lang.Desugar.Lambda
import Hschain.Utxo.Lang.Desugar (bindBodyToExpr)
import Hschain.Utxo.Lang.Desugar.Case
import Hschain.Utxo.Lang.Desugar.PatternCompiler
import Hschain.Utxo.Lang.Desugar.Records
import Hschain.Utxo.Lang.Core.Data.Prim(Name)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Core.Data.Prim as P

import qualified Language.HM as H

-- | Transforms script-language programms so that they are defined in terms of the  limited lambda-calculus.
-- Desugars syntax in many ways (like elimination of records, guards, pattern-matchings)
toExtendedLC :: MonadLang m => Module -> m CoreProg
toExtendedLC = toExtendedLC' <=< desugarModule


toExtendedLC' :: MonadLang m => Module -> m CoreProg
toExtendedLC' Module{..} =
  fmap (removeTopLevelLambdas . CoreProg) $ mapM toDef module'binds
  where
    toDef bind = do
      body <- exprToExtendedLC module'userTypes =<< bindBodyToExpr bind
      return $ Def
        { def'name = bind'name bind
        , def'args = []
        , def'body = body
        }

exprToExtendedLC :: MonadLang m => UserTypeCtx -> Lang -> m (Expr Text)
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
  Pk loc a                -> fromPk loc a
  Tuple loc args          -> fromTuple loc args
  UnOpE loc op a          -> fromUnOp loc op a
  BinOpE loc op a b       -> fromBinOp loc op a b
  GetEnv loc envId        -> fromGetEnv loc envId
  VecE loc e              -> fromVecExpr loc e
  TextE loc e             -> fromTextExpr loc e
  BoxE loc e              -> fromBoxExpr loc e
  Trace loc a b           -> fromTrace loc a b
  Let _ _ _               -> failedToEliminate "Complex let-expression"
  Ascr _ _ _              -> failedToEliminate "Type ascertion (Ascr)"
  InfixApply _ _ _ _      -> failedToEliminate "InfixApply"
  Lam _ _ _               -> failedToEliminate "Single argument Lam"
  RecConstr _ _ _         -> failedToEliminate "RecordConstr"
  RecUpdate _ _ _         -> failedToEliminate "RecUpdate"
  where
    fromVar loc VarName{..} = pure $ Fix $ EVar loc varName'name

    fromApply loc a b = pure $ Fix $ EAp loc a b

    fromLamList loc pats body =fmap (\args -> Fix $ ELam loc args body) $ mapM fromPat pats

    fromPat = \case
      PVar _ name -> pure $ varName'name name
      _           -> failedToEliminate "Non-variable pattern-cases"

    fromLet loc binds body = pure $ Fix $ ELet loc (fmap (first varName'name) binds) body

    fromCons loc name args = fmap (\constr -> fun loc constr $ V.toList args) (fromConstrName loc name)

    fromConstrName loc name = do
      ConsInfo{..} <- getConsInfo typeCtx name
      return $ Fix $ EConstr loc (fromType consInfo'type) consInfo'tagId consInfo'arity

    fromCaseOf loc expr alts = fmap (Fix . ECase loc expr) $ mapM fromCaseAlt alts

    fromCaseAlt CaseExpr{..} = case caseExpr'lhs of
      PCons loc cons ps -> do
        info <- getConsInfo typeCtx cons
        let tagId = consInfo'tagId info
            (argsT, rhsT) = H.extractFunType $ consInfo'type info
        args  <- mapM fromPat ps
        return $ CaseAlt
                  { caseAlt'loc        = loc
                  , caseAlt'tag        = tagId
                  , caseAlt'args       = zipWith P.Typed args $ fmap fromType argsT
                  , caseAlt'constrType = fromType rhsT
                  , caseAlt'rhs        = caseExpr'rhs
                  }
      PTuple loc ps -> do
        args <- mapM fromPat ps
        let arity = length ps
        return $ CaseAlt
                  { caseAlt'loc        = loc
                  , caseAlt'tag        = 0
                  , caseAlt'args       = zipWith P.Typed args $ tupleArgsT arity
                  , caseAlt'constrType = tupleConstrT arity
                  , caseAlt'rhs        = caseExpr'rhs
                  }
      _ -> failedToEliminate "Non-constructor case in case alternative"
      where
        tupleArgsT   arity = vs arity
        tupleConstrT arity = H.tupleT () $ vs arity
        vs arity = fmap (H.varT () . mappend "v" . showt) [1 .. arity]



    fromAlt _ _ _ = failedToEliminate "AltE expression. It should not be there (we need it only for type-inference check)"

    fromFailCase loc = pure $ Fix $ EBottom loc

    fromPrim loc p = pure $ Fix $ EPrim loc $ case p of
      PrimInt n       -> P.PrimInt n
      PrimString txt  -> P.PrimText txt
      PrimBool b      -> P.PrimBool b
      PrimSigma sigma -> P.PrimSigma $ flip cata sigma $ \case
                            SigmaPk k    -> P.SigmaPk k
                            SigmaAnd as  -> P.SigmaAnd as
                            SigmaOr as   -> P.SigmaOr as


    fromIf loc c t e = pure $ Fix $ EIf loc c t e

    fromPk loc a = pure $ ap1 loc (var loc "pk") a

    fromTuple loc args = pure $ fun loc (Fix $ EConstr loc ty tagId arity) $ V.toList args
      where
        arity = V.length args
        ty    = foldr (\v rhs -> arrowT v rhs) tyRhs vs
        tyRhs = tupleT vs
        vs    = fmap (varT . mappend "a" . showt) [1 .. arity]
        tagId = 0

    -- | TODO: how to handle tuple extractor and do we really need it
    -- if we have pattern-matching and case expressions?
    fromUnOp loc op a = pure $ ap1 loc (var loc $ fromOp op) a
      where
        fromOp = \case
          Not -> "not"
          Neg -> "negate"
          TupleAt _ _  -> error "TODO: tuple accessor"

    -- | TODO: Maybe we should consider to use special type for primary operators
    -- instead of relying on string names
    fromBinOp loc op a b = pure $ ap2 loc (var loc $ fromOp op) a b
      where
        fromOp = \case
          And                  -> "&&"
          Or                   -> "||"
          Plus                 -> "+"
          Minus                -> "-"
          Times                -> "*"
          Div                  -> "/"
          Equals               -> "=="
          NotEquals            -> "/="
          LessThan             -> "<"
          GreaterThan          -> ">"
          LessThanEquals       -> "<="
          GreaterThanEquals    -> ">="

    fromGetEnv _ envId = pure $ case envId of
      Height loc    -> var loc "getHeight"
      Input loc a   -> ap2 loc (var loc "listAt") (var loc "getInputs") a
      Output loc a  -> ap2 loc (var loc "listAt") (var loc "getOutputs") a
      Self loc      -> var loc "getSelf"
      Inputs loc    -> var loc "getInputs"
      Outputs loc   -> var loc "getOutputs"
      GetVar loc a  -> ap1 loc (var loc "getVar") a  -- todo: consider typing (now it's polymorphic but we need to be more specific)
                                                   -- we should introudce getVarInt, getVarString etc.

    fromVecExpr _ expr = pure $ case expr of
      NewVec loc args     -> newVec loc args
      VecAppend loc a b   -> ap2 loc (var loc "++") a b
      VecAt loc a b       -> ap2 loc (var loc "listAt") a b
      VecLength loc       -> var loc "length"
      VecMap loc          -> var loc "map"
      VecFold loc         -> var loc "foldl"
      where
        newVec loc args = V.foldr (cons loc) (nil loc) args

        cons loc a as = ap2 loc (Fix $ EConstr loc consTy 1 2) a as
        nil loc   = Fix $ EConstr loc nilTy 0 0

        nilTy  = listT (varT "a")
        consTy = arrowT (varT "a") (arrowT (listT (varT "a")) (listT (varT "a")))

    fromTextExpr _ expr = pure $ case expr of
      TextAppend loc a b    -> ap2 loc (var loc "<>") a b
      ConvertToText loc tag -> var loc (mappend "show" $ fromTextTag tag)
      TextLength loc        -> var loc "lengthText"
      TextHash loc algo     -> var loc (fromHashAlgo algo)
      where
        fromTextTag = \case
          IntToText    -> "Int"
          BoolToText   -> "Bool"
          ScriptToText -> "Script"  -- TODO: in low level language we don't have type for Script, or should we?

        fromHashAlgo = \case
          Sha256       -> "sha256"
          Blake2b256   -> "blake2b256"

    fromBoxExpr _ expr = pure $ case expr of
      PrimBox loc box     -> fromPrimBox loc box
      BoxAt loc a field   -> fromBoxField loc a field
      where
        fromBoxField loc a field = (\f -> ap1 loc f a) $ case field of
          BoxFieldId      -> var loc "getBoxId"
          BoxFieldValue   -> var loc "getBoxValue"
          BoxFieldScript  -> var loc "getBoxScript"
          BoxFieldArg key -> ap1 loc (var loc "getBoxArg") key

        fromPrimBox loc Box{..} = fun loc boxCons [id', value, script, args]
          where
            boxCons = Fix $ EConstr loc boxConsTy 0 4

            -- todo: args are not just integers
            -- consider other primitive types
            boxConsTy = foldr arrowT boxT [textT, intT, textT, listT intT]

            id'    = prim loc $ P.PrimText $ unBoxId box'id
            value  = prim loc $ P.PrimInt  $ box'value
            script = prim loc $ P.PrimText $ scriptToText box'script
            args   = Fix $ EConstr loc (listT intT) 0 0 -- todo put smth meaningful here, for now it's empty list


    fromTrace loc a b = pure $ ap2 loc (var loc "trace") a b

getConsInfo :: MonadLang m => UserTypeCtx -> ConsName -> m ConsInfo
getConsInfo typeCtx name = case M.lookup name $ userTypeCtx'constrs typeCtx of
      Just info -> pure info
      Nothing   -> throwError $ ExecError $ UnboundVariables [consToVarName name]

fromType :: Type -> P.Type
fromType = H.mapLoc (const ())

desugarModule :: MonadLang m => Module -> m Module
desugarModule =
      desugarCase
  <=< liftToModuleWithCtx desugarSyntaxExpr
  <=< altGroupToTupleModule <=< substWildcards

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

removeTopLevelLambdas :: CoreProg -> CoreProg
removeTopLevelLambdas (CoreProg defs) = CoreProg $ fmap removeTopLevelLambdasDef defs

removeTopLevelLambdasDef :: Comb Name -> Comb Name
removeTopLevelLambdasDef def@Def{..} =
  case def'body of
    Fix (ELam _ args body) -> removeTopLevelLambdasDef $
                                    def { def'args = def'args ++ args
                                        , def'body = body
                                        }
    _                        -> def


