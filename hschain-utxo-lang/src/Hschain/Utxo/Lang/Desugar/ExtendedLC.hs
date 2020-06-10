-- | Converts our language to extended lambda-calculus
-- defined in the module @Hschain.Utxo.Compile.LambdaLifting.Expr@
module Hschain.Utxo.Lang.Desugar.ExtendedLC(
  toExtendedLC
) where

import Hex.Common.Text (showt)

import Control.Arrow (first)

import Data.Fix
import Data.Text (Text)

import Hschain.Utxo.Lang.Expr hiding (Expr, tupleT)
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Build
import Hschain.Utxo.Lang.Core.Compile.TypeCheck(arrowT, varT, tupleT, listT)
import Hschain.Utxo.Lang.Desugar.Lambda
import Hschain.Utxo.Lang.Desugar.Records

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Core.Data.Prim as P

import qualified Language.HM as H

toExtendedLC :: MonadLang m => UserTypeCtx -> Lang -> m (Expr Text)
toExtendedLC ctx = toExtendedLC' ctx <=< desugarSyntax ctx

toExtendedLC' :: MonadLang m => UserTypeCtx -> Lang -> m (Expr Text)
toExtendedLC' typeCtx = cataM $ \case
  Var _ v               -> fromVar v
  Apply _ a b           -> fromApply a b
  LamList _ ps a        -> fromLamList ps a
  PrimLet _ bs a        -> fromLet bs a
  Cons _ cons args      -> fromCons cons args
  CaseOf _ expr alts    -> fromCaseOf expr alts
  AltE _ a b            -> fromAlt a b
  FailCase _            -> fromFailCase
  PrimE _ p             -> fromPrim p
  If _ a b c            -> fromIf a b c
  Pk _ a                -> fromPk a
  Tuple _ args          -> fromTuple args
  UnOpE _ op a          -> fromUnOp op a
  BinOpE _ op a b       -> fromBinOp op a b
  GetEnv _ envId        -> fromGetEnv envId
  VecE _ e              -> fromVecExpr e
  TextE _ e             -> fromTextExpr e
  BoxE _ e              -> fromBoxExpr e
  Trace _ a b           -> fromTrace a b
  Let _ _ _             -> failedToEliminate "Complex let-expression"
  Ascr _ _ _            -> failedToEliminate "Type ascertion (Ascr)"
  InfixApply _ _ _ _    -> failedToEliminate "InfixApply"
  Lam _ _ _             -> failedToEliminate "Single argument Lam"
  RecConstr _ _ _       -> failedToEliminate "RecordConstr"
  RecUpdate _ _ _       -> failedToEliminate "RecUpdate"
  Undef loc             -> throwError $ ExecError $ Undefined loc
  where
    fromVar VarName{..} = pure $ Fix $ EVar varName'name

    fromApply a b = pure $ Fix $ EAp a b

    fromLamList pats body =fmap (\args -> Fix $ ELam args body) $ mapM fromPat pats

    fromPat = \case
      PVar _ name -> pure $ varName'name name
      _           -> failedToEliminate "Non-variable pattern-cases"

    fromLet binds body = pure $ Fix $ ELet (fmap (first varName'name) binds) body

    fromCons name args = fmap (\constr -> fun constr $ V.toList args) (fromConstrName name)

    fromConstrName name = do
      ConsInfo{..} <- getConsInfo typeCtx name
      return $ Fix $ EConstr (fromType consInfo'type) consInfo'tagId consInfo'arity

    fromCaseOf expr alts = fmap (Fix . ECase expr) $ mapM fromCaseAlt alts

    fromCaseAlt CaseExpr{..} = case caseExpr'lhs of
      PCons _ cons ps -> do
        tagId <- fmap consInfo'tagId $ getConsInfo typeCtx cons
        args  <- mapM fromPat ps
        return $ CaseAlt
                  { caseAlt'tag  = tagId
                  , caseAlt'args = args
                  , caseAlt'rhs  = caseExpr'rhs
                  }
      _               -> failedToEliminate "Non-constructor case in case alternative"

    fromAlt = undefined

    fromFailCase = undefined

    fromPrim p = pure $ Fix $ EPrim $ case p of
      PrimInt n       -> P.PrimInt n
      PrimString txt  -> P.PrimText txt
      PrimBool b      -> P.PrimBool b
      PrimSigma sigma -> P.PrimSigma $ flip cata sigma $ \case
                            SigmaPk k    -> P.SigmaPk k
                            SigmaAnd as  -> P.SigmaAnd as
                            SigmaOr as   -> P.SigmaOr as


    fromIf c t e = pure $ Fix $ EIf c t e

    fromPk a = pure $ ap1 (var "pk") a

    fromTuple args = pure $ Fix $ EConstr ty tagId arity
      where
        arity = V.length args
        ty    = foldr (\v rhs -> arrowT v rhs) tyRhs vs
        tyRhs = tupleT vs
        vs    = fmap (varT . mappend "a" . showt) [1 .. arity]
        tagId = 0

    -- | TODO: how to handle tuple extractor and do we really need it
    -- if we have pattern-matching and case expressions?
    fromUnOp op a = pure $ ap1 (var $ fromOp op) a
      where
        fromOp = \case
          Not -> "not"
          Neg -> "negate"
          TupleAt _ _  -> error "TODO: tuple accessor"

    -- | TODO: Maybe we should consider to use special type for primary operators
    -- instead of relying on string names
    fromBinOp op a b = pure $ ap2 (var $ fromOp op) a b
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

    fromGetEnv envId = pure $ case envId of
      Height _    -> var "getHeight"
      Input _ a   -> ap2 (var "listAt") (var "getInputs") a
      Output _ a  -> ap2 (var "listAt") (var "getOutputs") a
      Self _      -> var "getSelf"
      Inputs _    -> var "getInputs"
      Outputs _   -> var "getOutputs"
      GetVar _ a  -> ap1 (var "getVar") a  -- todo: consider typing (now it's polymorphic but we need to be more specific)
                                           -- we should introudce getVarInt, getVarString etc.

    fromVecExpr expr = pure $ case expr of
      NewVec _ args     -> newVec args
      VecAppend _ a b   -> ap2 (var "++") a b
      VecAt _ a b       -> ap2 (var "listAt") a b
      VecLength _       -> var "length"
      VecMap _          -> var "map"
      VecFold _         -> var "foldl"
      where
        newVec args = V.foldr cons nil args

        cons a as = ap2 (Fix $ EConstr consTy 1 2) a as
        nil       = Fix $ EConstr nilTy 0 0

        nilTy  = listT (varT "a")
        consTy = arrowT (varT "a") (arrowT (listT (varT "a")) (listT (varT "a")))

    fromTextExpr expr = pure $ case expr of
      TextAppend _ a b    -> ap2 (var "<>") a b
      ConvertToText tag _ -> var (mappend "show" $ fromTextTag tag)
      TextLength _        -> var "lengthText"
      TextHash _ algo     -> var (fromHashAlgo algo)
      where
        fromTextTag = \case
          IntToText    -> "Int"
          BoolToText   -> "Bool"
          ScriptToText -> "Script"  -- TODO: in low level language we don't have type for Script, or should we?

        fromHashAlgo = \case
          Sha256       -> "sha256"
          Blake2b256   -> "blake2b256"

    fromBoxExpr expr = pure $ case expr of
      PrimBox _ _       -> undefined -- TODO do we really need this case in the language?
      BoxAt _ a field   -> fromBoxField a field
      where
        fromBoxField a field = (\f -> ap1 f a) $ case field of
          BoxFieldId      -> var "getBoxId"
          BoxFieldValue   -> var "getBoxValue"
          BoxFieldScript  -> var "getBoxScript"
          BoxFieldArg key -> ap1 (var "getBoxArg") key

    fromTrace a b = pure $ ap2 (var "trace") a b

getConsInfo :: MonadLang m => UserTypeCtx -> ConsName -> m ConsInfo
getConsInfo typeCtx name = case M.lookup name $ userTypeCtx'constrs typeCtx of
      Just info -> pure info
      Nothing   -> throwError $ ExecError $ UnboundVariables [consToVarName name]

fromType :: Type -> P.Type
fromType = H.mapLoc (const ())

failedToEliminate :: MonadError Error m => Text -> m a
failedToEliminate msg = throwError $ InternalError $ FailedToEliminate msg

desugarSyntax :: MonadLang m => UserTypeCtx -> Lang -> m Lang
desugarSyntax ctx = removeRecords ctx <=< desugarLambdaCalculus

