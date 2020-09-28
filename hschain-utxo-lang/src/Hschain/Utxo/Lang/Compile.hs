-- | compilation of our language to the core-language that is
-- used in the transaction
module Hschain.Utxo.Lang.Compile(
    compile
  , toCoreScript
) where

import Control.Lens hiding (op)
import Control.Monad

import Data.Fix
import qualified Data.Map.Strict       as Map
import qualified Data.Functor.Foldable as RS

import Hschain.Utxo.Lang.Expr hiding (Type, TypeContext)
import Hschain.Utxo.Lang.Desugar.ExtendedLC
import Hschain.Utxo.Lang.Compile.LambdaLifting
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Infer
import Hschain.Utxo.Lang.Compile.Monomorphize
import Hschain.Utxo.Lang.Core.Types        (Typed(..), TypeCore(..), Name, typed'valueL)
import Hschain.Utxo.Lang.Core.Compile.Expr (CoreProg(..), ExprCore, scomb'bodyL, coreProgToScript)
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (lookupSignature, TypeContext)
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Infer

import qualified Data.Vector as V

import qualified Language.HM       as H
import qualified Language.HM.Subst as H

import qualified Hschain.Utxo.Lang.Core.Compile.Expr as Core

toCoreScript :: Module -> Either Error Script
toCoreScript m = fmap coreProgToScript $ runInferM $ compile m

-- | Compilation to Core-lang program from the script-language.
compile :: MonadLang m => Module -> m CoreProg
compile
  =  return . substPrimOp
 <=< toCoreProg
-- <=< makeMonomorphic
 <=< specifyCompareOps
 <=< annotateTypes
  .  lambdaLifting
 <=< toExtendedLC

-- | Perform sunbstiturion of primops
substPrimOp :: CoreProg -> CoreProg
substPrimOp
  = _Wrapped' . each . scomb'bodyL . typed'valueL %~ go
  where
    go = RS.cata $ \case
      Core.EVarF v
        | Just op <- Map.lookup v Core.monoPrimopNameMap
          -> Core.EPrimOp op
      e -> RS.embed e

-- | Transforms type-annotated monomorphic program without lambda-expressions (all lambdas are lifted)
-- to Core program.
toCoreProg :: MonadLang m => TypedLamProg -> m CoreProg
toCoreProg = fmap CoreProg . mapM toScomb . unAnnLamProg

toScomb :: MonadLang m => TypedDef -> m Core.Scomb
toScomb Def{..} = do
  args <- traverse convertTyped def'args
  expr <- toCoreExpr def'body
  return $ Core.Scomb
      { Core.scomb'name   = varName'name def'name
      , Core.scomb'args   = V.fromList args
      , Core.scomb'body   = expr
      }


toCoreExpr :: MonadLang m => TypedExprLam -> m (Typed TypeCore ExprCore)
toCoreExpr expr@(Fix (Ann expressionTy _)) = do
  e  <- cataM convert expr
  ty <- toCoreType expressionTy
  return $ Typed e ty
  where
    convert (Ann exprTy val) = case val of
      EVar loc name        -> specifyPolyFun loc typeCtx exprTy name
      EPrim _ prim         -> pure $ Core.EPrim $ primLoc'value prim
      EPrimOp _ primOp     -> Core.EPrimOp <$> traverse toCoreType primOp
      EAp _  f a           -> pure $ Core.EAp f a
      -- FIXME: We don't take recurion between let bindings into account
      ELet _ binds body    -> pure $
        let addLet (nm, e) = Core.ELet (typed'value nm) e
        in foldr addLet body binds
      ELam _ xs body       -> toLambda xs body
      EIf _ c t e          -> pure $ Core.EIf c t e
      ECase _ e alts       -> Core.ECase e <$> traverse convertAlt alts
      EConstr _ consTy m _ -> do ty <- toCoreType consTy
                                 pure $ Core.EConstr (resultType ty) m
      EAssertType _ e _    -> pure e
      EBottom _            -> pure $ Core.EBottom
    
    convertAlt CaseAlt{..} = return Core.CaseAlt
      { caseAlt'args = typed'value <$> caseAlt'args
      , ..
      }
    toLambda []                  body = pure body
    toLambda (Typed x ty : vars) body = do
      e   <- toLambda vars body
      ty' <- toCoreType ty
      pure $ Core.ELam x ty' e
    typeCtx = mempty

convertTyped :: MonadLang m => Typed (H.Type loc Name) a -> m (Typed TypeCore a)
convertTyped (Typed a ty) = do
  ty' <- toCoreType ty
  return $ Typed a ty'

resultType :: TypeCore -> TypeCore
resultType (_ :-> b) = resultType b
resultType  a        = a

-- | TODO: now we check only prelude functions.
-- But it would be great to be able for user also to write polymorphic functions.
-- We need to think on more generic rule for substitution like this.
specifyPolyFun :: MonadLang m => Loc -> TypeContext -> H.Type () Name -> Name -> m ExprCore
specifyPolyFun loc ctx ty name = do
  case lookupSignature name ctx of
    Just sig -> fromSignature $ H.monoT $ typeCoreToType sig
    Nothing  -> return $ Core.EVar name
  where
    fromSignature sig
      | null args && H.isMono genTy = return $ Core.EVar name
      | otherwise                   = fromPolyType genTy args
      where
        (args, genTy) = H.splitSignature sig

    fromPolyType genTy argOrder =
      case ty `H.subtypeOf` genTy of
        Right subst -> toPolyVar subst argOrder
        Left err    -> throwError $ TypeError $ H.setLoc loc err
    -- FIXME: what to do with polymorphic expressions?
    toPolyVar subst argOrder =
      case mapM (H.applyToVar subst) argOrder of
        Just _  -> failedToFindMonoType loc name
        Nothing -> failedToFindMonoType loc name


toCoreType :: MonadLang m => H.Type loc Name -> m TypeCore
toCoreType (H.Type ty) = cataM go ty
  where
    -- FIXME: add sane error messages
    go = \case
      H.ArrowT _ a b      -> pure $ a :-> b
      H.VarT _ _          -> failedToFindMonoType noLoc "Type variable encountered"
      H.TupleT _ xs       -> pure $ TupleT xs
      H.ListT  _ a        -> pure $ ListT a
      H.ConT _ "Int"   [] -> pure IntT
      H.ConT _ "Bool"  [] -> pure BoolT
      H.ConT _ "Text"  [] -> pure TextT
      H.ConT _ "Bytes" [] -> pure BytesT
      H.ConT _ "Sigma" [] -> pure SigmaT
      H.ConT _ "Box"   [] -> pure BoxT
      H.ConT _ _ _        -> failedToFindMonoType noLoc "Unknown type"
