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
import Hschain.Utxo.Lang.Core.Data.Prim (Typed(..), TypeCore, Name, typed'valueL)
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
toCoreProg :: forall m . MonadLang m => TypedLamProg -> m CoreProg
toCoreProg = fmap CoreProg . mapM toScomb . unAnnLamProg
  where
    toScomb :: TypedDef -> m Core.Scomb
    toScomb Def{..} = do
      expr <- toCoreExpr def'body
      return $ Core.Scomb
          { Core.scomb'name   = varName'name def'name
          , Core.scomb'args   = V.fromList def'args
          , Core.scomb'body   = expr
          }

    toCoreExpr :: TypedExprLam -> m (Typed ExprCore)
    toCoreExpr expr@(Fix (Ann ty _)) = fmap (\val -> Typed val ty) (cataM convert expr)
      where
        convert (Ann exprTy val) = case val of
          EVar loc name        -> specifyPolyFun loc typeCtx exprTy name          
          EPrim _ prim         -> pure $ Core.EPrim $ primLoc'value prim
          EPrimOp _ primOp     -> pure $ Core.EPrimOp primOp
          EAp _  f a           -> pure $ Core.EAp f a
          -- FIXME: We don't take recurion between let bindings into account
          ELet _ binds body    -> pure $
            let addLet (nm, e) = Core.ELet (typed'value nm) e
            in foldr addLet body binds
          ELam _ _ _           -> eliminateLamError
          EIf _ c t e          -> pure $ Core.EIf c t e
          ECase _ e alts       -> pure $ Core.ECase e (fmap convertAlt alts)
          EConstr _ consTy m n -> pure $ Core.EConstr consTy m n
          EAssertType _ e _    -> pure e
          EBottom _            -> pure $ Core.EBottom

        convertAlt CaseAlt{..} = Core.CaseAlt caseAlt'tag caseAlt'args caseAlt'rhs

        eliminateLamError = failedToEliminate "Lambda-expressions for core language. Do lambda-lifting to eliminate."

        typeCtx = mempty


-- | TODO: now we check only prelude functions.
-- But it would be great to be able for user also to write polymorphic functions.
-- We need to think on more generic rule for substitution like this.
specifyPolyFun :: MonadLang m => Loc -> TypeContext -> TypeCore -> Name -> m ExprCore
specifyPolyFun loc ctx ty name = do
  case lookupSignature name ctx of
    Just sig -> fromSignature sig
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

    toPolyVar subst argOrder =
      case mapM (H.applyToVar subst) argOrder of
        Just ts -> return $ Core.EPolyVar name ts
        Nothing -> failedToFindMonoType loc name
