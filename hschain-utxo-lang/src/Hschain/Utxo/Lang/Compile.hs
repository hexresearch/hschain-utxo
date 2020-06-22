-- | compilation of our language to the core-language that is
-- used in the transaction
module Hschain.Utxo.Lang.Compile(
  compile
) where

import Control.Monad

import Data.Fix

import Hschain.Utxo.Lang.Expr hiding (Type)
import Hschain.Utxo.Lang.Desugar.ExtendedLC
import Hschain.Utxo.Lang.Compile.LambdaLifting
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Infer
import Hschain.Utxo.Lang.Compile.Monomorphize
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Typed(..), Type)
import Hschain.Utxo.Lang.Monad

import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Core.Compile.Expr as Core


-- | Compilation to Core-lang program from the script-language.
compile :: MonadLang m => Module -> m Core.CoreProg
compile = toCoreProg <=< makeMonomorphic <=< annotateTypes . lambdaLifting <=< toExtendedLC

-- | Transforms type-annotated monomorphic program without lambda-expressions (all lambdas are lifted)
-- to Core program.
toCoreProg :: forall m . MonadLang m => TypedProg -> m Core.CoreProg
toCoreProg = mapM toScomb
  where
    toScomb :: AnnComb Type (Typed Name) -> m Core.Scomb
    toScomb Def{..} = do
      expr <- toCoreExpr def'body
      return $ Core.Scomb
          { Core.scomb'name = def'name
          , Core.scomb'args = V.fromList def'args
          , Core.scomb'body = expr
          }

    toCoreExpr :: AnnExpr Type (Typed Name) -> m (Typed Core.Expr)
    toCoreExpr expr@(Fix (Ann ty _)) = fmap (\val -> Typed val ty) (cataM convert expr)
      where
        convert (Ann exprTy val) = case val of
          EVar _ name          -> pure $ Core.EVar name
          EPrim _ prim         -> pure $ Core.EPrim prim
          EAp _  f a           -> pure $ Core.EAp f a
          ELet _ binds e       -> pure $ Core.ELet binds e
          ELam _ _ _           -> eliminateLamError
          EIf _ c t e          -> pure $ Core.EIf c t e
          ECase _ e alts       -> pure $ Core.ECase (Typed e exprTy) (fmap convertAlt alts)
          EConstr _ consTy m n -> pure $ Core.EConstr consTy m n
          EBottom _            -> pure $ Core.EBottom

        convertAlt CaseAlt{..} = Core.CaseAlt caseAlt'tag caseAlt'args caseAlt'rhs

        eliminateLamError = failedToEliminate "Lambda-expressions for core language. Do lambda-lifting to eliminate."

