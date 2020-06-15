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
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Typed(..), Type)
import Hschain.Utxo.Lang.Monad

import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Core.Compile.Expr as Core

type TypedProg = AnnProg Type (Typed Name)

-- | Compilation to Core-lang program from the script-language.
compile :: MonadLang m => Module -> m Core.CoreProg
compile = toCoreProg <=< makeMonomorphic . annotateTypes . lambdaLifting <=< toExtendedLC

-- | Infers types for all subexpressions
annotateTypes :: CoreProg -> TypedProg
annotateTypes = undefined

-- | Makes types monomorphic.
makeMonomorphic :: MonadLang m => TypedProg -> m TypedProg
makeMonomorphic = undefined

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
          EVar name          -> pure $ Core.EVar name
          EPrim prim         -> pure $ Core.EPrim prim
          EAp  f a           -> pure $ Core.EAp f a
          ELet binds e       -> pure $ Core.ELet binds e
          ELam _ _           -> eliminateLamError
          EIf c t e          -> pure $ Core.EIf c t e
          ECase e alts       -> pure $ Core.ECase (Typed e exprTy) (fmap convertAlt alts)
          EConstr consTy m n -> pure $ Core.EConstr consTy m n
          EBottom            -> pure $ Core.EBottom

        convertAlt CaseAlt{..} = Core.CaseAlt caseAlt'tag caseAlt'args caseAlt'rhs

        eliminateLamError = failedToEliminate "Lambda-expressions for core language. Do lambda-lifting to eliminate."

