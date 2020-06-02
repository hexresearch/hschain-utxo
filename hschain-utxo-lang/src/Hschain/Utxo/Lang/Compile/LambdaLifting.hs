module Hschain.Utxo.Lang.Compile.LambdaLifting(
  lambdaLifting
) where

import Hex.Common.Text

import Control.Monad.State.Strict

import Data.Fix
import Data.Set (Set)

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.List as L
import qualified Data.Set  as S

lambdaLifting :: CoreProg -> CoreProg
lambdaLifting = rename . abstract . annotateFreeVars

-- | Annotates expression tree with free vars.
annotateFreeVars :: CoreProg -> AnnProg (Set Name) Name
annotateFreeVars = fmap onDef
  where
    onDef def@Def{..} = fmap (getFreeVars initLocals) def
      where
        initLocals = S.fromList def'args

    getFreeVars :: Set Name -> Expr Name -> AnnExpr (Set Name) Name
    getFreeVars localVars (Fix x) = case x of
      EPrim p -> prim p
      EVar v  -> var v
      EAp a b -> app a b
      ELam as e -> lam as e
      EIf a b c -> iff a b c
      EConstr ty m n -> constr ty m n
      ECase e alts -> cases e alts
      ELet binds body -> letExpr binds body
      where
        getLocals = ann'note . unFix

        prim p = Fix $ Ann S.empty (EPrim p)

        var v = Fix $
          if (S.member v localVars)
            then Ann (S.singleton v) (EVar v)
            else Ann S.empty (EVar v)

        app a b = Fix $ Ann (getLocals ea <> getLocals eb) (EAp ea eb)
          where
            ea = getFreeVars localVars a
            eb = getFreeVars localVars b

        lam args e = Fix $ Ann (getLocals ebody S.\\ argVars) (ELam args ebody)
          where
            ebody   = getFreeVars newVars e
            newVars = localVars <> argVars
            argVars = S.fromList args

        iff a b c = Fix $ Ann (getLocals ea <> getLocals eb <> getLocals ec) (EIf ea eb ec)
          where
            ea = getFreeVars localVars a
            eb = getFreeVars localVars b
            ec = getFreeVars localVars c

        constr ty m n = Fix $ Ann S.empty (EConstr ty m n)

        cases = undefined

        letExpr binds body = Fix $ Ann (defnsFree <> bodyFree) (ELet ebinds ebody)
          where
            rhss = fmap (getFreeVars localVars) $ fmap snd binds
            binders = fmap fst binds
            binderSet = S.fromList binders
            bodyLocals = binderSet <> localVars
            ebinds = zip binders rhss

            freeInValues = foldMap getLocals rhss
            defnsFree = freeInValues
            ebody = getFreeVars bodyLocals body
            bodyFree = getLocals ebody S.\\ binderSet

-- | Gives name SC to all lambda-abstractions (showrt for supercombinator)
abstract :: AnnProg (Set Name) Name -> CoreProg
abstract = fmap (fmap abstractExpr)
  where
    abstractExpr :: AnnExpr (Set Name) Name -> Expr Name
    abstractExpr = cata $ \Ann{..} -> case ann'value of
      ELam args body -> lam ann'note args body
      other          -> Fix other

    lam freeVars args body =
      L.foldl (\f a -> Fix $ EAp f (Fix $ EVar a)) sc freeArgs
      where
        sc = Fix $ ELet [(scName, scRhs)] (Fix $ EVar scName)

        freeArgs = S.toList freeVars

        scName = "sc"
        scRhs  = Fix $ ELam (freeArgs ++ args) body

type RenameM a = State Int a

runRenameM :: RenameM a -> a
runRenameM a = evalState a 0

rename :: CoreProg -> CoreProg
rename prog = runRenameM $ mapM (mapM renameM) prog

renameM :: Expr Name -> RenameM (Expr Name)
renameM = cataM $ \case
  other -> return $ Fix other


freshName :: RenameM Name
freshName = do
  freshId <- get
  put $ freshId + 1
  return $ mappend "v" (showt freshId)



