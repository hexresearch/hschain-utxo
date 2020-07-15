-- | Check that program has no recursion.
module Hschain.Utxo.Lang.Core.Compile.RecursionCheck(
  recursionCheck
) where

import Data.Set (Set)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Graph as G
import qualified Data.List  as L
import qualified Data.Set   as S

-- | Check that program has no recursion
-- We should check all top level bindings and let-expressions.
recursionCheck :: CoreProg -> Bool
recursionCheck (CoreProg prog) =
     (depIsAcyclic $ fmap scombToDep prog)
  && (all checkLets prog)

-- | Dependency (lefthand side, righthand side)
type Dep = (Name, [Name])

-- | Check that dependency graph is acyclic
depIsAcyclic :: [Dep] -> Bool
depIsAcyclic deps = all isAcyclic $ G.stronglyConnComp depGraph
  where
    depGraph = fmap (\(lhs, rhs) -> (lhs, lhs, rhs)) deps

    isAcyclic = \case
      G.AcyclicSCC _ -> True
      G.CyclicSCC _  -> False

-- | Find free variables for expression.
freeVars :: Expr -> Set Name
freeVars = \case
  EVar name     -> S.singleton name
  EPrim _       -> S.empty
  EAp f a       -> freeVars f <> freeVars a
  ELet binds e  -> freeLetVars binds e
  EIf a b c     -> freeVars a <> freeVars b <> freeVars c
  ECase e alts  -> freeVars (typed'value e) <> foldMap freeAltVars alts
  EConstr _ _ _ -> S.empty
  EBottom       -> S.empty
  where
    freeAltVars CaseAlt{..} =
      freeVars caseAlt'rhs S.\\ (S.fromList $ fmap typed'value caseAlt'args)

    freeLetVars binds e = eVars <> bindVars
      where
        eVars = freeVars e S.\\ bindNames

        bindNames = S.fromList $ fmap (typed'value . fst) binds

        bindVars = fst $ L.foldl' go (mempty, mempty) binds
          where
            go (res, binded) (name, expr) =
              ((res <> freeVars expr) S.\\ binded, S.insert (typed'value name) binded)

-- | Build dependencies for a single supercmbinator
scombToDep :: Scomb -> Dep
scombToDep Scomb{..} = (scomb'name, S.toList $ freeVars $ typed'value scomb'body)

-- | Check all subexpressions that let'bindings are acyclic.
checkLets :: Scomb -> Bool
checkLets = checkLetExpr . typed'value . scomb'body

-- | Check all subexpressions that let'bindings are acyclic.
checkLetExpr :: Expr -> Bool
checkLetExpr = \case
  EAp f a       -> checkLetExpr f && checkLetExpr a
  ELet binds e  -> checkBinds binds e
  EIf a b c     -> checkLetExpr a && checkLetExpr b && checkLetExpr c
  ECase e alts  -> checkLetExpr (typed'value e) && all checkAlts alts
  EConstr _ _ _ -> True
  EVar _        -> True
  EPrim _       -> True
  EBottom       -> True
  where
    checkBinds binds e = checkLetExpr e && all (checkLetExpr . snd) binds && check binds
      where
        check bs = depIsAcyclic $ fmap (\b -> (typed'value $ fst b, S.toList $ freeVars $ snd b)) bs

    checkAlts CaseAlt{..} = checkLetExpr caseAlt'rhs


