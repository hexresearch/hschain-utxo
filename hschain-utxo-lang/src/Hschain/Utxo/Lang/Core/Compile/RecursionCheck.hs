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
freeVars :: ExprCore -> Set Name
freeVars = \case
  EVar name       -> fromVar name
  EPolyVar name _ -> fromVar name
  EPrim _         -> S.empty
  EAp f a         -> freeVars f <> freeVars a
  ELet binds e    -> freeLetVars binds e
  EIf a b c       -> freeVars a <> freeVars b <> freeVars c
  ECase e alts    -> freeVars e <> foldMap freeAltVars alts
  EConstr _ _ _   -> S.empty
  EBottom         -> S.empty
  where
    fromVar name = S.singleton name
    freeAltVars CaseAlt{..} =
      freeVars caseAlt'rhs S.\\ (S.fromList $ fmap typed'value caseAlt'args)

    freeLetVars binds e = eVars <> bindVars
      where
        eVars = freeVars e S.\\ bindNames

        bindNames = S.fromList $ fmap fst binds

        bindVars = fst $ L.foldl' go (mempty, mempty) binds
          where
            go (res, binded) (name, expr) =
              ((res <> freeVars expr) S.\\ binded, S.insert name binded)

-- | Build dependencies for a single supercmbinator
scombToDep :: Scomb -> Dep
scombToDep Scomb{..} = (scomb'name, S.toList $ freeVars $ typed'value scomb'body)

-- | Check all subexpressions that let'bindings are acyclic.
checkLets :: Scomb -> Bool
checkLets = checkLetExpr . typed'value . scomb'body

-- | Check all subexpressions that let'bindings are acyclic.
checkLetExpr :: ExprCore -> Bool
checkLetExpr = \case
  EAp f a       -> checkLetExpr f && checkLetExpr a
  ELet binds e  -> checkBinds binds e
  EIf a b c     -> checkLetExpr a && checkLetExpr b && checkLetExpr c
  ECase e alts  -> checkLetExpr e && all checkAlts alts
  EVar _        -> checkVar
  EPolyVar _ _  -> checkVar
  EConstr _ _ _ -> True
  EPrim _       -> True
  EBottom       -> True
  where
    checkVar = True

    checkBinds binds e = checkLetExpr e && all (checkLetExpr . snd) binds && check binds
      where
        check bs = depIsAcyclic $ fmap (\b -> (fst b, S.toList $ freeVars $ snd b)) bs

    checkAlts CaseAlt{..} = checkLetExpr caseAlt'rhs


