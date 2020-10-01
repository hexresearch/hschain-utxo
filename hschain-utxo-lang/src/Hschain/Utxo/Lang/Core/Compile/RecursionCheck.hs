-- | Check that program has no recursion.
module Hschain.Utxo.Lang.Core.Compile.RecursionCheck(
    recursionCheck
  , progDependencySort
) where

import Data.Maybe
import Data.Set (Set, (\\))

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Types

import qualified Data.Graph as G
import qualified Data.Set   as S

-- | Check that program has no recursion
-- We should check all top level bindings and let-expressions.
recursionCheck :: CoreProg -> Bool
recursionCheck
  = depIsAcyclic . progDependencySort
  where
    depIsAcyclic = isJust

-- | Dependency (lefthand side, righthand side)
type Dep = (Name, [Name])

-- | Returns chain of sorted by dependency top-level combinator names
-- It's nothing if graph has cycles.
progDependencySort :: CoreProg -> Maybe [Name]
progDependencySort (CoreProg prog) = mapM getAcyclic $ G.stronglyConnComp depGraph
  where
    deps     = fmap scombToDep prog
    depGraph = fmap (\(lhs, rhs) -> (lhs, lhs, rhs)) deps

    getAcyclic = \case
      G.AcyclicSCC v -> Just v
      G.CyclicSCC _  -> Nothing


-- | Find free variables for expression.
freeVars :: ExprCore -> Set Name
freeVars = \case
  EVar name       -> S.singleton name
  EPrim _         -> S.empty
  EPrimOp{}       -> S.empty
  EAp f a         -> freeVars f <> freeVars a
  ELet nm e body  -> freeVars e <> S.delete nm (freeVars body)
  EIf a b c       -> freeVars a <> freeVars b <> freeVars c
  ECase e alts    -> freeVars e <> foldMap freeAltVars alts
  EConstr{}       -> S.empty
  EBottom         -> S.empty
  where
    freeAltVars CaseAlt{..} =
      freeVars caseAlt'rhs \\ S.fromList caseAlt'args



-- | Build dependencies for a single supercmbinator
scombToDep :: Scomb -> Dep
scombToDep Scomb{..} = (scomb'name, S.toList $ freeVars $ typed'value scomb'body)
