-- | Check that program has no recursion.
module Hschain.Utxo.Lang.Core.Compile.RecursionCheck(
    recursionCheck
  , progDependencySort
) where

import Data.Maybe
import Data.Set (Set)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Graph as G
import qualified Data.Set   as S

-- | Check that program has no recursion
-- We should check all top level bindings and let-expressions.
recursionCheck :: CoreProg -> Bool
recursionCheck cp@(CoreProg prog) =
     (depIsAcyclic $ progDependencySort cp)
  && (all checkLets prog)
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
  EVar name       -> fromVar name
  EPrim _         -> S.empty
  EPrimOp{}       -> S.empty
  EAp f a         -> freeVars f <> freeVars a
  ELet nm e body  -> freeLetVars nm e body
  EIf a b c       -> freeVars a <> freeVars b <> freeVars c
  ECase e alts    -> freeVars e <> foldMap freeAltVars alts
  EConstr _ _ _   -> S.empty
  EBottom         -> S.empty
  where
    fromVar name = S.singleton name
    freeAltVars CaseAlt{..} =
      freeVars caseAlt'rhs S.\\ (S.fromList caseAlt'args)
    freeLetVars nm e body = S.delete nm (freeVars e <> freeVars body)


-- | Build dependencies for a single supercmbinator
scombToDep :: Scomb -> Dep
scombToDep Scomb{..} = (scomb'name, S.toList $ freeVars $ typed'value scomb'body)

-- | Check all subexpressions that let'bindings are acyclic.
checkLets :: Scomb -> Bool
checkLets = checkLetExpr . typed'value . scomb'body

-- | Check all subexpressions that let'bindings are acyclic.
checkLetExpr :: ExprCore -> Bool
checkLetExpr = \case
  EAp f a        -> checkLetExpr f && checkLetExpr a
  ELet nm e body -> nm `S.notMember` freeVars e && checkLetExpr body
  EIf a b c      -> checkLetExpr a && checkLetExpr b && checkLetExpr c
  ECase e alts   -> checkLetExpr e && all (checkLetExpr . caseAlt'rhs) alts
  EVar _         -> True
  EConstr _ _ _  -> True
  EPrim _        -> True
  EPrimOp{}      -> True
  EBottom        -> True


