-- | Reorder definitions in the programm by dependencies
module Hschain.Utxo.Lang.Compile.Dependencies(
    freeVars
  , orderDependencies
) where

import Data.Fix
import Data.Foldable (toList)
import Data.Graph
import Data.Set (Set)

import Hschain.Utxo.Lang.Expr (VarName(..))
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Types (Name)

import qualified Data.List as L
import qualified Data.Set  as S
import qualified Data.Sequence as Seq

-- | Finds free variables for expression
freeVars :: ExprLam Name -> Set Name
freeVars = foldFix $ \case
  EVar _ var        -> S.singleton var
  EPrim _ _         -> mempty
  EPrimOp _ _       -> mempty
  EAp _ a b         -> a <> b
  ELet _ bs e       -> freeLet bs e
  ELam _ as e       -> e `S.difference` (S.fromList as)
  EIf _ a b c       -> a <> b <> c
  ECase _ e alts    -> e <> foldMap freeAlts alts
  EConstr _ _       -> mempty
  EAssertType _ e _ -> e
  EBottom _         -> mempty
  where
    freeLet binds body = (body `S.difference` binded) <> free
      where
        (free, binded) = L.foldl' go (mempty, mempty) binds

        go (freeSet, bindSet) (v, rhs) = (freeSet <> (rhs `S.difference` bindSet), bindSet <> S.singleton v)

    freeAlts CaseAlt{..} = caseAlt'rhs `S.difference` (S.fromList caseAlt'args)

defFreeVars :: Comb Name -> Set Name
defFreeVars Def{..} = freeVars def'body `S.difference` (S.fromList def'args)

-- | Reorders programm definitions by dependencies
orderDependencies :: LamProg -> LamProg
orderDependencies = fromDepGraph . stronglyConnComp . toDepGraph

toDepGraph :: LamProg -> [(Comb Name, Name, [Name])]
toDepGraph (LamProg prog) =
  fmap (\def -> (def, varName'name $ def'name def, S.toList $ defFreeVars def)) prog

fromDepGraph :: [SCC (Comb Name)] -> LamProg
fromDepGraph = LamProg . toList . foldMap getVertex
  where
    getVertex = \case
      AcyclicSCC v -> Seq.singleton v
      CyclicSCC vs -> Seq.fromList vs


