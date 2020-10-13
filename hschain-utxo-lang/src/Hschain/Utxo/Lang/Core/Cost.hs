-- | Estimates the cost of execution of core-script
module Hschain.Utxo.Lang.Core.Cost(
  --   Cost(..)
  -- , getProgCost
) where
{-
import Control.Applicative

import Data.Fix
import Data.Map.Strict (Map)

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.Compile.Expr

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T


-- | Cost of execution
data Cost = Cost
  { cost'time   :: !Int   -- ^ execution cost
  , cost'memory :: !Int   -- ^ memory usage
  } deriving (Show, Eq)

data CostVar
  = MonoCost Cost                         -- ^ monomorphic case gives plain cost

fromMonoCost :: CostVar -> Cost
fromMonoCost = \case
  MonoCost c -> c

-- | Costs for free variables
type CostMap = Map Name CostVar

-- | Costs for type variables
type TypeCostMap = Map Name Cost


-- | Evaluates the execution of "main" expression
getProgCost :: ExprCore -> Maybe Cost
getProgCost = exprCost mempty mempty


lookupCost :: Name -> CostMap -> Maybe CostVar
lookupCost = M.lookup

insertCost :: Name -> CostVar -> CostMap -> CostMap
insertCost = M.insert

exprCost :: TypeCostMap -> CostMap -> ExprCore -> Maybe Cost
exprCost typeCostMap costMap expr = case expr of
  EVar name         -> costVar name
  EPrim p           -> costPrim p
  EPrimOp op        -> costPrimOp op
  ELam _ _ e        -> exprCost typeCostMap costMap e
  EAp f a           -> costAp f a
  ELet name v body  -> costLet name v body
  EIf  c t e        -> costIf c t e
  ECase e alts      -> costCase e alts
  EConstr _ _       -> pure unitCost
  EBottom           -> costBottom
  where
    rec = exprCost typeCostMap costMap

    costVar name = fromMonoCost <$> lookupCost name costMap

    costPrim p = return $ primToCost p
    costPrimOp op = return $ primOpToCost op

    costAp f a = liftA2 addCost (rec f) (rec a)

    costLet name v body = do
      vCost    <- rec v
      bodyCost <- exprCost typeCostMap (insertCost name (MonoCost vCost) costMap) body
      return $ sumCost [nameCost, vCost, bodyCost]
      where
        nameCost = primToCost $ PrimText name

    costIf c t e = liftA3 (\c' t' e' -> addCost c' (maxCost t' e')) (rec c) (rec t) (rec e)

    costCase e alts = liftA2 addCost (rec e) (fmap maximumCost $ mapM costAlt alts)

    -- FIXME: We dropped types from case alternatives
    costAlt CaseAlt{..} = Just unitCost
    -- exprCost typeCostMap (appendArgs typeCostMap caseAlt'args costMap) caseAlt'rhs

    costBottom = return unitCost


sumCost :: [Cost] -> Cost
sumCost = L.foldl' addCost (Cost 0 0)

maximumCost :: [Cost] -> Cost
maximumCost = L.foldl' maxCost (Cost 0 0)

addCost :: Cost -> Cost -> Cost
addCost = appendCostBy (+)

maxCost :: Cost -> Cost -> Cost
maxCost = appendCostBy max

appendCostBy :: (Int -> Int -> Int) -> Cost -> Cost -> Cost
appendCostBy op (Cost a1 b1) (Cost a2 b2) = Cost (a1 `op` a2) (b1 `op` b2)

unitCost :: Cost
unitCost = Cost 1 1

-- | TODO think over concrete sizes
intCost, boolCost :: Cost
intCost = Cost 1 64
boolCost = Cost 1 1


primToCost :: Prim -> Cost
primToCost = \case
  PrimInt _     -> intCost
  PrimText txt  -> Cost 1 (4 * T.length txt)
  PrimBytes bs  -> Cost 1 (BS.length bs)
  PrimBool _    -> boolCost
  PrimSigma s   -> sigmaToCost s

sigmaToCost :: Sigma PublicKey -> Cost
sigmaToCost = cata $ \case
  SigmaPk _     -> publicKeyCost
  SigmaAnd as   -> sumCost $ unitCost : as
  SigmaOr  as   -> sumCost $ unitCost : as
  SigmaBool _   -> unitCost

publicKeyCost :: Cost
publicKeyCost = Cost 1 512

-- | TODO: think over concrete values for complexity of operations
primOpToCost :: PrimOp a -> Cost
primOpToCost op
  | isListOp op = listOpCost
  | otherwise   = simpleOpCost
  where
    listOpCost   = Cost 1000 1
    simpleOpCost = Cost 2 1

    isListOp = \case
      OpListMap    _ _ -> True
      OpListAt     _   -> True
      OpListAppend _   -> True
      OpListLength _   -> True
      OpListFoldr  _ _ -> True
      OpListFoldl  _ _ -> True
      OpListFilter _   -> True
      _                -> False

-}
