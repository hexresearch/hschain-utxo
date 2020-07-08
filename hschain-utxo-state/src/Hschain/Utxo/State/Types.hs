-- | Types for hschain-utxo state
module Hschain.Utxo.State.Types where

import Hex.Common.Aeson

import Codec.Serialise (Serialise)

import Data.Text (Text)
import Data.Map.Strict (Map)

import GHC.Generics

import Hschain.Utxo.Lang

import qualified Data.Map.Strict as M

-- | Blockchain is a set of Boxes.
-- each box contains value and script that protects value from spending.
data BoxChain = BoxChain
  { boxChain'boxes  :: !(Map BoxId Box)  -- ^ collection of boxes
  , boxChain'height :: !Integer          -- ^ height of blockchain
  } deriving (Show, Eq, Generic, Serialise)

-- | Empty initial blockchain state.
emptyBoxChain :: BoxChain
emptyBoxChain = BoxChain
  { boxChain'boxes  = M.empty
  , boxChain'height = 0
  }

-- | Tx referes to input boxes by identifiers and
-- contains functions to read blockchain environment.
--
-- This function substitutes references for real input boxes
-- and supplies environment for execution.
--
-- The value of type @TxArg@ is self-contained for execution.
toTxArg :: BoxChain -> Tx -> Either Text TxArg
toTxArg bch@BoxChain{..} Tx{..} = fmap (\inputs ->
  TxArg
    { txArg'outputs = tx'outputs
    , txArg'inputs  = inputs
    , txArg'args    = tx'args
    , txArg'proof   = tx'proof
    , txArg'env     = getEnv bch
    }
  ) mInputs
  where
    mInputs = mapM (\boxId -> maybe (noInputFor boxId) Right $ M.lookup boxId boxChain'boxes) tx'inputs
    noInputFor (BoxId idx) = Left $ mconcat ["Error: no box input with id: ", idx]

-- | Read blockchain environment.
getEnv :: BoxChain -> Env
getEnv BoxChain{..} = Env { env'height = boxChain'height }

$(deriveJSON dropPrefixOptions ''BoxChain)

