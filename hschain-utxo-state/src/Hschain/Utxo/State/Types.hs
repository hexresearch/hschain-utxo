module Hschain.Utxo.State.Types where

import Hex.Common.Aeson

import Control.Monad
import Data.Map.Strict (Map)

import Hschain.Utxo.Lang

import qualified Data.Map.Strict as M

data BoxChain = BoxChain
  { boxChain'boxes  :: !(Map BoxId Box)
  , boxChain'height :: !Integer
  } deriving (Show, Eq)

emptyBoxChain :: BoxChain
emptyBoxChain = BoxChain
  { boxChain'boxes  = M.empty
  , boxChain'height = 0
  }

toTxArg :: BoxChain -> Tx -> Maybe TxArg
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
    mInputs = mapM (\boxId -> M.lookup boxId boxChain'boxes) tx'inputs

getEnv :: BoxChain -> Env
getEnv BoxChain{..} = Env { env'height = boxChain'height }

$(deriveJSON dropPrefixOptions ''BoxChain)

