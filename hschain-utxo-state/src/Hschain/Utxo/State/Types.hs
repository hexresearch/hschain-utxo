module Hschain.Utxo.State.Types where

import Hex.Common.Aeson

import Codec.Serialise (Serialise)
import Control.Monad

import Data.Text (Text)
import Data.Map.Strict (Map)

import GHC.Generics

import Hschain.Utxo.Lang

import qualified Data.Map.Strict as M

data BoxChain = BoxChain
  { boxChain'boxes  :: !(Map BoxId Box)
  , boxChain'height :: !Integer
  } deriving (Show, Eq, Generic, Serialise)

emptyBoxChain :: BoxChain
emptyBoxChain = BoxChain
  { boxChain'boxes  = M.empty
  , boxChain'height = 0
  }

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

getEnv :: BoxChain -> Env
getEnv BoxChain{..} = Env { env'height = boxChain'height }

$(deriveJSON dropPrefixOptions ''BoxChain)

