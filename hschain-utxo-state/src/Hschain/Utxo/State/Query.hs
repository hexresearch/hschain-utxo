-- | query useful stats from blockchain state
module Hschain.Utxo.State.Query(
  getBoxBalance
) where

import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types

import qualified Data.Map.Strict as M

-- | Get value contained in the box by its identifier.
getBoxBalance :: BoxChain -> BoxId -> Maybe Money
getBoxBalance BoxChain{..} boxId =
  fmap box'value $ M.lookup boxId boxChain'boxes

