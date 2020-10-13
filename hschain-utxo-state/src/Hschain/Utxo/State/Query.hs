-- | query useful stats from blockchain state
module Hschain.Utxo.State.Query(
    getBox
  , getBoxBalance
) where

import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types

import qualified Data.Map.Strict as M

-- | Get box by its identifier.
getBox :: BoxChain -> BoxId -> Maybe Box
getBox BoxChain{..} boxId =
  fmap postBox'content $ M.lookup boxId boxChain'boxes

-- | Get value contained in the box by its identifier.
getBoxBalance :: BoxChain -> BoxId -> Maybe Money
getBoxBalance BoxChain{..} boxId =
  fmap (box'value . postBox'content) $ M.lookup boxId boxChain'boxes

