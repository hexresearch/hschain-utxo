-- | Common type definitions
module Hschain.Utxo.Lang.Core.Data.Utils(
    Name
  , Addr
) where

import Data.Text (Text)

type Name = Text
type Addr = Int

