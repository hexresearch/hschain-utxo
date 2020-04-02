module Hschain.Utxo.Lang.Monad(
    MonadLang(..)
  , module X
) where

import Control.Monad.Except               as X
import Hschain.Utxo.Lang.Error            as X
import Hschain.Utxo.Lang.Desugar.FreshVar as X

class (MonadFreshVar m, MonadError Error m) => MonadLang m where

