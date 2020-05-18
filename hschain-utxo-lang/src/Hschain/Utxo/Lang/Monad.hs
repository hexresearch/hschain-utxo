-- | Module defines main execution monad of the language.
module Hschain.Utxo.Lang.Monad(
    MonadLang
  , module X
) where

import Control.Monad.Except               as X
import Hschain.Utxo.Lang.Error            as X
import Hschain.Utxo.Lang.Desugar.FreshVar as X

-- | Monad for language execution and type-checking.
--
-- To execute language we have to be able to allocate fresh variable names
-- and report errors.
class (MonadFreshVar m, MonadError Error m) => MonadLang m where

