-- | Module defines main execution monad of the language.
module Hschain.Utxo.Lang.Monad(
    MonadLang
  , wrongPatPrimMixture
  , wrongPatConsMixture
  , module X
) where

import Control.Monad.Except               as X
import Hschain.Utxo.Lang.Error            as X
import Hschain.Utxo.Lang.Desugar.FreshVar as X

import Hschain.Utxo.Lang.Expr (Loc)

-- | Monad for language execution and type-checking.
--
-- To execute language we have to be able to allocate fresh variable names
-- and report errors.
class (MonadFreshVar m, MonadError Error m) => MonadLang m where

-- errors:

wrongPatPrimMixture :: MonadLang m => Loc -> m a
wrongPatPrimMixture loc = throwError $ PatError $ WrongPatPrimMixture loc

wrongPatConsMixture :: MonadLang m => Loc -> m a
wrongPatConsMixture loc = throwError $ PatError $ WrongPatConsMixture loc

