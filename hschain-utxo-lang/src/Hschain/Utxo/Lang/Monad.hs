-- | Module defines main execution monad of the language.
module Hschain.Utxo.Lang.Monad(
    MonadLang
  , mapBinds
  , liftToModule
  , liftToModuleWithCtx
  , module X
) where

import Control.Monad.Except               as X
import Hschain.Utxo.Lang.Error            as X
import Hschain.Utxo.Lang.Desugar.FreshVar as X

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Module
import Hschain.Utxo.Lang.UserType

-- | Monad for language execution and type-checking.
--
-- To execute language we have to be able to allocate fresh variable names
-- and report errors.
class (MonadFreshVar m, MonadError Error m) => MonadLang m where

mapBinds :: (Lang -> Lang) -> Module -> Module
mapBinds f m = m { module'binds = fmap f $ module'binds m }

liftToModule :: MonadLang m => (Lang -> m Lang) -> Module -> m Module
liftToModule f m = do
  binds <- mapM f $ module'binds m
  return $ m { module'binds = binds }

liftToModuleWithCtx :: MonadLang m => (UserTypeCtx -> Lang -> m Lang) -> Module -> m Module
liftToModuleWithCtx f m = do
  binds <- mapM (f $ module'userTypes m) $ module'binds m
  return $ m { module'binds = binds }

