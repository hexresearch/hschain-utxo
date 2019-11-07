module Hex.Common.Lock (
    Lock
  , newLock
  , closeLock
  , openLock
  , isLocked
  , lockFilter
) where

import Control.Monad.IO.Class

import Data.Bool
import Data.IORef

-- | Provides lock behaviour
newtype Lock = Lock { unLock :: IORef Bool }

newLock :: MonadIO m => m Lock
newLock = liftIO $ fmap Lock $ newIORef False

closeLock :: MonadIO m => Lock -> m ()
closeLock (Lock ref) = liftIO $ writeIORef ref True

openLock  :: MonadIO m => Lock -> m ()
openLock (Lock ref) = liftIO $ writeIORef ref False

isLocked :: MonadIO m => Lock -> m Bool
isLocked = liftIO . readIORef . unLock

lockFilter :: MonadIO m => Lock -> a -> m (Maybe a)
lockFilter ref a = do
  isLock <- isLocked ref
  return $ bool (Just a) Nothing isLock
