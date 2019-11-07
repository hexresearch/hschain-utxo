module Hex.Common.Delay(
  sleep,
  waitForever
) where

import Control.Concurrent (newEmptyMVar, takeMVar, myThreadId)
import Control.Monad.IO.Class
import Foreign.StablePtr

import Data.Time
import Control.Concurrent.Thread.Delay

-- | Stop the thread for some time in seconds.
sleep :: MonadIO m => NominalDiffTime -> m ()
sleep dt = liftIO . delay $ toMicroseconds dt

-- | Convert time to microseconds
toMicroseconds :: NominalDiffTime -> Integer
toMicroseconds t = ceiling $ toRational t * 1000000

-- | Stop the thread forever
waitForever :: MonadIO m => m ()
--waitForever = liftIO $ takeMVar globalBlackvar
waitForever = liftIO $ do
  lock <- newEmptyMVar
  _ <- newStablePtr =<< myThreadId
  takeMVar lock
