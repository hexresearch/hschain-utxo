-- |
-- Install handler for SIGTERM which converts it to UserInterrupt sent
-- to main thread
module Hex.Common.Server.Sigterm where

import Control.Concurrent
import Control.Exception
import System.Posix.Signals

-- | Converts SIGTERM to UserInterrupt exception and send it to thread
--   in which handler was called. Naturally this functions is expected
--   to be called in main.
installSigtermHandler :: IO ()
installSigtermHandler = do
  tid <- myThreadId
  _   <- installHandler sigTERM (CatchOnce (throwTo tid UserInterrupt)) Nothing
  return ()

