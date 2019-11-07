{-# LANGUAGE CPP #-}
module Hex.Common.Immortal(
  immortalProc
) where

import Control.Monad
import Control.Monad.IO.Class
#if MIN_VERSION_immortal(0,3,0)
import Control.Monad.IO.Unlift
#else
import Control.Monad.Trans.Control
#endif

import qualified Control.Immortal as Immortal

#if MIN_VERSION_immortal(0,3,0)
immortalProc :: (MonadIO m, MonadUnliftIO m) => String -> m () -> m ()
#else
immortalProc :: (MonadIO m, MonadBaseControl IO m) => String -> m () -> m ()
#endif
immortalProc label proc = void . Immortal.createWithLabel label $ const $ Immortal.onFinish echoExit proc
  where
    echoExit e = liftIO $ do
      putStrLn $ mconcat ["Process ", label, " exits with:"]
      print e
