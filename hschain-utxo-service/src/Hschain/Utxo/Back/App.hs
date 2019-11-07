{-# LANGUAGE CPP #-}
module Hschain.Utxo.Back.App(
    runApp
) where

import Control.Concurrent (newEmptyMVar, takeMVar, myThreadId)
import Control.Monad
import Control.Monad.IO.Class
import Foreign.StablePtr

import Data.Proxy
import Data.String

import Network.Wai
import Network.Wai.Handler.Warp
import Servant.Server (serve, hoistServer)

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Back.Monad
import Hschain.Utxo.Back.Config
import Hschain.Utxo.Back.Server
import Hschain.Utxo.Back.Env

import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Immortal as Immortal

#if MIN_VERSION_immortal(0,3,0)
import Control.Monad.IO.Unlift
#else
import Control.Monad.Trans.Control
#endif

serverApp :: AppEnv -> Config -> Application
serverApp env config = do
  serve (Proxy :: Proxy UtxoAPI) utxoServerImpl
  where
    utxoServerImpl = hoistServer (Proxy :: Proxy UtxoAPI) (runServerM env) utxoServer

-- | Warp settings for the given ServerSettings and ServerSettings values.
warpSettings :: AppEnv -> ServerConfig -> Warp.Settings
warpSettings AppEnv {..} ServerConfig{..} =
    setPort serverConfig'port $
    setHost (fromString serverConfig'host) $
      defaultSettings
--    & setOnException (katipOnExceptionHandler env'katip)

runApp :: AppEnv -> Config -> IO ()
runApp env settings = do
  -- Run the application with Warp
  let httpServer
        = runSettings (warpSettings env $ config'server settings)
        $ serverApp env settings
  immortalProc "hschain-utxo-main-service" $ liftIO httpServer
  runBackgroundProcesses env
  waitForever

runBackgroundProcesses :: AppEnv -> IO ()
runBackgroundProcesses _ = return () -- no procs so far

-----------------------------------------------------------------------

-- | Stop the thread forever
waitForever :: MonadIO m => m ()
--waitForever = liftIO $ takeMVar globalBlackvar
waitForever = liftIO $ do
  lock <- newEmptyMVar
  _ <- newStablePtr =<< myThreadId
  takeMVar lock

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

