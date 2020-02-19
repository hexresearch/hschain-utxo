{-# LANGUAGE CPP #-}
module Hschain.Utxo.Back.App(
    runWebNode
  , runValidator
) where

import Hex.Common.Delay
import Hex.Common.Immortal

import Control.Concurrent (newEmptyMVar, takeMVar, myThreadId)
import Control.Immortal
import Control.Monad
import Control.Monad.IO.Class
import Foreign.StablePtr

import Data.Proxy
import Data.String

import Network.Wai
import Network.Wai.Handler.Warp
import Servant.Server (serve, hoistServer)

import HSChain.Control

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Blockchain
import Hschain.Utxo.Back.Monad
import Hschain.Utxo.Back.Config
import Hschain.Utxo.Back.Server
import Hschain.Utxo.Back.Env
import Hschain.Utxo.Lang

import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Immortal as Immortal

runWebNode :: Config -> [Tx] -> IO ()
runWebNode cfg@Config{..} genesis = do
  (env, acts) <- initEnv config'node genesis
  runConcurrently $ (void (runApp env cfg) >> waitForever)
                  : acts

runValidator :: NodeSpec -> [Tx] -> IO ()
runValidator nspec genesis = do
  (_, acts) <- initEnv nspec genesis
  runConcurrently $ waitForever : acts

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

runApp :: AppEnv -> Config -> IO Thread
runApp env settings = do
  -- Run the application with Warp
  let httpServer
        = runSettings (warpSettings env $ config'server settings)
        $ serverApp env settings
  (pid :: Thread) <- immortalProc' "hschain-utxo-main-service" $ liftIO httpServer
  runBackgroundProcesses env
  return pid

runBackgroundProcesses :: AppEnv -> IO ()
runBackgroundProcesses _ = return () -- no procs so far

