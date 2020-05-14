module Hschain.Utxo.Back.App(
    runWebNode
  , runValidator
) where

import Hex.Common.Delay

import Control.Concurrent (newEmptyMVar, takeMVar, myThreadId, ThreadId, forkIO)
import Control.Monad
import Control.Monad.Cont
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

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.Wai.Handler.Warp as Warp

-- | Runs webnode.
runWebNode :: Config -> Genesis -> IO ()
runWebNode cfg@Config{..} genesis = flip runContT return $ do
  (env, acts) <- initEnv config'node genesis
  liftIO $ runConcurrently $ (greetNode config'node >> runApp env cfg)
                           : acts

-- Runs validator
runValidator :: NodeSpec -> Genesis -> IO ()
runValidator nspec genesis = flip runContT return $ do
  (_, acts) <- initEnv nspec genesis
  liftIO $ runConcurrently $ (greetNode nspec >> waitForever) : acts

-- | Echo print to show that node started.
greetNode :: NodeSpec -> IO ()
greetNode NodeSpec{..} = T.putStrLn $ mconcat
  [ "Starts ", logSpec'namespace nspec'logs , " on port ", T.pack nspec'port ]

-- | Create service application
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

-- | Run the application with Warp
runApp :: AppEnv -> Config -> IO ()
runApp env settings = do
  let httpServer
        = runSettings (warpSettings env $ config'server settings)
        $ serverApp env settings
  httpServer

