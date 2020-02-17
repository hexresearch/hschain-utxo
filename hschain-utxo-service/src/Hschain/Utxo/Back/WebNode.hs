module Hschain.Utxo.Back.WebNode(
  webNodeMain
) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Concurrent.Chan as C
import Data.Maybe
import Network.Wai.Handler.Warp as Warp

import Hex.Common.Delay
import Hex.Common.Server.Sigterm
import Hschain.Utxo.Blockchain
import Hschain.Utxo.Lang
import Hschain.Utxo.Back.Config
import Hschain.Utxo.Back.Env

runHuralM :: KatipEnv -> AppM () -> IO ()
runHuralM = undefined
newEnv = undefined
utxoApp = undefined

data AppM a = AppM a

webNodeMain :: UtxoSettings -> [Tx] -> IO ()
webNodeMain UtxoSettings{..} genesisTx = do
  installSigtermHandler

  let monitoringPort = fromMaybe (read (nspec'port utxo'blockchain) + 1000)
                                  (nspec'monitoringPort utxo'blockchain)
  putStrLn $ mconcat ["Start node monitoring on port: ", show monitoringPort]
  -- startAppMonitoring monitoringPort

  katipEnv <- newKatipEnv utxo'logs "hschain-utxo-web"
  hschainChan <- C.newChan

  runHuralM katipEnv $ do

    interpretSpecWithCallback (liftIO . writeChan hschainChan) utxo'blockchain genesisTx $ \bchain' -> do
      let bchain = hoistBchain (runHuralM katipEnv) bchain'

      env <- liftIO $ runStdoutLoggingT $ newEnv utxo'web hschainChan bchain
      let listenPort = serverConfig'port utxo'web
      liftIO $ putStrLn $ "Started listening on 127.0.0.1:" ++ show listenPort
      liftIO $ Warp.run listenPort $ utxoApp env

      waitForever


