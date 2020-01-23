module Hschain.Utxo.Test.Client.Proc(
  runTestProc
) where

import Control.Concurrent

import Control.Immortal
import Control.Monad
import Control.Monad.IO.Class
import Control.Timeout

import Data.Ord

import Test.Hspec

import Hschain.Utxo.Back.App
import Hschain.Utxo.Back.Config
import Hschain.Utxo.Back.Env
import Hschain.Utxo.Test.Client.Monad(App, runTest, toHspec, TestSpec(..), initGenesis)

import qualified Hschain.Utxo.API.Client as C

data Options = Options
  { configPath  :: !FilePath
  , genesisPath :: !FilePath
  }
  deriving (Show)

defaultServiceOptions :: Options
defaultServiceOptions = Options
  { configPath  = "./config/config.yaml"
  , genesisPath = "./config/genesis.json"
  }

defaultTestSpec :: TestSpec
defaultTestSpec = TestSpec
  { testSpec'client  = C.ClientSpec "127.0.0.1" 8181 False
  , testSpec'verbose = False
  }

app :: Options -> Genesis -> IO Thread
app opt@Options{..} genesis = do
  cfg <- loadConfig configPath
  putStrLn $ mconcat ["Starts hschain-utxo server on port ", show $ serverConfig'port $ config'server cfg]
  appEnv <- initEnv genesis
  runApp appEnv cfg

runTestProc :: App () -> IO Spec
runTestProc testApp = do
  test <- runTest defaultTestSpec $ do
    genesis <- initGenesis
    serviceProc <- liftIO $ app defaultServiceOptions genesis
    wait
    testApp
    liftIO $ stop serviceProc
  return $ toHspec $ test
  where
    wait = sleep 0.25

