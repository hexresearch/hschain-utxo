-- | Module defines main process that starts nodes and performs tests
-- triggers exchange of transaction and collects test stats.
module Hschain.Utxo.Test.Client.Proc(
  runTestProc
) where

import Hex.Common.Yaml

import Control.Concurrent
import Control.Timeout
import Test.Hspec

import Hschain.Utxo.Back.App
import Hschain.Utxo.Back.Config
import Hschain.Utxo.Lang.Sigma (newSecret)
import Hschain.Utxo.Test.Client.Monad(App, runTest, toHspec, TestSpec(..), initGenesis)

import qualified Hschain.Utxo.API.Client as C


-- | configs for tests
data Options = Options
  { configWebnodePath :: !FilePath    -- ^ config for webnode
  }
  deriving (Show)

defaultServiceOptions :: Options
defaultServiceOptions = Options
  { configWebnodePath = "config/main/node-web.yaml"
  }

defaultTestSpec :: TestSpec
defaultTestSpec = TestSpec
  { testSpec'client  = C.ClientSpec "127.0.0.1" 8080 False
  , testSpec'verbose = True
  }

app :: Options -> Genesis -> IO [ThreadId]
app opt genesisTx = do
  _nodeCfg  <- readYaml $ configWebnodePath opt :: IO Config
  webThread <- forkIO $ runWebNode nodeCfg genesisTx
  return [webThread]


runTestProc :: App () -> IO Spec
runTestProc testApp = do
  masterSecret <- newSecret
  let (genesis, masterBoxId) = initGenesis masterSecret
  tids <- app defaultServiceOptions genesis
  wait
  test <- runTest defaultTestSpec masterSecret masterBoxId $ testApp
  mapM_ killThread tids
  wait
  return $ toHspec $ test
  where
    wait = sleep 1
