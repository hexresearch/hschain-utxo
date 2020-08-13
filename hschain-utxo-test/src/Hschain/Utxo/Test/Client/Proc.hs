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
  { configValidatorPath  :: ![FilePath]  -- ^ validator configs
  , configWebnodePath    :: !FilePath    -- ^ config for webnode
  , genesisPath          :: !FilePath    -- ^ path to genesis
  , testDir              :: !FilePath    -- ^ path to keep data for nodes
  }
  deriving (Show)

configDir :: FilePath -> FilePath
configDir = mappend "./config/"

nodeConfigDir :: FilePath -> FilePath
nodeConfigDir = configDir . mappend "main/"

poolConfigDir :: FilePath -> FilePath
poolConfigDir = configDir . mappend "pool/"


defaultServiceOptions :: Options
defaultServiceOptions = Options
  { configValidatorPath  = fmap nodeConfigDir
        [ "node-val1.yaml"
        , "node-val2.yaml"
        , "node-val3.yaml"
        , "node-val4.yaml"
        ]
  , configWebnodePath = nodeConfigDir "node-web.yaml"
  , genesisPath = poolConfigDir "genesis.json"
  , testDir = "./test-craddle"
  }

defaultTestSpec :: TestSpec
defaultTestSpec = TestSpec
  { testSpec'client  = C.ClientSpec "127.0.0.1" 8080 False
  , testSpec'verbose = True
  }

app :: Options -> Genesis -> IO [ThreadId]
app opt genesisTx = do
  valCfgs <- mapM readYaml $ configValidatorPath opt
  nodeCfg <- readYaml $ configWebnodePath opt
  valThreads <- mapM (\cfg -> forkIO $ runValidator cfg genesisTx) valCfgs
  webThread  <- forkIO $ runWebNode nodeCfg genesisTx
  return $ webThread : valThreads


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
