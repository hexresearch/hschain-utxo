module Hschain.Utxo.Test.Client.Proc(
  runTestProc
) where

import Hex.Common.Control
import Hex.Common.Aeson
import Hex.Common.Yaml

import Control.Concurrent

import Control.Immortal
import Control.Monad
import Control.Monad.IO.Class
import Control.Timeout

import Data.Maybe
import Data.Ord

import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Hspec

import HSChain.Logger

import Hschain.Utxo.Blockchain
import Hschain.Utxo.Back.App
import Hschain.Utxo.Back.Config
import Hschain.Utxo.Back.Env
import Hschain.Utxo.Test.Client.Monad(App, runTest, toHspec, TestSpec(..), initGenesis)

import qualified Hschain.Utxo.API.Client as C

data Options = Options
  { configValidatorPath  :: ![FilePath]
  , configWebnodePath    :: !FilePath
  , genesisPath          :: !FilePath
  , testDir              :: !FilePath
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
  { testSpec'client  = C.ClientSpec "127.0.0.1" 8181 False
  , testSpec'verbose = False
  }

app :: Options -> Genesis -> IO [ThreadId]
app opt genesisTx = setupTest opt $ \tempDir -> do
  valCfgs <- fmap2 (substValidatorTempDir tempDir) $ mapM readValidatorConfig $ configValidatorPath opt
  nodeCfg <- fmap (substWebnodeTempDir tempDir) $ readWebnodeConfig $ configWebnodePath opt
  valThreads <- mapM (\cfg -> forkIO $ runValidator cfg genesisTx) valCfgs
  webThread <- forkIO $ runWebNode nodeCfg genesisTx
  return $ webThread : valThreads
  where
    readValidatorConfig :: FilePath -> IO NodeSpec
    readValidatorConfig = readYaml

    readWebnodeConfig :: FilePath -> IO Config
    readWebnodeConfig = readYaml

    substValidatorTempDir = substNodeSpec
    substWebnodeTempDir dir cfg@Config{..} = cfg
      { config'node = substNodeSpec dir config'node
      }

    substNodeSpec dir spec@NodeSpec{..} = spec
      { nspec'dbName = dir <.> nspec'dbName
      , nspec'logs   = substLog dir nspec'logs
      }

    substLog dir spec@LogSpec{..} = spec
      { logSpec'files = fmap (substScribeSpec dir) logSpec'files
      }

    substScribeSpec dir spec@ScribeSpec{..} = spec
      { scribe'path = fmap (dir <.> ) scribe'path
      }

runTestProc :: App () -> IO Spec
runTestProc testApp = do
  test <- runTest defaultTestSpec $ do
    genesis <- initGenesis
    serviceProcs <- liftIO $ app defaultServiceOptions genesis
    wait
    testApp
    liftIO $ mapM_ killThread serviceProcs
  return $ toHspec $ test
  where
    wait = sleep 0.25

setupTest :: Options -> (FilePath -> IO a) -> IO a
setupTest Options{..} cont = do
  createDirectoryIfMissing True testDir
  withTempDirectory testDir "test" cont


