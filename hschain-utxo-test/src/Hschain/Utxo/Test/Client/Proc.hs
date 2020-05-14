-- | Module defines main process that starts nodes and performs tests
-- triggers exchange of transaction and collects test stats.
module Hschain.Utxo.Test.Client.Proc(
  runTestProc
) where

import Hex.Common.Control
import Hex.Common.Aeson
import Hex.Common.Yaml

import Control.Concurrent

import Control.Monad
import Control.Monad.IO.Class
import Control.Timeout

import Data.Maybe
import Data.Ord
import Data.UUID

import System.Directory
import System.FilePath
import System.Random
import Test.Hspec

import HSChain.Logger

import Hschain.Utxo.Blockchain
import Hschain.Utxo.Back.App
import Hschain.Utxo.Back.Config
import Hschain.Utxo.Back.Env
import Hschain.Utxo.Lang.Sigma (newSecret)
import Hschain.Utxo.Test.Client.Monad(App, runTest, toHspec, TestSpec(..), initGenesis)

import qualified Data.List as L

import qualified Hschain.Utxo.API.Client as C

-- | Resources of children processes
data Resource = Resource
  { resource'threads :: [ThreadId]
  , resource'dbs     :: [FilePath]
  } deriving (Show)

clearResource :: Resource -> IO ()
clearResource Resource{..} = do
  mapM_ killThread resource'threads
  mapM_ clearDb resource'dbs

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
  , testSpec'verbose = False
  }

app :: Options -> Genesis -> IO Resource
app opt genesisTx = do
  valCfgs <-  mapM readValidatorConfig $ configValidatorPath opt
  nodeCfg <- readWebnodeConfig $ configWebnodePath opt
  if (isInMemoryTest nodeCfg valCfgs)
    then do
      putStrLn "In memory test"
      go valCfgs nodeCfg
    else withTestDir opt $ \tempDir ->
            go (fmap (substValidatorTempDir tempDir) valCfgs) (substWebnodeTempDir tempDir nodeCfg)
  where
    go valCfgs nodeCfg = do
      valThreads <- mapM (\cfg -> forkIO $ runValidator cfg genesisTx) valCfgs
      webThread <- forkIO $ runWebNode nodeCfg genesisTx
      return $ Resource
        { resource'threads = webThread : valThreads
        , resource'dbs     = catMaybes $ fmap nspec'dbName $ config'node nodeCfg : valCfgs
        }

    readValidatorConfig :: FilePath -> IO NodeSpec
    readValidatorConfig = readYaml

    readWebnodeConfig :: FilePath -> IO Config
    readWebnodeConfig = readYaml

    substValidatorTempDir = substNodeSpec
    substWebnodeTempDir dir cfg@Config{..} = cfg
      { config'node = substNodeSpec dir config'node
      }

    substNodeSpec dir spec@NodeSpec{..} = spec
      { nspec'dbName = fmap (substDir dir) nspec'dbName
      , nspec'logs   = substLog dir nspec'logs
      }

    substLog dir spec@LogSpec{..} = spec
      { logSpec'files = fmap (substScribeSpec dir) logSpec'files
      }

    substScribeSpec dir spec@ScribeSpec{..} = spec
      { scribe'path = fmap (substDir dir) scribe'path
      }

    substDir dir path
      | isAbsolute path = path
      | otherwise       = dir </> path
      where
        isAbsolute = L.isPrefixOf "/"

runTestProc :: App () -> IO Spec
runTestProc testApp = do
  masterSecret <- newSecret
  serviceResources <- app defaultServiceOptions $ initGenesis masterSecret
  wait
  test <- runTest defaultTestSpec masterSecret $ testApp
  clearResource serviceResources
  wait
  return $ toHspec $ test
  where
    wait = sleep 1

withTestDir :: Options -> (FilePath -> IO a) -> IO a
withTestDir Options{..} cont = do
  createDirectoryIfMissing True testDir
  withFixedDirectory testDir "test" cont
  where
    withFixedDirectory dir name cont = do
      let resDir = dir </> name
      putStrLn $ mconcat ["Alocate directory for tests: ", resDir]
      createDirectory resDir
      cont resDir

    withTempDirectory dir name cont = do
      tempDir <- getTempDirName name
      let resDir= dir </> tempDir
      putStrLn $ mconcat ["Alocate directory for tests: ", resDir]
      createDirectory resDir
      cont resDir
      where
        getTempDirName name = do
          suffix :: UUID <- randomIO
          return $ mconcat [name, "-", show suffix]

clearDb :: FilePath -> IO ()
clearDb path = removeFile path

isInMemoryTest :: Config -> [NodeSpec] -> Bool
isInMemoryTest webNode validators =
  all isInMemoryNode $ config'node webNode : validators

isInMemoryNode :: NodeSpec -> Bool
isInMemoryNode NodeSpec{..} =
     (isNothing nspec'dbName || nspec'dbName == Just ":memory:")
  && (L.null $ logSpec'files nspec'logs)

