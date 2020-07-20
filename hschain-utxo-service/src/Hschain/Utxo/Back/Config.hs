-- | Config for hschain-utxo node
module Hschain.Utxo.Back.Config(
    Genesis
  , UtxoSettings(..)
  , LogSpec(..)
  , Config(..)
  , ServerConfig(..)
  , loadConfig
  , loadGenesis
) where

import Control.Exception
import Control.Monad.IO.Class

import Data.Aeson
import Data.Aeson.TH
import Data.Yaml.Config            (loadYamlSettings, useEnv)

import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Blockchain.Net

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB

-- | Config of the node.
data UtxoSettings = UtxoSettings
  { utxo'web         :: ServerConfig
  , utxo'blockchain  :: NodeSpec
  }

-- | Genesis of hschain-utxo
type Genesis = [Tx]

-- | Config of the service
data Config = Config
  { config'server :: ServerConfig
  , config'node   :: NodeSpec
  } deriving (Show)

-- | Network-side config
data ServerConfig = ServerConfig
  { serverConfig'host   :: !String  -- ^ service port
  , serverConfig'port   :: !Int     -- ^ service host
  } deriving (Show, Eq)

-- | Load config from file
loadConfig :: MonadIO m => FilePath -> m Config
loadConfig path = liftIO $ loadYamlSettings [path] [] useEnv

loadGenesis :: FilePath -> IO (Maybe Genesis)
loadGenesis = readJson

readJson :: FromJSON a => FilePath -> IO (Maybe a)
readJson = fmap (decode' . LB.fromStrict =<<) . readStrictByteStringSafe

readStrictByteStringSafe :: FilePath -> IO (Maybe BS.ByteString)
readStrictByteStringSafe file =
  catchIOError (fmap Just $ BS.readFile file) (const $ return Nothing)

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = Control.Exception.catch

-------------------------------
-- JSON instances
--

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (length ("serverConfig'" :: String))} ''ServerConfig)
$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (length ("config'" :: String))} ''Config)



