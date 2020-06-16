module Hschain.Utxo.App(
  runApp
) where

import Hex.Common.Aeson
import Hex.Common.Yaml

import Control.Monad

import Data.Maybe

import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Text (Text)

import Servant.Server

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import qualified Hschain.Utxo.State.Query as S
import Hschain.Utxo.State.Types

import Hschain.Utxo.API.Rest

-- | Server implementation for 'UtxoAPI'
utxoServer :: ServerT UtxoAPI ServerM
utxoServer =
       postTxEndpoint
  :<|> getBoxBalanceEndpoint
  :<|> getTxSigmaEndpoint
  :<|> getEnvEndpoint
  :<|> getStateEndpoint

postTxEndpoint :: Tx -> ServerM PostTxResponse
postTxEndpoint tx = fmap PostTxResponse $ postTxWait tx

getBoxBalanceEndpoint :: BoxId -> ServerM (Maybe Money)
getBoxBalanceEndpoint boxId =
  fmap (\bch -> S.getBoxBalance bch boxId) readBoxChain

getTxSigmaEndpoint :: Tx -> ServerM SigmaTxResponse
getTxSigmaEndpoint tx =
  fmap (\bch -> uncurry SigmaTxResponse $ execInBoxChain tx bch) readBoxChain

getEnvEndpoint :: ServerM GetEnvResponse
getEnvEndpoint = do
  bch <- readBoxChain
  return $ GetEnvResponse $ getEnv bch

getStateEndpoint :: ServerM BoxChain
getStateEndpoint = readBoxChain

-- |Application environment.
data AppEnv

-- | Server monad that holds internal environment
newtype ServerM a = ServerM { unServerM :: ReaderT AppEnv Handler a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadReader AppEnv
           , MonadThrow, MonadCatch)

newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT AppEnv Handler) a }

-- | Execution of 'ServerM'
runServerM :: AppEnv -> ServerM a -> Handler a
runServerM e = flip runReaderT e . unServerM

-- | Execution of 'ServerM' in IO monad
runServerMIO :: AppEnv -> ServerM a -> IO a
runServerMIO env m = do
  ea <- runHandler $ runServerM env m
  case ea of
    Left e -> fail $ "runServerMIO: " <> show e
    Right a -> return a

-- | Reads current state of the block chain
readBoxChain :: ServerM BoxChain
readBoxChain = readBoxChainState

{-

import Hschain.Utxo.Lang
import Hschain.Utxo.Back.App
import Hschain.Utxo.App.Options

runApp :: IO ()
runApp = app =<< readOptions

app :: Options -> IO ()
app = \case
  RunWebNode{..}    -> runBy runWebNode   runWebNode'config   runWebNode'genesis
  RunValidator{..}  -> runBy runValidator runValidator'config runValidator'genesis
  where
    runBy :: (FromJSON a, Show a) => (a -> [Tx] -> IO ()) -> FilePath -> FilePath -> IO ()
    runBy cmd configPath genesisPath =
      join $ cmd <$> readYaml configPath <*> readGenesis genesisPath

    readGenesis :: FilePath -> IO [Tx]
    readGenesis = fmap (fromMaybe err) . readJson
      where
        err = error "Error: failed to read genesis"

-}
