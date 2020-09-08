-- | This module defines client for API to hschain-utxo node
module Hschain.Utxo.API.Client where

import Hex.Common.Text
import Hex.Common.Url

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import Data.Bool
import Data.Int
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Servant.API

import qualified Data.Text as T
import qualified Servant.Client as C

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types hiding (getEnv)

-- | Errors in 'ClientM' monad
data ClientError
  = InvalidBaseUrl !Text
  | ClientError !C.ClientError
  deriving (Eq, Show, Generic)

instance Exception ClientError

-- | Tighten an error info into servant client common client error type
fromClientError :: ClientError -> C.ClientError
fromClientError e@(InvalidBaseUrl{}) = C.ConnectionError (toException e)
fromClientError (ClientError e) = e

-- | Client specification to connect with hschain-utxo node
data ClientSpec = ClientSpec
  { clientSpec'host     :: !Text  -- ^ Node host
  , clientSpec'port     :: !Int   -- ^ Node port
  , clientSpec'https    :: !Bool  -- ^ Use https
  } deriving (Show, Eq)

-- | Helper to call the xenochain API-methods
call :: MonadIO m => ClientSpec -> ClientM a -> m (Either Text a)
call ClientSpec{..} method = fmap toResp $ runClientM method =<< makeClientConfig ops
  where
    ops = HttpQueryOpsUrl url

    url = (bool toUrl toUrlHttps clientSpec'https) clientSpec'host clientSpec'port

    toResp x = case x of
      Left err  -> Left $ showt err
      Right val -> Right val

-- | Initial environment that is needed to execute 'ClientM' action
data ClientConfig = ClientConfig {
    clientUrl      :: !Text -- ^ Base url for API server
  , clientManager  :: !Manager -- ^ Connection manager
  } deriving (Generic)

-- | Monad that is used to perform calls to NEM NIS API.
newtype ClientM a = ClientM { unClientM :: ReaderT ClientConfig (ExceptT ClientError C.ClientM) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, Generic
    , MonadError ClientError, MonadReader ClientConfig )

-- | Execute API actions
runClientM :: MonadIO m
  => ClientM a -- ^ Action
  -> ClientConfig -- ^ Required info to execute action
  -> m (Either ClientError a)
runClientM ma cfg@ClientConfig{..} = case C.parseBaseUrl . T.unpack $ clientUrl of
  Left e -> return $ Left . InvalidBaseUrl . T.pack . show $ e
  Right burl -> do
    res <- liftIO $ C.runClientM (runExceptT $ runReaderT (unClientM ma) cfg) $ C.ClientEnv clientManager burl Nothing
    return $ join . first ClientError $ res

-- | Helper to embed 'ClientM' action into more specific monad
liftClientM :: C.ClientM a -> ClientM a
liftClientM = ClientM . lift . lift

postTx :: Tx -> ClientM PostTxResponse
postTx tx = liftClientM (postTx' tx)

getBox :: BoxId -> ClientM (Maybe Box)
getBox boxId = liftClientM (getBox' boxId)

getBoxBalance :: BoxId -> ClientM (Maybe Money)
getBoxBalance boxId = liftClientM (getBoxBalance' boxId)

getTxSigma :: Tx -> ClientM SigmaTxResponse
getTxSigma txHash = liftClientM (getTxSigma' txHash)

getEnv :: ClientM GetEnvResponse
getEnv = liftClientM getEnv'

getHeight :: ClientM Int64
getHeight = fmap (\(GetEnvResponse env) -> env'height env) getEnv

getState :: ClientM BoxChain
getState = liftClientM getState'

getUtxos :: ClientM [BoxId]
getUtxos = liftClientM getUtxos'

hasUtxo :: BoxId -> ClientM Bool
hasUtxo boxId = liftClientM (hasUtxo' boxId)

readBlock :: Int -> ClientM (Maybe [Tx])
readBlock n = liftClientM (readBlock' n)

readBlockchainHeight :: ClientM Int
readBlockchainHeight = liftClientM readBlockchainHeight'

-- Client auto implementation
--
postTx' :: Tx -> C.ClientM PostTxResponse
getBox' :: BoxId -> C.ClientM (Maybe Box)
getBoxBalance' :: BoxId -> C.ClientM (Maybe Money)
getTxSigma' :: Tx -> C.ClientM SigmaTxResponse
getEnv' :: C.ClientM GetEnvResponse
getState' :: C.ClientM BoxChain
getUtxos' :: C.ClientM [BoxId]
hasUtxo' :: BoxId -> C.ClientM Bool
readBlock' :: Int -> C.ClientM (Maybe [Tx])
readBlockchainHeight' :: C.ClientM Int
(      postTx'
  :<|> getBox'
  :<|> getBoxBalance'
  :<|> getTxSigma'
  :<|> getEnv'
  :<|> getState'
  :<|> getUtxos'
  :<|> hasUtxo'
  :<|> readBlock'
  :<|> readBlockchainHeight'
  ) = C.client (Proxy :: Proxy UtxoAPI)

data HttpQueryOps =
  HttpQueryOps {
    httpHost :: Text
  , httpPort :: Int
  }
  | HttpQueryOpsUrl {
    httpUrl :: Text
  }
  deriving (Show, Eq)

makeClientManager' :: (MonadIO m) => m Manager
makeClientManager' = liftIO $ newManager tlsManagerSettings
  { managerResponseTimeout = responseTimeoutMicro $ 180 * 1e6 -- three minutes
  }

makeClientConfig :: (MonadIO m) => HttpQueryOps -> m ClientConfig
makeClientConfig ops = do
  mng <- makeClientManager'
  pure $ ClientConfig
      { clientUrl = url
      , clientManager  = mng
      }
  where
    url = case ops of
            HttpQueryOps    {..} -> mconcat ["http://", httpHost, ":", T.pack . show $ httpPort]
            HttpQueryOpsUrl {..} -> httpUrl

query :: ClientConfig -> ClientM a -> IO (Either ClientError a)
query = flip runClientM

