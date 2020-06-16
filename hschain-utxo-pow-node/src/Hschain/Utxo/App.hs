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

import HSChain.Crypto.Classes
import qualified HSChain.Crypto.Classes.Hash as Crypto

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


-- | Connection to hschain internals.
-- Low level API to post transactions.
data Bchain m = Bchain {
    bchain'conn       :: Connection 'RO BData
  , bchain'mempool    :: Mempool m (Alg BData) Tx
  , bchain'store      :: BChStore m BData
  , bchain'waitForTx  :: m (TxHash -> m Bool)
  }

-- | Transform underlying monad for @Bchain@.
hoistBchain :: Functor n => (forall a. m a -> n a) -> Bchain m -> Bchain n
hoistBchain f Bchain{..} = Bchain
  { bchain'mempool    = hoistMempool f bchain'mempool
  , bchain'store      = hoistDict    f bchain'store
  , bchain'waitForTx  = fmap f <$> f bchain'waitForTx
  , ..
  }


class MonadIO m => MonadBChain m where
  askBchain :: m (Bchain IO)

--------------------------------------------------
------ bchain store operations

writeTx :: (MonadBChain m) => Tx -> m (Maybe TxHash)
writeTx tx = do
  Bchain{..} <- askBchain
  liftIO $ fmap ((\(Crypto.Hashed (Crypto.Hash h)) -> TxHash h)) <$>
    ((\cursor -> pushTransaction cursor tx) =<< getMempoolCursor bchain'mempool)

readBlock :: (MonadIO m, MonadBChain m) => Int -> m (Maybe [Tx])
readBlock height = do
  Bchain{..} <- askBchain
  liftIO $ do
    mb <- runDBT bchain'conn $ queryRO $ retrieveBlock (Height $ fromIntegral height)
    pure $ unBData . merkleValue . blockData <$> mb

blockchainSize :: (MonadIO m, MonadBChain m) => m Int
blockchainSize = do
  Bchain{..} <- askBchain
  liftIO $ do
    Height h <- runDBT bchain'conn $ queryRO blockchainHeight
    pure $! fromIntegral h

readBoxChainState :: (MonadBChain m) => m BoxChain
readBoxChainState = do
  Bchain{..} <- askBchain
  liftIO $ merkleValue . snd <$> bchCurrentState bchain'store

waitForTx :: (MonadBChain m) => m (TxHash -> m Bool)
waitForTx = do
  Bchain{..} <- askBchain
  fmap liftIO <$> liftIO bchain'waitForTx

postTxWait :: (MonadBChain m) => Tx -> m (Maybe TxHash)
postTxWait tx = do
  -- We start listening before sending transaction to mempool to avoid
  -- race when tx is commited before we start listening
  listener <- waitForTx
  runMaybeT $ do
    h <- MaybeT $ writeTx tx
    guard =<< lift (listener h)
    -- lift $ incMetricSurelyPostedTx tx
    return h


