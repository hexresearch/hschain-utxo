module Hschain.Utxo.Blockchain.Bchain where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
-- import HSChain.Blockchain.Interpretation
import HSChain.Store
-- import HSChain.Run    (runDBT)
import qualified HSChain.Crypto as Crypto

import Hschain.Utxo.Lang hiding (Height)
import Hschain.Utxo.State
import Hschain.Utxo.State.Types
import Hschain.Utxo.Blockchain.Hschain

data Bchain m = Bchain {
    bchain'conn       :: Connection 'RO BData
  , bchain'cursor     :: MempoolCursor m (Alg BData) Tx
  , bchain'store      :: BChStore m BData
  , bchain'waitForTx  :: m (TxHash -> m Bool)
  }

hoistBchain :: Functor n => (forall a. m a -> n a) -> Bchain m -> Bchain n
hoistBchain f Bchain{..} = Bchain
  { bchain'cursor     = hoistMempoolCursor  f bchain'cursor
  , bchain'store      = hoistBChStore       f bchain'store
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
  liftIO $ fmap ((\(Crypto.Hashed (Crypto.Hash h)) -> TxHash h)) <$> pushTransaction bchain'cursor tx

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

