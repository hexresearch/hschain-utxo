-- | Defines type for low-level connection with hschain internals
module Hschain.Utxo.Blockchain.Bchain where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Coerce

import HSChain.Crypto.Classes.Hash
import HSChain.Mempool
import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Store

import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types
import Hschain.Utxo.Blockchain.Logic


-- | Connection to hschain internals.
-- Low level API to post transactions.
data Bchain m = Bchain
  { bchain'conn          :: Connection 'RO
  , bchain'cached        :: Cached BData
  , bchain'mempoolCursor :: MempoolCursor (Hashed (Alg BData) Tx) Tx
  , bchain'state         :: m BoxChain
  , bchain'waitForTx     :: m (TxHash -> m Bool)
  }

-- | Transform underlying monad for @Bchain@.
hoistBchain :: Functor n => (forall a. m a -> n a) -> Bchain m -> Bchain n
hoistBchain f Bchain{..} = Bchain
  { bchain'state      = f bchain'state
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
  liftIO $ fmap coerce <$> pushTxSync bchain'mempoolCursor tx

readBlock :: (MonadIO m, MonadReadDB m, MonadCached BData m) => Int -> m (Maybe [Tx])
readBlock height = do
  mb <- queryRO $ retrieveBlock $ Height $ fromIntegral height
  pure $ unBData . merkleValue . blockData <$> mb

blockchainSize :: (MonadIO m, MonadReadDB m, MonadCached BData m) => m Int
blockchainSize = do
  Height h <- queryRO blockchainHeight
  pure $! fromIntegral h

readBoxChainState :: (MonadBChain m) => m BoxChain
readBoxChainState = liftIO . bchain'state =<< askBchain

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

