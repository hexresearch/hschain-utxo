-- | Iterates over blocks and transactions and puts them to TChan.
module Hschain.Utxo.Test.Client.Chan(
    BlockChan
  , newBlockChan
  , stopBlockChan
  , getBlockTChan
  , findTx
  , findTxM
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Extra (findM)

import Data.Either
import Data.Maybe
import Data.Time

import Hex.Common.Control

import Hschain.Utxo.Lang
import Hschain.Utxo.API.Client

-- | Spawns eternal process that reads new blocks in sequence from current height.
data BlockChan = BlockChan
  { blockChan'proc    :: Async ()
  , blockChan'chan    :: TChan [Tx]
  , blockChan'height  :: TVar Int
  }

-- | Spawn block-reader that polls node for new blocks every so milliseconds.
newBlockChan :: ClientSpec -> NominalDiffTime -> Maybe Int -> IO BlockChan
newBlockChan clientSpec dtime mHeight = do
  tchan <- newTChanIO
  currentHeight <- maybe getCurrentHeight pure mHeight
  Right block <- call clientSpec $ readBlock currentHeight
  atomically $ mapM_ (writeTChan tchan) block
  heightVar <- newTVarIO currentHeight
  proc <- async $ periodic dtime $ do
    readNewBlocks tchan heightVar
  return $ BlockChan
    { blockChan'proc   = proc
    , blockChan'chan   = tchan
    , blockChan'height = heightVar
    }
  where
    readNewBlocks :: TChan [Tx] -> TVar Int -> IO ()
    readNewBlocks tchan heightVar = do
      prevHeight <- getPrevHeight heightVar
      currentHeight  <- getCurrentHeight
      when (prevHeight < currentHeight) $ do
        bs <- fetchBlocksFromTo (prevHeight + 1) currentHeight
        atomically $ do
          writeTVar heightVar currentHeight
          mapM_ (writeTChan tchan) bs

    fetchBlocksFromTo :: Int -> Int -> IO [[Tx]]
    fetchBlocksFromTo prevHeight currentHeight = fmap (fromRight []) $ call clientSpec $ fmap catMaybes $
      mapM readBlock [prevHeight .. currentHeight]

    getCurrentHeight = do
      Right currentHeight <- call clientSpec readBlockchainHeight
      return currentHeight

    getPrevHeight tvar = readTVarIO tvar

getBlockTChan :: BlockChan -> TChan [Tx]
getBlockTChan = blockChan'chan

stopBlockChan :: BlockChan -> IO ()
stopBlockChan BlockChan{..} = cancel blockChan'proc

findTx :: BlockChan -> (Tx -> Bool) -> Int -> IO (Maybe Tx)
findTx bch condition maxTries = findTxM bch (pure . condition) maxTries

findTxM :: BlockChan -> (Tx -> IO Bool) -> Int -> IO (Maybe Tx)
findTxM bch condition maxTries = go (getBlockTChan bch) 0
  where
    go tchan count
      | count == maxTries = do
          stopBlockChan bch
          return Nothing
      | otherwise         = do
          txs <- atomically $ readTChan tchan
          mTx <- findM condition txs
          case mTx of
            Nothing -> go tchan (count + 1)
            Just tx -> do
              stopBlockChan bch
              return $ Just tx

