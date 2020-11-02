{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
-- |
module TM.BCH.Util
  ( Test
  , Blk
    -- * Mining utils
  , Mine
  , runMiner
  , mineBlock
  , badBlock
  , badTx
  , mineBlockE
    -- * Transactions
  , simpleInputRef
  , simpleScript
  , burnBox
  ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Cont

import Data.Fix
import qualified Data.Vector as V
import System.Timeout

import Test.Tasty.HUnit
import Prelude hiding ((<*))

import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.Types.Merkle.Types
import HSChain.PoW.Types hiding (Tx)
import HSChain.PoW.Tests
import HSChain.PoW.BlockIndex
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import qualified HSChain.POW            as POW
import HSChain.Network.Types
import HSChain.Network.Mock
import HSChain.PoW.Consensus
import HSChain.Store.Query

import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Pow.App.Types
import qualified Hschain.Utxo.Lang.Sigma as Sigma


----------------------------------------------------------------
-- Parameters for test blockchain
----------------------------------------------------------------

-- | Work function parameters for testing. We disable work checks in
--   order to be able to create new block as fast as we want
data Test

-- | Block type used in tests
type Blk = UTXOBlock Test

instance UtxoPOWCongig Test where
  powConfig      _ = POW.defaultPOWConfig
  checkBlockWork _ = False

genesis :: Block (UTXOBlock Test)
genesis = Block
  { blockHeight = Height 0
  , blockTime   = Time   0
  , prevBlock   = Nothing
  , blockData   = UTXOBlock
    { ubNonce  = ""
    , ubData   = merkled []
    , ubTarget = Target $ 2^(256::Int) - 1
    }
  }


----------------------------------------------------------------
-- Helpers for running test sequences with blockchain engine
----------------------------------------------------------------

-- | Monad for manual creation of blocks. It tracks blockchain head
-- and able to create new block on request.
newtype Mine a = Mine (ReaderT ( Src (BH Blk, StateView (HSChainT IO) Blk)
                               , PoW (HSChainT IO) Blk
                               , BlockDB (HSChainT IO) Blk
                               )
                        (StateT (BH (UTXOBlock Test))
                          (HSChainT IO))
                        a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail)

-- | Execute program
runMiner :: Mine a -> IO a
runMiner (Mine action) = withHSChainT $ evalContT $ do
  -- Set up blockchain node
  mock <- liftIO newMockNet
  (db, bIdx, sView) <- lift $ utxoStateView genesis
  cns <- lift $ createConsensus db sView bIdx
  pow <- startNode netcfg (createMockNode mock (NetAddrV4 1 1000)) [] db cns
  -- Start node and
  lift $ do
    upd <- atomicallyIO $ chainUpdate pow
    let Just bh = lookupIdx (blockID genesis) bIdx
    flip evalStateT bh
      $ runReaderT action (upd, pow, db)
  where
    netcfg = NetCfg { nKnownPeers     = 3
                    , nConnectedPeers = 3
                    }


-- | Create block with coinbase spendable by given public key. If
--   @Nothing@ is provided coinbase will be spendable by anyone. If
--   block is rejected exception is thrown.
mineBlock :: Maybe Sigma.PublicKey -> [Tx] -> Mine BoxId
mineBlock mpk txs = mineBlockE mpk Nothing txs >>= \case
  Left  e -> liftIO $ throwM e
  Right a -> return a

-- | Same as 'mineBlock' but block must be rejected instead
badBlock :: [Tx] -> Mine ()
badBlock txs = mineBlockE Nothing Nothing txs >>= \case
  Left  _ -> return ()
  Right _ -> error "Block should be rejected"

badTx :: Sigma.ProofEnv -> GTx (Sigma.Sigma Sigma.PublicKey) Box -> Mine ()
badTx env tx = do
  tx' <- newProofTx env tx
  badBlock [tx']

-- | Same as 'mineBlock' but doesn't throw exception when block is rejected.
mineBlockE :: Maybe Sigma.PublicKey -> Maybe Money -> [Tx] -> Mine (Either SomeException BoxId)
mineBlockE mpk mFee txs = Mine $ do
  bh            <- get
  (upd,pow,_db) <- ask
  -- Compute fee
  fee <- case mFee of
    Just fee -> pure fee
    Nothing  -> do Right txArgs <- lift . lift
                                 $ queryRO . runExceptT
                                 $ forM txs
                                 $ buildTxArg (getDatabaseBox NoChange) (Env 0)
                   pure $! sumOf (each . to sumTxInputs)  txArgs
                         - sumOf (each . to sumTxOutputs) txArgs
  -- Make coinbase transaction
  let bid = bhBID bh
      coinbaseBox = Box { box'value  = miningRewardAmount + fee
                        , box'script = coreProgToScript $ case mpk of
                            Nothing -> EPrim $ PrimBool True
                            Just pk -> EPrim $ PrimSigma $ Fix $ Sigma.SigmaPk pk
                        , box'args   = mempty
                        }
      coinbase = Tx { tx'inputs  = V.singleton BoxInputRef
                      { boxInputRef'id      = BoxId $ let UB'BID h = bid in h
                      , boxInputRef'args    = mempty
                      , boxInputRef'proof   = Nothing
                      , boxInputRef'sigs    = V.empty
                      , boxInputRef'sigMask = SigAll
                      }
                    , tx'outputs = V.fromList [coinbaseBox]
                    }
      txId  = computeTxId coinbase
      boxId = computeBoxId txId 0
  -- Send block
  res <- lift $ lift $ sendNewBlock pow Block
    { blockHeight = succ $ bhHeight bh
    , blockTime   = let Height h = bhHeight bh
                    in Time (fromIntegral h * 1000)
    , prevBlock   = Just bid
    , blockData   = UTXOBlock
      { ubData   = merkled $ coinbase : txs
      , ubNonce  = ""
      , ubTarget = Target $ 2^(256::Int) - 1
      }
    }
  case res of
    Left  e  -> pure $ Left e
    Right () -> do
      liftIO (timeout 1e6 (atomically $ await upd)) >>= \case
        Nothing      -> error "Blockchain timed out"
        Just (bh',_) -> do
          -- Check total balance of live UTXO
          r <- queryRO $ basicQuery_
            "SELECT box \
            \  FROM utxo_set \
            \  JOIN utxo_state ON live_utxo = utxo_id"
          let coins    = sumOf (each . to fromOnly . to box'value) r
              Height h = bhHeight bh'
          liftIO $ fromIntegral h * 100 @=? coins
          --
          put bh'
          return $ Right boxId


----------------------------------------------------------------
-- Helpers for building transaction
----------------------------------------------------------------

-- | Create BoxInputRef which is protected by simple signature script
simpleInputRef :: BoxId -> Sigma.PublicKey -> BoxInputRef (Sigma.Sigma Sigma.PublicKey)
simpleInputRef boxId pk = BoxInputRef
  { boxInputRef'id      = boxId
  , boxInputRef'args    = mempty
  , boxInputRef'proof   = Just $ Fix $ Sigma.SigmaPk pk
  , boxInputRef'sigs    = []
  , boxInputRef'sigMask = SigAll
  }

simpleScript :: Sigma.PublicKey -> Script
simpleScript pk = coreProgToScript $ EPrim $ PrimSigma $ Fix $ Sigma.SigmaPk pk

-- | Unspendable box
burnBox :: Money -> Box
burnBox n = Box { box'value  = n
                , box'script = coreProgToScript $ EPrim $ PrimBool False
                , box'args   = mempty
                }
