{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- |
module TM.Blocks where

import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Coerce
import qualified Data.Vector as V
import System.Timeout

import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Crypto (Hash(..))
import HSChain.Control.Class
import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.Types.Merkle.Types
import HSChain.PoW.Types hiding (Tx)
import HSChain.PoW.Tests
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import qualified HSChain.POW            as POW
import HSChain.Logger
import HSChain.Network.Types
import HSChain.Network.Mock
import HSChain.PoW.Consensus
import HSChain.Store.Query

import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Pow.App.Types

tests :: TestTree
tests = testGroup "Running blockchain"
  [ testCase "Test harness works" $ runMiner $ do
      _ <- mineBlock []
      _ <- mineBlock []
      (return ())
  ]


----------------------------------------------------------------
-- Parameters for test blockchain
----------------------------------------------------------------

data Test

instance UtxoPOWCongig Test where
  powConfig      _ = POW.defaultPOWConfig
  checkBlockWork _ = False

genesis :: Block (UTXOBlock Test)
genesis = GBlock
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
type Blk = UTXOBlock Test

-- | Monad for manual creation of blocks
newtype Mine a = Mine (ReaderT ( Src (BH Blk, StateView (HSChainT IO) Blk)
                               , PoW (HSChainT IO) Blk
                               )
                        (StateT (BH (UTXOBlock Test))
                          (HSChainT IO))
                        a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

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
      $ runReaderT action (upd, pow)
  where
    netcfg = NetCfg { nKnownPeers     = 3
                    , nConnectedPeers = 3
                    }

mineBlock :: [Tx] -> Mine BoxId
mineBlock txs = mineBlockE txs >>= \case
  Left  e -> liftIO $ throwM e
  Right a -> return a

-- | Mine block containing transactions
mineBlockE :: [Tx] -> Mine (Either SomeException BoxId)
mineBlockE txs = Mine $ do
  bh        <- get
  (upd,pow) <- ask
  -- Make coinbase transaction
  let bid = bhBID bh
      coinbaseBox = Box { box'value  = miningRewardAmount
                        , box'script = coreProgToScript $ EPrim (PrimBool True)
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
  r <- lift $ lift $ sendNewBlock pow GBlock
    { blockHeight = succ $ bhHeight bh
    , blockTime   = let Height h = bhHeight bh
                    in Time (fromIntegral h * 1000)
    , prevBlock   = Just bid
    , blockData   = UTXOBlock
      { ubData   = merkled [coinbase]
      , ubNonce  = ""
      , ubTarget = Target $ 2^(256::Int) - 1
      }
    }
  case r of
    Left  e  -> pure $ Left e
    Right () -> do
      liftIO (timeout 1e6 (atomically $ await upd)) >>= \case
        Nothing      -> pure $ Left undefined
        Just (bh',_) -> do put bh'
                           return $ Right boxId
  where
    netcfg = NetCfg { nKnownPeers     = 3
                    , nConnectedPeers = 3
                    }
