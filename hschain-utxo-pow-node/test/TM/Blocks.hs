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
module TM.Blocks where

import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Coerce
import Data.Fix
import Data.Boolean
import qualified Data.Vector as V
import System.Timeout

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

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

import Hschain.Utxo.Lang.Expr (intArgs)
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Sigma.Types (generateKeyPair, KeyPair(..))
import Hschain.Utxo.Pow.App.Types
import qualified Hschain.Utxo.Lang.Sigma          as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Protocol as Sigma


tests :: TestTree
tests = testGroup "Running blockchain"
  [ testCase "Test harness works" $ runMiner $ do
      _ <- mineBlock Nothing []
      _ <- mineBlock Nothing []
      return ()
  , testCase "Pay for coffee" $ runMiner payforCoffee
  ]

----------------------------------------------------------------
-- Pay for cofee
----------------------------------------------------------------

payforCoffee :: Mine ()
payforCoffee = do
  alice@KeyPair{publicKey=pkAlice, secretKey=skAlice} <- liftIO $ generateKeyPair
  bob@KeyPair  {publicKey=pkBob,   secretKey=skBob  } <- liftIO $ generateKeyPair
  let sigmaEnv = Sigma.Env [ alice, bob ]
  -- H=1 Alice mines block
  bidAlice <- mineBlock (Just pkAlice) []
  -- H=2 Alice sends reward to Bob with hash-lock
  let getSpendHeight = listAt (getBoxIntArgList (getInput (int 0))) (int 0)
      -- receiver can get money only height is greater than specified limit
      receiverScript
        =   pk' pkBob
        &&* toSigma (getSpendHeight <* getHeight)
      -- sender can get money back if height is less or equals to specified limit
      refundScript
        =   pk' pkAlice
        &&* toSigma (getSpendHeight >=* getHeight)
  txToBob <- newProofTx sigmaEnv $ Tx
    { tx'inputs =
      [ BoxInputRef
        { boxInputRef'id      = bidAlice
        , boxInputRef'args    = mempty
        , boxInputRef'proof   = Just $ Fix $ SigmaPk pkAlice
        , boxInputRef'sigs    = []
        , boxInputRef'sigMask = SigAll
        }
      ]
    , tx'outputs =
      [ Box { box'value  = 100
            , box'script = mainScriptUnsafe $ receiverScript ||* refundScript
            , box'args   = intArgs [ 4 ]
            }
      ]
    }
  _ <- mineBlock Nothing [ txToBob ]
  -- H=3 Bob tries to spend transaction
  let coffeeBoxId = computeBoxId (computeTxId txToBob) 0
  txBob <- newProofTx sigmaEnv $ Tx
    { tx'inputs =
      [ BoxInputRef
        { boxInputRef'id      = coffeeBoxId
        , boxInputRef'args    = mempty
        , boxInputRef'proof   = Just $ Fix $ SigmaPk pkBob
        , boxInputRef'sigs    = []
        , boxInputRef'sigMask = SigAll
        }
      ]
    , tx'outputs =
      [ Box { box'value  = 100
            , box'script = coreProgToScript $ EPrim $ PrimBool False
            , box'args   = mempty
            }
      ]
    }
  Left _ <- mineBlockE Nothing [ txBob ]
  -- H=3,4. Just skip some
  _ <- mineBlock Nothing []
  _ <- mineBlock Nothing []
  -- H=5. Bob successfully spends transaction
  _ <- mineBlock Nothing [ txBob ]
  pure ()




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
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail)

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

mineBlock :: Maybe PublicKey -> [Tx] -> Mine BoxId
mineBlock mpk txs = mineBlockE mpk txs >>= \case
  Left  e -> liftIO $ throwM e
  Right a -> return a

-- | Mine block containing transactions
mineBlockE :: Maybe PublicKey -> [Tx] -> Mine (Either SomeException BoxId)
mineBlockE mpk txs = Mine $ do
  bh        <- get
  (upd,pow) <- ask
  -- Make coinbase transaction
  let bid = bhBID bh
      coinbaseBox = Box { box'value  = miningRewardAmount
                        , box'script = coreProgToScript $ case mpk of
                            Nothing -> EPrim $ PrimBool True
                            Just pk -> EPrim $ PrimSigma $ Fix $ SigmaPk pk
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
      { ubData   = merkled $ coinbase : txs
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
