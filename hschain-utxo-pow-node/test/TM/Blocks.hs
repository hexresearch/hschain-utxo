{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE OverloadedLists #-}
-- |
module TM.Blocks where

import Control.Monad.Reader

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma.Types (generateKeyPair, KeyPair(..))
import qualified Hschain.Utxo.Lang.Sigma.Protocol as Sigma

import TM.BCH.Util

tests :: TestTree
tests = testGroup "Running blockchain"
  [ testCase "Test harness works" $ runMiner $ do
      _ <- mineBlock Nothing []
      _ <- mineBlock Nothing []
      return ()
  , testCase "No double transactions" $ runMiner noDoubleTx
  , testCase "Simple transfers"       $ runMiner simpleTransfers
  ]

----------------------------------------------------------------
-- Same transaction couldn't be used twice
----------------------------------------------------------------

noDoubleTx :: Mine ()
noDoubleTx = do
  alice@KeyPair  {publicKey=pkAlice  } <- liftIO generateKeyPair
  let sigmaEnv = Sigma.Env [ alice ]
  -- H=1. Alice mines block
  bidAlice <- mineBlock (Just pkAlice) []
  -- H=2. Alice spends mining reward
  txAlice <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidAlice pkAlice ]
    , tx'outputs = [ burnBox 100 ]
    , tx'dataInputs = []
    }
  _ <- mineBlock Nothing [txAlice]
  -- Same transaction now should be rejected
  mineBlockE Nothing (Just 0) [txAlice] >>= \case
    Left  _ -> pure ()
    Right _ -> error "Block should be rejected"
  return ()


----------------------------------------------------------------
-- Simple transfers
--
-- We just move moeny around and check that movements are correct
----------------------------------------------------------------

simpleTransfers :: Mine ()
simpleTransfers = do
  alice@KeyPair  {publicKey=pkAlice  } <- liftIO generateKeyPair
  bob@KeyPair    {publicKey=pkBob    } <- liftIO generateKeyPair
  charlie@KeyPair{publicKey=pkCharlie} <- liftIO generateKeyPair
  let sigmaEnv = Sigma.Env [ alice, bob, charlie ]
  ----------------------------------------
  -- H=1 Alice mines block
  bidAlice <- mineBlock (Just pkAlice) []
  ----------------------------------------
  -- H=2
  --
  -- Bob can't spend output
  badTx sigmaEnv $ Tx
    { tx'inputs     = [ simpleInputRef bidAlice pkBob ]
    , tx'outputs    = [ burnBox 100 ]
    , tx'dataInputs = []
    }
  -- Alice can't create more money
  badTx sigmaEnv $ Tx
    { tx'inputs     = [ simpleInputRef bidAlice pkAlice ]
    , tx'outputs    = [ burnBox 101 ]
    , tx'dataInputs = []
    }
  -- Now Alice may spend mined block and pay 10 coin in fees
  txAlice <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidAlice pkAlice ]
    , tx'outputs =
      [ Box { box'value  = 60
            , box'script = simpleScript pkBob
            , box'args   = mempty
            }
      , Box { box'value  = 30
            , box'script = simpleScript pkCharlie
            , box'args   = mempty
            }
      ]
    , tx'dataInputs = []
    }
  bidBob <- mineBlock (Just pkBob) [txAlice]
  ----------------------------------------
  -- H=3
  --
  --  * Bob burns new coinbase with fee
  txBob1 <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidBob pkBob ]
    , tx'outputs = [ burnBox 110 ]
    , tx'dataInputs = []
    }
  --  * Bob and Charlie burn money received from Alice
  txBob2 <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef (computeBoxId (computeTxId txAlice) 0) pkBob ]
    , tx'outputs = [ burnBox 60 ]
    , tx'dataInputs = []
    }
  txCharlie <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef (computeBoxId (computeTxId txAlice) 1) pkCharlie ]
    , tx'outputs = [ burnBox 30 ]
    , tx'dataInputs = []
    }
  _ <- mineBlock Nothing [ txBob1, txBob2, txCharlie ]
  pure ()
 
