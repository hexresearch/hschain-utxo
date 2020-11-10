{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE OverloadedLists #-}
-- |
module TM.Blocks where

import Control.Monad.Reader

import Data.Boolean

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

import Hschain.Utxo.Lang.Expr (intArgs)
import Hschain.Utxo.Lang.Build
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
  , testCase "Pay for coffee (Bob)"   $ runMiner $ payforCoffee True
  , testCase "Pay for coffee (Alice)" $ runMiner $ payforCoffee False
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
    { tx'inputs  = [ simpleInputRef bidAlice pkBob ]
    , tx'outputs = [ burnBox 100 ]
    }
  -- Alice can't create more money
  badTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidAlice pkAlice ]
    , tx'outputs = [ burnBox 101 ]
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
    }
  bidBob <- mineBlock (Just pkBob) [txAlice]
  ----------------------------------------
  -- H=3
  --
  --  * Bob burns new coinbase with fee
  txBob1 <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidBob pkBob ]
    , tx'outputs = [ burnBox 110 ]
    }
  --  * Bob and Charlie burn money received from Alice
  txBob2 <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef (computeBoxId (computeTxId txAlice) 0) pkBob ]
    , tx'outputs = [ burnBox 60 ]
    }
  txCharlie <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef (computeBoxId (computeTxId txAlice) 1) pkCharlie ]
    , tx'outputs = [ burnBox 30 ]
    }
  _ <- mineBlock Nothing [ txBob1, txBob2, txCharlie ]
  pure ()

----------------------------------------------------------------
-- Pay for cofee
----------------------------------------------------------------

payforCoffee :: Bool -> Mine ()
payforCoffee isBob = do
  alice@KeyPair{publicKey=pkAlice} <- liftIO $ generateKeyPair
  bob@KeyPair  {publicKey=pkBob  } <- liftIO $ generateKeyPair
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
    { tx'inputs  = [ simpleInputRef bidAlice pkAlice ]
    , tx'outputs =
      [ Box { box'value  = 100
            , box'script = mainScriptUnsafe $ receiverScript ||* refundScript
            , box'args   = intArgs [ 4 ]
            }
      ]
    }
  _ <- mineBlock Nothing [ txToBob ]
  let coffeeBoxId = computeBoxId (computeTxId txToBob) 0
  -- Here we play ou two possible scenario
  case isBob of
    True -> do
      -- H=3 Bob tries to spend transaction
      txBob <- newProofTx sigmaEnv $ Tx
        { tx'inputs  = [ simpleInputRef coffeeBoxId pkBob ]
        , tx'outputs = [ burnBox 100 ]
        }
      badBlock [ txBob ]
      -- H=3,4. Just skip some
      _ <- mineBlock Nothing []
      _ <- mineBlock Nothing []
      -- H=5. Bob successfully spends transaction
      _ <- mineBlock Nothing [ txBob ]
      pure ()
    ----------------------------------------
    False -> do
      -- H=3 Alice tries to spend transaction
      txAlice <- newProofTx sigmaEnv $ Tx
        { tx'inputs  = [ simpleInputRef coffeeBoxId pkAlice ]
        , tx'outputs = [ burnBox 100 ]
        }
      _ <- mineBlock Nothing [txAlice]
      pure ()
