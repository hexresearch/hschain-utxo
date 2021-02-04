{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE QuasiQuotes         #-}
-- |
module TM.SmartCon.PayForCoffee where

import Control.Monad.Reader

import Data.Int

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Sigma.Types (generateKeyPair, KeyPair(..))
import qualified Hschain.Utxo.Lang.Sigma as Sigma

import TM.BCH.Util

tests :: TestTree
tests = testGroup "Pay for coffee"
  [ testCase "Bob"   $ runMiner $ payforCoffee True
  , testCase "Alice" $ runMiner $ payforCoffee False
  ]

payforCoffee :: Bool -> Mine ()
payforCoffee isBob = do
  alice@KeyPair{getPublicKey=pkAlice} <- liftIO $ generateKeyPair
  bob@KeyPair  {getPublicKey=pkBob  } <- liftIO $ generateKeyPair
  let sigmaEnv = Sigma.toProofEnv [ alice, bob ]
  -- H=1 Alice mines block
  bidAlice <- mineBlock (Just pkAlice) []
  -- H=2 Alice sends reward to Bob with time-lock
  Right txToBob <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidAlice pkAlice ]
    , tx'outputs =
      [ Box { box'value  = 100
            , box'script = coffeeScript pkAlice pkBob
            , box'args   = toArgs (4 :: Int)
            }
      ]
    , tx'dataInputs = []
    }
  _ <- mineBlock Nothing [ txToBob ]
  let coffeeBoxId = computeBoxId (computeTxId txToBob) 0
  -- Here we play ou two possible scenario
  case isBob of
    True -> do
      -- H=3 Bob tries to spend transaction
      Right txBob <- newProofTx sigmaEnv $ Tx
        { tx'inputs  = [ simpleInputRef coffeeBoxId pkBob ]
        , tx'outputs = [ burnBox 100 ]
        , tx'dataInputs = []
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
      Right txAlice <- newProofTx sigmaEnv $ Tx
        { tx'inputs  = [ simpleInputRef coffeeBoxId pkAlice ]
        , tx'outputs = [ burnBox 100 ]
        , tx'dataInputs = []
        }
      _ <- mineBlock Nothing [txAlice]
      pure ()

coffeeScript :: PublicKey -> PublicKey -> Script
coffeeScript senderPk receiverPk = [utxo|
    h              = getBoxArgs getSelf :: Int
    receiverScript = pk $(receiverPk) &&* (h <*  getHeight)
    refundScript   = pk $(senderPk)   &&* (h >=* getHeight)

    main = receiverScript ||* refundScript
|]

