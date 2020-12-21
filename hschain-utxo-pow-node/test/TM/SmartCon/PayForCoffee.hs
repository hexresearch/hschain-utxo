{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE OverloadedLists #-}
-- |
module TM.SmartCon.PayForCoffee where

import Control.Monad.Reader

import Data.Boolean

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma.Types (generateKeyPair, KeyPair(..))
import qualified Hschain.Utxo.Lang.Sigma.Protocol as Sigma

import TM.BCH.Util

tests :: TestTree
tests = testGroup "Pay for coffee"
  [ testCase "Bob"   $ runMiner $ payforCoffee True
  , testCase "Alice" $ runMiner $ payforCoffee False
  ]

payforCoffee :: Bool -> Mine ()
payforCoffee isBob = do
  alice@KeyPair{publicKey=pkAlice} <- liftIO $ generateKeyPair
  bob@KeyPair  {publicKey=pkBob  } <- liftIO $ generateKeyPair
  let sigmaEnv = Sigma.Env [ alice, bob ]
  -- H=1 Alice mines block
  bidAlice <- mineBlock (Just pkAlice) []
  -- H=2 Alice sends reward to Bob with hash-lock
  let getSpendHeight = getBoxArgs (getInput (int 0))
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
      txBob <- newProofTx sigmaEnv $ Tx
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
      txAlice <- newProofTx sigmaEnv $ Tx
        { tx'inputs  = [ simpleInputRef coffeeBoxId pkAlice ]
        , tx'outputs = [ burnBox 100 ]
        , tx'dataInputs = []
        }
      _ <- mineBlock Nothing [txAlice]
      pure ()
