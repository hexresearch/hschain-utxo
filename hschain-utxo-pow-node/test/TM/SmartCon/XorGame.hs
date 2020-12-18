{-# LANGUAGE OverloadedLists            #-}
-- |
-- This is implementation of XorGame from Advanced ErgoScript Tutorial
-- (§3.1)
module TM.SmartCon.XorGame where

import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.Int
import Data.Fix
import Data.String
import System.Random

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

import HSChain.Crypto (ByteRepr(..))
import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma.Types (generateKeyPair, KeyPair(..))
import Hschain.Utxo.Lang

import qualified Hschain.Utxo.Lang.Sigma            as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Protocol   as Sigma
import qualified Hschain.Utxo.Lang.Utils.Hash       as H

import TM.BCH.Util

tests :: TestTree
tests = testGroup "XorGame"
  [ testCase "XorGame [Alice]" $ runMiner $ xorGame 0 0
  , testCase "XorGame [Bob]"   $ runMiner $ xorGame 0 1
  ]

xorGame :: Int64 -> Int64 -> Mine ()
xorGame aliceGuess bobGuess = do
  alice@KeyPair  {publicKey=pkAlice  } <- liftIO generateKeyPair
  bob@KeyPair    {publicKey=pkBob    } <- liftIO generateKeyPair
  let sigmaEnv = Sigma.Env [ alice, bob ]
  -- Alice and Bob mine some money
  bidAlice <- mineBlock (Just pkAlice) []
  bidBob   <- mineBlock (Just pkBob  ) []
  -- First Alice prepares both halves of scripts
  (commitmentHash, secret) <- makeAliceSecret aliceGuess
  let fullScript = fullGameScript commitmentHash (encodeToBS pkAlice)
      halfScript = halfGameScript $ hashScript fullScript
  -- Alice posts half-game transaction
  txAlice <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidAlice pkAlice ]
    , tx'outputs = [
        Box { box'value  = 100
            , box'script = halfScript
            , box'args   = mempty
            }
        ]
    , tx'dataInputs = []
    }
  let aliceGameBid = computeBoxId (computeTxId txAlice) 0
  _ <- mineBlock Nothing [ txAlice ]
  -- Bob posts full game script
  txBob <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidBob pkBob
                   , BoxInputRef { boxInputRef'id      = aliceGameBid
                                 , boxInputRef'args    = mempty
                                 , boxInputRef'proof   = Nothing
                                 , boxInputRef'sigs    = []
                                 , boxInputRef'sigMask = SigAll
                                 }
                   ]
    , tx'outputs = [ Box { box'value  = 200
                         , box'script = fullScript
                         , box'args   = toArgs @(Int64, Int, ByteString) (bobGuess, 100, encodeToBS pkBob)
                         }
                   ]
    , tx'dataInputs = []
    }
  let bobGameBid = computeBoxId (computeTxId txBob) 0
  _ <- mineBlock Nothing [ txBob ]
  -- Now check that correct player can spend TX
  let aliceWin = Tx
        { tx'inputs  = [
            BoxInputRef { boxInputRef'id      = bobGameBid
                        , boxInputRef'args    = toArgs @(ByteString, Int64) (secret, aliceGuess)
                        , boxInputRef'proof   = Just $ Fix $ Sigma.SigmaPk pkAlice
                        , boxInputRef'sigs    = []
                        , boxInputRef'sigMask = SigAll
                        }
            ]
        , tx'outputs = [ burnBox 200 ]
        , tx'dataInputs = []
        }
      bobWin = Tx
        { tx'inputs  = [
            BoxInputRef { boxInputRef'id      = bobGameBid
                        , boxInputRef'args    = toArgs @(ByteString, Int64) (secret, aliceGuess)
                        , boxInputRef'proof   = Just $ Fix $ Sigma.SigmaPk pkBob
                        , boxInputRef'sigs    = []
                        , boxInputRef'sigMask = SigAll
                        }
            ]
        , tx'outputs = [ burnBox 200 ]
        , tx'dataInputs = []
        }
  case aliceGuess == bobGuess of
    -- Alice wins
    True -> do
      badTx sigmaEnv bobWin
      tx <- newProofTx sigmaEnv aliceWin
      _  <- mineBlock Nothing [tx]
      pure ()
    -- Bob wins
    False -> do
      badTx sigmaEnv aliceWin
      tx <- newProofTx sigmaEnv bobWin
      _  <- mineBlock Nothing [tx]
      pure ()

makeAliceSecret :: MonadIO m => Int64 -> m (ByteString, ByteString)
makeAliceSecret guess = liftIO $ do
  s <- fmap fromString $ sequence $ replicate 64 randomIO
  let k = H.getSha256 $ s <> serialiseTerm guess
  return (k, s)

----------------------------------------------------------------

halfGameScript :: ByteString -> Script
halfGameScript fullGameScriptHash = [utxo|

  validBobInput b = (b == 0) || (b == 1)

  main = case (getBoxArgs out :: (Int, Int, Bytes)) of
    (bobGuess, bobDeadline, _) -> andSigma
        [ toSigma (validBobInput bobGuess)
        , sha256 (getBoxScript out) ==* $(fullGameScriptHash)
        , (length getOutputs ==* 1) ||* (length getOutputs ==* 2)
        , bobDeadline >=* (getHeight + 30)
        , getBoxValue out >=* (2 * getBoxValue getSelf) ]
    where
      out = getOutput 0
|]

fullGameScript commitmentHash alice = [utxo|

  main = case (getArgs, getBoxArgs getSelf) of
    ((s, a), (b, bobDeadline, bob)) ->
          (pk bob &&* (getHeight >* bobDeadline))
      ||* (   (sha256 (appendBytes s (serialise (a :: Int))) ==* $(commitmentHash))
          &&* (   (pk $(alice) &&* (a ==* b))
              ||* (pk bob      &&* (a /=* b))
              )
          )
|]

