{-# LANGUAGE OverloadedLists #-}
-- | Test for ErgoMix exchange as described in Section 3.3 of
-- "Advanced ErgoScript tutorial" by Ergo developers (2019)
module TM.SmartCon.ErgoMix where

import Control.Monad.Reader

import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Sigma.EllipticCurve(EC(..))
import qualified Hschain.Utxo.Lang.Sigma          as Sigma
import System.Random (randomIO)

import TM.BCH.Util

tests :: TestTree
tests = testGroup "ErgoMix test"
  [ testCase "ErgoMix: Alice and Bob make mix-exchange" $ runMiner ergoMixTest
  ]

ergoMixTest :: Mine ()
ergoMixTest = do
  alice <- liftIO newKeyPair
  bob   <- liftIO newKeyPair
  let pkAlice = getPublicKey alice
      pkBob   = getPublicKey bob
      sigmaEnv = Sigma.toProofEnv [ alice, bob ]
  -- Alice and Bob mine some money
  bidAlice <- mineBlock (Just pkAlice) []
  bidBob   <- mineBlock (Just pkBob  ) []
  -- Alice adds money to mix pool
  bidAlicePool <- alicePostToPool sigmaEnv pkAlice bidAlice
  -- Bob generates random guess flag, and mixes Alice's Box with his own box.
  bobGuess <- liftIO randomIO
  bobPoolBids <- bobJoinMix bobGuess sigmaEnv bob pkAlice bidBob bidAlicePool
  -- Parties spend the money according to turn.
  -- Note that each party can see only public key of the other party.
  aliceSpends (pickAliceBoxId bobGuess bobPoolBids) alice pkBob
  bobSpends   (pickBobBoxId   bobGuess bobPoolBids) bob   pkAlice
  return ()

alicePostToPool :: ProofEnv -> PublicKey -> BoxId -> Mine BoxId
alicePostToPool env pkAlice bidAlice = do
  tx <- newProofTx env $ Tx
        { tx'inputs  = [ singleOwnerInput bidAlice pkAlice ]
        , tx'outputs =
            [ Box { box'value  = 100
                  , box'script = halfMixScript (hashScript fullMixScript)
                  , box'args   = toArgs @PublicKey pkAlice
                  }
            ]
       , tx'dataInputs = []
        }
  _ <- mineBlock Nothing [ tx ]
  return (computeBoxId (computeTxId tx) 0)

bobJoinMix :: Bool -> ProofEnv -> KeyPair -> PublicKey -> BoxId -> BoxId -> Mine (BoxId, BoxId)
bobJoinMix bobGuess env bob pkAlice bidBob bidAlicePool = do
  tx <- newProofTx env $ Tx
        { tx'inputs =
            [ singleOwnerInput bidBob pkBob
            , BoxInputRef
                { boxInputRef'id      = bidAlicePool
                , boxInputRef'args    = mempty
                , boxInputRef'proof   = Just $ Sigma.OR ()
                    [ Sigma.OR () $ (if bobGuess then id else NE.reverse)
                      [ Sigma.dtupleSigma gx gy gxy
                      , Sigma.dtupleSigma gx gxy gy
                      ]
                    , Sigma.dlogSigma gx
                    ]
                , boxInputRef'sigs    = []
                , boxInputRef'sigMask = SigAll
                }
            ]
        , tx'outputs = let mixer = (if bobGuess then id else flip) toOut
                       in [mixer gy gxy, mixer gxy gy]
        , tx'dataInputs = []
        }
  _ <- mineBlock Nothing [tx]
  let txId = computeTxId tx
  return (computeBoxId txId 0, computeBoxId txId 1)
  where
    gx  = pkAlice
    gy  = pkBob
    y   = getSecretKey bob
    gxy = y .*^ gx

    pkBob = getPublicKey bob

    toOut :: PublicKey -> PublicKey -> Box
    toOut w1 w2 = Box
      { box'value  = 100
      , box'script = fullMixScript
      , box'args   = toArgs (pkAlice, w1, w2)
      }

aliceSpends :: BoxId -> KeyPair -> PublicKey -> Mine ()
aliceSpends bid alice pkBob = do
  tx <- newProofTx (toProofEnv [alice]) $ Tx
        { tx'inputs  = [spendInput ]
        , tx'outputs = [ burnBox 100 ]
        , tx'dataInputs = []
        }
  void $ mineBlock Nothing [tx]
  where
    spendInput = BoxInputRef
      { boxInputRef'id      = bid
      , boxInputRef'args    = mempty
      , boxInputRef'proof   = Just $ Sigma.OR ()
            [ Sigma.dlogSigma g_xy
            , Sigma.dtupleSigma g_y g_x g_xy
            ]
      , boxInputRef'sigs    = []
      , boxInputRef'sigMask = SigAll
      }
      where
        g_y  = pkBob
        g_x  = getPublicKey alice
        g_xy = getSecretKey alice .*^ g_y

bobSpends :: BoxId -> KeyPair -> PublicKey -> Mine ()
bobSpends bid bob pkAlice = do
  tx <- newProofTx (toProofEnv [bob]) $ Tx
        { tx'inputs  = [spendInput ]
        , tx'outputs = [ burnBox 100 ]
        , tx'dataInputs = []
        }
  void $ mineBlock Nothing [tx]
  where
    spendInput = BoxInputRef
      { boxInputRef'id      = bid
      , boxInputRef'args    = mempty
      , boxInputRef'proof   = Just $ Sigma.OR ()
            [ Sigma.dlogSigma g_y
            , Sigma.dtupleSigma g_xy g_x g_y
            ]
      , boxInputRef'sigs    = []
      , boxInputRef'sigMask = SigAll
      }
      where
        g_y  = getPublicKey bob
        g_x  = pkAlice
        g_xy = getSecretKey bob .*^ g_x

pickAliceBoxId :: Bool -> (BoxId, BoxId) -> BoxId
pickAliceBoxId turn (bid1, bid2)
  | turn      = bid1
  | otherwise = bid2

pickBobBoxId :: Bool -> (BoxId, BoxId) -> BoxId
pickBobBoxId turn bids = pickAliceBoxId (not turn) bids

-------------------------------------------------------------
-- ErgoMix scripts

fullMixScript :: Script
fullMixScript = [utxo|

main = pk d ||* dtuple c u d
  where
    (u, c, d) = getBoxArgs getSelf
|]

halfMixScript :: ByteString -> Script
halfMixScript fullScriptHash = [utxo|

main = fullMixTx &&* (bob ||* alice)
  where
    u = getBoxArgs getSelf
    (u0, c0, d0) = getBoxArgs (getOutput 0) :: (Bytes, Bytes, Bytes)
    (u1, c1, d1) = getBoxArgs (getOutput 1) :: (Bytes, Bytes, Bytes)

    bob   =  toSigma ((u0 == u) && (u1 == u) && (c0 == d1) && (c1 == d0))
         &&* (dtuple u c0 d0 ||* dtuple u d0 c0)

    fullMixBox b = sha256 (getBoxScript b) == $(fullScriptHash)
    fullMixTx = toSigma (
                 (getBoxValue (getOutput 0) == getBoxValue getSelf)
              && (getBoxValue (getOutput 1) == getBoxValue getSelf)
              && fullMixBox (getOutput 0)
              && fullMixBox (getOutput 1))

    alice = pk u
|]

