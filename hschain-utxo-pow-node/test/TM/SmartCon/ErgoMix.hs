{-# LANGUAGE OverloadedLists #-}
-- |
module TM.SmartCon.ErgoMix where

import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.Fix
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Sigma.EllipticCurve(EC(..))
import qualified Hschain.Utxo.Lang.Sigma          as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Protocol as Sigma

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
      sigmaEnv = Sigma.Env [ alice, bob ]
  -- Alice and Bob mine some money
  bidAlice <- mineBlock (Just pkAlice) []
  bidBob   <- mineBlock (Just pkBob  ) []
  bidAlicePool <- alicePostToPool sigmaEnv pkAlice bidAlice
  (bidBobPool1, bidBobPool2) <- bobJoinMix sigmaEnv bob pkAlice bidBob bidAlicePool
  return ()

alicePostToPool :: ProofEnv -> PublicKey -> BoxId -> Mine BoxId
alicePostToPool env pkAlice bidAlice = do
  tx <- newProofTx env $ Tx
        { tx'inputs  = [
            BoxInputRef { boxInputRef'id      = bidAlice
                        , boxInputRef'args    = mempty
                        , boxInputRef'proof   = Just $ Sigma.dlogSigma pkAlice
                        , boxInputRef'sigs    = []
                        , boxInputRef'sigMask = SigAll
                        }
            ]
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

bobJoinMix :: ProofEnv -> KeyPair -> PublicKey -> BoxId -> BoxId -> Mine (BoxId, BoxId)
bobJoinMix env bob pkAlice bidBob bidAlicePool = do
  tx <- newProofTx env $ Tx
        { tx'inputs =
            [ BoxInputRef
                { boxInputRef'id      = bidAlicePool
                , boxInputRef'args    = toArgs @PublicKey pkAlice
                , boxInputRef'proof   = Nothing
                , boxInputRef'sigs    = []
                , boxInputRef'sigMask = SigAll
                }
            , BoxInputRef
                { boxInputRef'id      = bidBob
                , boxInputRef'args    = mempty
                , boxInputRef'proof   = Just $ Fix $ Sigma.SigmaOr
                                                [ Fix $ Sigma.SigmaOr
                                                  [ Fix $ Sigma.SigmaPk $ Sigma.dtupleInput gx gy gxy
                                                  , Fix $ Sigma.SigmaPk $ Sigma.dtupleInput gx gxy gy
                                                  ]
                                                , Fix $ Sigma.SigmaPk $ Sigma.dlogInput gx
                                                ]
                , boxInputRef'sigs    = []
                , boxInputRef'sigMask = SigAll
                }
            ]
        , tx'outputs = [toOut gy gxy, toOut gxy gy]
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

