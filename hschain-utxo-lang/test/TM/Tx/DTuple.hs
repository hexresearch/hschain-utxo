{-# Language OverloadedLists #-}
-- | Test for DTuple signature
module TM.Tx.DTuple(
  tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Fix

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Parser.Quoter
import Hschain.Utxo.Lang.Sigma.EllipticCurve (EC(..))
import Hschain.Utxo.Lang.Sigma.Protocol (ProofInput(..))
import Hschain.Utxo.Lang.Sigma.DTuple (DTuple(..))
import Hschain.Utxo.Lang.Types
import TM.Tx.Sigma (verifyTx)

tests :: TestTree
tests = testGroup "sigma-protocols"
  [ testCase "verify dtuple proof"        $ (True @=? )  =<< verifyDtupleProof
  , testCase "verify dtuple owner script" $ (True @=? )  =<< verifyDtupleTx
  ]

verifyDtupleProof :: IO Bool
verifyDtupleProof = do
  alice <- newKeyPair
  bob   <- newKeyPair
  let gx  = getPublicKey alice
      y   = getSecretKey bob
      gy  = getPublicKey bob
      gxy = y .*^ gx
      inp = InputDTuple $ DTuple groupGenerator gx gy gxy
      expr = Fix $ SigmaPk inp
  eProof <- newProof (toProofEnv [bob]) expr msg
  return $ either (const False) (\proof -> verifyProof proof msg) eProof
  where
    msg = SigMessage $ hashBlob "tx message"

verifyDtupleTx :: IO Bool
verifyDtupleTx = do
  alice <- newKeyPair
  bob   <- newKeyPair
  tx <- dtupleTx (getPublicKey alice) bob
  return $ verifyTx tx


dtupleTx :: PublicKey -> KeyPair -> IO Tx
dtupleTx gx keys = newProofTx (toProofEnv [keys]) $ Tx
  { tx'inputs     = [inBox]
  , tx'outputs    = [outBox]
  , tx'dataInputs = []
  }
  where
    gy  = getPublicKey keys
    y   = getSecretKey keys
    gxy = y .*^ gx

    inBox = BoxInputRef
              { boxInputRef'id      = BoxId $ hashBlob "box-1"
              , boxInputRef'args    = mempty
              , boxInputRef'proof   = Just $ Fix $ SigmaPk $ dtupleInput gx gy gxy
              , boxInputRef'sigs    = []
              , boxInputRef'sigMask = SigAll
              }

    outBox = Box
              { box'value  = 1
              , box'args   = mempty
              , box'script = let pubKey = getPublicKey keys
                             in  [utxo| pk $(pubKey) |]
              }

