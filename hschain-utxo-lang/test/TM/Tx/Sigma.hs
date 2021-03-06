-- | Basic tests for sigma-protocols
module TM.Tx.Sigma(
    tests
  , verifyTx
) where

import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Parser.Quoter
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types

tests :: TestTree
tests = testGroup "sigma-protocols"
  [ testCase "verify correct single owner script"            $ ( @=? True)  =<< verifyAliceTx
  , testCase "verify broken tx"                              $ ( @=? False) =<< verifyBrokenTx
  ]

-- | Inits transaction that is owned by alice and has correct proof.
initTx :: IO (Tx, GTx (SigmaE () ProofInput) Box)
initTx = do
  aliceSecret <- newSecret
  let alicePubKey = toPublicKey aliceSecret
      aliceProofEnv = toProofEnv [getKeyPair aliceSecret]
  resTx <- newProofTx aliceProofEnv $ tx alicePubKey
  return (resTx, tx alicePubKey)
  where
    tx pubKey = Tx
      { tx'inputs  = return $ singleOwnerInput (BoxId $ hashBlob "box-1") pubKey
       , tx'outputs = return $ Box
          { box'value  = 1
          , box'script = [utxo| pk $(pubKey) |]
          , box'args   = mempty
          }
      , tx'dataInputs = mempty
      }

-- | Verify that proof is correct
verifyAliceTx :: IO Bool
verifyAliceTx = fmap (verifyTx . fst) initTx

-- | Let's pretend that Bob captures Alice's correct transaction
-- and tries to steal the output by injecting his ownership script.
bobStealsTx :: Tx -> IO Tx
bobStealsTx aliceTx = do
  bobSecret <- newSecret
  let bobPubKey = toPublicKey bobSecret
      -- robbery happens: we substitute ownership of all boxes to Bob's key
      stealScript box = box
        { box'script = [utxo| pk $(bobPubKey) |]
        }
  return $ aliceTx
    { tx'outputs = fmap stealScript $ tx'outputs aliceTx
    }

-- | Verify that broken tx is incorrect. It should return False.
-- Proof should become invalid because Tx's digital signature does not
-- corresponds to supplied proof.
verifyBrokenTx :: IO Bool
verifyBrokenTx = do
  aliceTx <- fmap fst initTx
  bobTx <- bobStealsTx aliceTx
  return $ verifyTx bobTx

-- | External TX verifier.
verifyTx :: Tx -> Bool
verifyTx tx = all (\BoxInputRef{..} -> maybe False (\proof -> verifyProof proof (getSigMessage boxInputRef'sigMask tx)) boxInputRef'proof) $ tx'inputs tx

