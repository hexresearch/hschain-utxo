-- | Basic tests for sigma-protocols
module TM.Tx.Sigma(
  tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types

tests :: TestTree
tests = testGroup "sigma-protocols"
  [ testCase "verify correct sign message (same for pre-tx)" $ ( @=? True)  =<< verifySameSignMessage
  , testCase "verify correct box identifiers"                $ ( @=? True)  =<< verifyValidBoxIds
  , testCase "verify correct single owner script"            $ ( @=? True)  =<< verifyAliceTx
  , testCase "verify broken tx"                              $ ( @=? False) =<< verifyBrokenTx
  ]

verifySameSignMessage :: IO Bool
verifySameSignMessage = do
  (tx, preTx) <- initTx
  return $ computeTxId tx == computePreTxId preTx

verifyValidBoxIds :: IO Bool
verifyValidBoxIds = do
  tx <- fmap fst initTx
  return $ validateOutputBoxIds tx

-- | Inits transaction that is owned by alice and has correct proof.
initTx :: IO (Tx, PreTx BoxInputRef)
initTx = do
  aliceSecret <- newSecret
  let alicePubKey = getPublicKey aliceSecret
      aliceProofEnv = toProofEnv [getKeyPair aliceSecret]
  resTx <- newProofTx aliceProofEnv $ tx alicePubKey
  return (resTx, fmap expectedBox'input $ tx alicePubKey)
  where
    tx pubKey = PreTx
      { preTx'inputs = singleOwnerInput (BoxId $ hashBlob "box-1") pubKey
      , preTx'outputs = return $ PreBox
                                      { preBox'value  = 1
                                      , preBox'script = mainScriptUnsafe $ pk $ text $ publicKeyToText pubKey
                                      , preBox'args   = mempty
                                      }
      }

-- | Verify that proof is correct
verifyAliceTx :: IO Bool
verifyAliceTx = fmap (verifyTx . fst) initTx

-- | Let's pretend that Bob captures Alice's correct transaction
-- and tries to steal the output by injecting his ownership script.
bobStealsTx :: Tx -> IO Tx
bobStealsTx aliceTx = do
  bobSecret <- newSecret
  let bobPubKey = getPublicKey bobSecret
      -- robbery happens: we substitute ownership of all boxes to Bob's key
      stealScript box = box
        { box'script = mainScriptUnsafe $ pk $ text $ publicKeyToText bobPubKey
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
verifyTx tx = all (maybe False (\proof -> verifyProof proof tid) . boxInputRef'proof) $ tx'inputs tx
  where
    tid = computeTxId tx

