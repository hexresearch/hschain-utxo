-- | Basic tests for sigma-protocols
module TM.Tx.Sigma(
  tests
) where

import Data.Fix
import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types

tests :: TestTree
tests = testGroup "sigma-protocols"
  [ testCase "verify correct sign message (same for tx-content)" $ ( @=? True)  =<< verifySameSignMessage
  , testCase "verify correct single owner script"                $ ( @=? True)  =<< verifyAliceTx
  , testCase "verify broken tx"                                  $ ( @=? False) =<< verifyBrokenTx
  ]

singleOwnerSigma :: PublicKey -> Sigma PublicKey
singleOwnerSigma pubKey = Fix $ SigmaPk pubKey

verifySameSignMessage :: IO Bool
verifySameSignMessage = do
  (tx, txContent) <- initTx
  return $ getTxBytes tx == getTxContentBytes txContent

-- | Inits transaction that is owned by alice and has correct proof.
initTx :: IO (Tx, TxContent BoxInputRef)
initTx = do
  aliceSecret <- newSecret
  let alicePubKey = getPublicKey aliceSecret
      aliceProofEnv = toProofEnv [getKeyPair aliceSecret]
  resTx <- newProofTx aliceProofEnv $ tx alicePubKey
  return (resTx, fmap expectedBox'input $ tx alicePubKey)
  where
    tx pubKey = TxContent
      { txContent'inputs = return $ ExpectedBox
                                      { expectedBox'sigma = Just $ singleOwnerSigma pubKey
                                      , expectedBox'input = inputRef
                                      }
      , txContent'outputs = return $ BoxContent
                                      { boxContent'value  = 1
                                      , boxContent'script = mainScriptUnsafe $ pk $ text $ publicKeyToText pubKey
                                      , boxContent'args   = mempty
                                      }
      }

    inputRef = BoxInputRef
      { boxInputRef'id    = BoxId "box-1"
      , boxInputRef'args  = mempty
      , boxInputRef'proof = Nothing
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
verifyTx tx = all (maybe False (\proof -> verifyProof proof message) . boxInputRef'proof) $ tx'inputs tx
  where
    message = getTxBytes tx

