{-# Language OverloadedLists #-}
{-# Language OverloadedStrings #-}
-- | Basic tests for sigma-protocols
module TM.Tx.Sigma(
  tests
) where

import Data.Fix
import Data.Text

import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types

tests :: TestTree
tests = testGroup "sigma-protocols"
  [ testCase "verify correct single owner script"  $ ( @=? Right True)  =<< verifyAliceTx
  , testCase "verify broken tx"                    $ ( @=? Right False) =<< verifyBrokenTx
  ]

singleOwnerSigma :: PublicKey -> Sigma PublicKey
singleOwnerSigma pubKey = Fix $ SigmaPk pubKey

-- | Inits transaction that is owned by alice and has correct proof.
initTx :: IO (Either Text Tx)
initTx = do
  aliceSecret <- newSecret
  let alicePubKey = getPublicKey aliceSecret
      aliceProofEnv = toProofEnv [getKeyPair aliceSecret]
  case outBox alicePubKey of
    Left _    -> return $ Left "Failed to create owner script"
    Right box -> do
      let preTx = Tx
            { tx'inputs  = [BoxId "box-1"]
            , tx'outputs = [box]
            , tx'proof   = Nothing
            , tx'args    = mempty
            }
      eProof <- newProof aliceProofEnv (singleOwnerSigma alicePubKey) (getTxBytes preTx)
      return $ fmap (\proof -> preTx { tx'proof = Just proof }) eProof
  where
    outBox owner = fmap (\script -> Box
      { box'id     = BoxId "box-2"
      , box'value  = 1
      , box'script = script
      , box'args   = mempty
      }) $ mainScript $ pk $ text $ publicKeyToText owner

-- | Verify that proof is correct
verifyAliceTx :: IO (Either Text Bool)
verifyAliceTx = do
  eTx <- initTx
  return $ verifyTx =<< eTx

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
verifyBrokenTx :: IO (Either Text Bool)
verifyBrokenTx = do
  eAliceTx <- initTx
  case eAliceTx of
    Left err      -> return $ Left err
    Right aliceTx -> do
      bobTx <- bobStealsTx aliceTx
      return $ verifyTx bobTx

-- | External TX verifier.
verifyTx :: Tx -> Either Text Bool
verifyTx tx = do
  proof <- maybe (Left "No proof found") Right $ tx'proof tx
  return $ verifyProof proof (getTxBytes tx)
