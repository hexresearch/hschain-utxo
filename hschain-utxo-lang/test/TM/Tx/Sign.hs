{-# LANGUAGE OverloadedLists #-}
module TM.Tx.Sign(
  tests
) where

import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe

import HSChain.Crypto (Hash(..))
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.RefEval
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Crypto.Signature

import TM.Core.Common (mkBoxInput, mkBoxOutput)

import qualified Data.ByteString.Char8 as C

import HSChain.Crypto (ByteRepr(..))

import qualified Data.Vector as V

tests :: TestTree
tests = testGroup "signatures"
  [ testCase "verify signature primitives"  $ (True @=? ) =<< verifySignPrimitives
  , testCase "signature encode/decode"      $ (True @=? ) =<< testByteReprSignature
  , testCase "test checkSig"                $ (True @=? ) =<< testCheckSig
  , testCase "test checkMultiSig"           $ (True @=? ) =<< testCheckMultiSig 2   -- 2 of 3 multisig, pass 2
  , testCase "test checkMultiSig fail"      $ (False @=? ) =<< testCheckMultiSig 1  -- 2 of 3 multisig, pass 1, should fail
  ]

verifySignPrimitives :: IO Bool
verifySignPrimitives = do
  secret <- newSecret
  let pubKey = getPublicKey secret
  sig <- sign secret toyMessage
  return $ verify pubKey sig toyMessage

toyMessage :: SigMessage
toyMessage = fromJust $ decodeFromBS $ C.pack "Sign me!"

testByteReprSignature :: IO Bool
testByteReprSignature = do
  secret <- newSecret
  sig <- sign secret toyMessage
  return $ decodeFromBS (encodeToBS sig) == Just sig


pkExpr :: PublicKey -> Core Name
pkExpr = bytes . encodeToBS

testCheckSig :: IO Bool
testCheckSig = do
  alice <- newSecret
  let alicePubKey = getPublicKey alice
  env  <- inputEnv [alice] testMsg
  let script = checkSig (pkExpr alicePubKey) (int 0)
  return $ evalProg env script == EvalPrim (PrimBool True)

-- | Checks for 2 of 3 multi-sig
-- First argument is how many signatures to provide (use non zero, less than 4)
testCheckMultiSig :: Int -> IO Bool
testCheckMultiSig sigCount = do
  privKeys <- replicateM 3 newSecret
  let pubKeys = fmap getPublicKey privKeys
  -- all signatures but first dropCount keys are present, and we duplicate first signature as fill in
  env <- inputEnv (dupFirst $ dropPrivs privKeys) testMsg
  let script = checkMultiSig (int 2) (listExpr BytesT $ fmap pkExpr pubKeys) (listExpr IntT $ fmap int [0, 1, 2])
  return $ evalProg env script == EvalPrim (PrimBool True)
  where
    dropCount = 3 - sigCount

    dupFirst as = replicate dropCount (head as) ++ as
    dropPrivs allPrivKeys = drop dropCount allPrivKeys

testMsg :: SigMessage
testMsg = SigMessage $ fromJust $ decodeFromBS "Simple TX to sign"

inputEnv :: [Secret] -> SigMessage -> IO InputEnv
inputEnv keys msg = do
  sigs <- mapM (\k -> sign k msg) keys
  return $ InputEnv
    { inputEnv'height     = 10
    , inputEnv'self       = in1
    , inputEnv'inputs     = [in1]
    , inputEnv'outputs    = [out1]
    , inputEnv'dataInputs = []
    , inputEnv'args       = mempty
    , inputEnv'sigs       = V.fromList sigs
    , inputEnv'sigMsg     = msg
    }
  where
    in1 = mkBoxInput (BoxId $ Hash "box-1") Box
      { box'value  = 1
      , box'script = Script "in1"
      , box'args   = mempty
      }

    out1 = mkBoxOutput 10 (BoxId $ Hash "box-3") Box
      { box'value  = 3
      , box'script = Script "out1"
      , box'args   = mempty
      }
