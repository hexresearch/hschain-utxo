module TM.Tx.Sign(
  tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Crypto.Signature

import qualified Data.ByteString.Char8 as C

import HSChain.Crypto (ByteRepr(..))

tests :: TestTree
tests = testGroup "signatures"
  [ testCase "verify signature primitives"  $ (True @=? )  =<< verifySignPrimitives
  , testCase "signature encode/decode"      $ (True @=? )  =<< testByteReprSignature
  ]

verifySignPrimitives :: IO Bool
verifySignPrimitives = do
  secret <- newSecret
  let pubKey = getPublicKey secret
  sig <- signMessage secret toyMessage
  return $ verifyMessage pubKey sig toyMessage

toyMessage :: SigMessage
toyMessage = fromJust $ decodeFromBS $ C.pack "Sign me!"

testByteReprSignature :: IO Bool
testByteReprSignature = do
  secret <- newSecret
  sig <- signMessage secret toyMessage
  return $ decodeFromBS (encodeToBS sig) == Just sig


