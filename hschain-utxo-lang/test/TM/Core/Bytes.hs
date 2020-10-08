module TM.Core.Bytes(
    tests
) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.RefEval
import qualified Hschain.Utxo.Lang.Const as Const
import TM.Core.Common


tests :: TestTree
tests = testGroup "core-bytes"
  [ testProgram "hash"               (progHash val)            (PrimBool True)
  , testProgram "hash append"        (progHashAppend val 2)    (PrimBool True)
  , testProgram "ser/deser id"       (progConvertIdInt 100)    (PrimBool True)
  , testProgram "ser/deser id"       (progConvertIdText "100") (PrimBool True)
  ]
  where
    val = "hello bytes"

sha256V :: ExprCore -> ExprCore
sha256V = EAp "sha256"

serialiseIntV :: ExprCore -> ExprCore
serialiseIntV = EAp "serialiseInt"

deserialiseIntV :: ExprCore -> ExprCore
deserialiseIntV = EAp "deserialiseInt"

appendBytesV :: ExprCore -> ExprCore -> ExprCore
appendBytesV a b = ap name [a, b]
  where
    name = EVar $ Const.appendBytes

progHash :: ByteString -> ExprCore
progHash bs = equals BytesT (sha256V $ bytes bs) (bytes $ getSha256 bs)

progHashAppend :: ByteString -> Int64 -> ExprCore
progHashAppend bs n
  = equals BytesT
    (sha256V $ appendBytesV (bytes bs) (serialiseIntV $ int n))
    (bytes $ getSha256 $ bs <> serialiseInt n)

progConvertIdInt :: Int64 -> ExprCore
progConvertIdInt n
  = equals IntT (deserialiseIntV $ serialiseIntV $ int n) (int n)

progConvertIdText :: Text -> ExprCore
progConvertIdText n
  = equals TextT (deserialiseIntV $ serialiseIntV $ text n) (text n)

---------------------------------------
-- utils

testProgram :: String -> ExprCore -> Prim -> TestTree
testProgram name prog res = testGroup name
  [ testCase "typecheck" $ case typeCheck prog of
      Left  e -> assertFailure $ show e
      Right _ -> pure ()
  , testCase          "simple"    $ EvalPrim res @=? evalProg env prog
  ]
