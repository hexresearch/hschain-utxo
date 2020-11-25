module TM.Core.Bytes(
    tests
) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)

import Test.Tasty

import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Types
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

sha256V :: Core Identity Name -> Core Identity Name
sha256V = EAp "sha256"

serialiseIntV :: Core Identity Name -> Core Identity Name
serialiseIntV = EAp "serialiseInt"

deserialiseIntV :: Core Identity Name -> Core Identity Name
deserialiseIntV = EAp "deserialiseInt"

appendBytesV :: Core Identity Name -> Core Identity Name -> Core Identity Name
appendBytesV a b = ap name [a, b]
  where
    name = EVar $ Const.appendBytes

progHash :: ByteString -> Core Identity Name
progHash bs = equals BytesT (sha256V $ bytes bs) (bytes $ getSha256 bs)

progHashAppend :: ByteString -> Int64 -> Core Identity Name
progHashAppend bs n
  = equals BytesT
    (sha256V $ appendBytesV (bytes bs) (serialiseIntV $ int n))
    (bytes $ getSha256 $ bs <> serialiseInt n)

progConvertIdInt :: Int64 -> Core Identity Name
progConvertIdInt n
  = equals IntT (deserialiseIntV $ serialiseIntV $ int n) (int n)

progConvertIdText :: Text -> Core Identity Name
progConvertIdText n
  = equals TextT (deserialiseIntV $ serialiseIntV $ text n) (text n)
