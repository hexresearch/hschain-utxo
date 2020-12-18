module TM.Core.Bytes(
    tests
) where

import Data.ByteString (ByteString)
import Data.Int

import Test.Tasty

import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Utils.Hash
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Types
import TM.Core.Common


tests :: TestTree
tests = testGroup "core-bytes"
  [ testProgram "hash"               (progHash val)                     (PrimBool True)
  , testProgram "hash append"        (progHashAppend val 2)             (PrimBool True)
  , testProgram "ser/deser id int"   (progConvertId int IntT 100)     (PrimBool True)
  , testProgram "ser/deser id text"  (progConvertId text TextT "100") (PrimBool True)
  ]
  where
    val = "hello bytes"

sha256V :: Core Name -> Core Name
sha256V a = primOp OpSHA256 [a]

serialise :: TypeCore -> Core Name-> Core Name
serialise ty a = primOp (OpToBytes ty) [a]

deserialise :: TypeCore -> Core Name -> Core Name
deserialise ty a = primOp (OpFromBytes ty) [a]

appendBytes :: Core Name -> Core Name -> Core Name
appendBytes a b = primOp OpBytesAppend [a, b]

progHash :: ByteString -> Core Name
progHash bs = equals BytesT (sha256V $ bytes bs) (bytes $ getSha256 bs)

progHashAppend :: ByteString -> Int64 -> Core Name
progHashAppend bs n
  = equals BytesT
    (sha256V $ appendBytes (bytes bs) (serialise IntT $ int n))
    (bytes $ getSha256 $ bs <> serialiseTerm n)

progConvertId :: (a -> Core Name) -> TypeCore -> a -> Core Name
progConvertId con ty n
  = equals ty (deserialise ty $ serialise ty $ con n) (con n)

