module TM.Core.Bytes(
    tests
) where

import Data.ByteString (ByteString)
import Data.Int

import Test.Tasty

import Hschain.Utxo.Lang.Types (ArgType(..))
import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Types
import TM.Core.Common


tests :: TestTree
tests = testGroup "core-bytes"
  [ testProgram "hash"               (progHash val)                     (PrimBool True)
  , testProgram "hash append"        (progHashAppend val 2)             (PrimBool True)
  , testProgram "ser/deser id int"   (progConvertId int IntArg 100)     (PrimBool True)
  , testProgram "ser/deser id text"  (progConvertId text TextArg "100") (PrimBool True)
  ]
  where
    val = "hello bytes"

sha256V :: Core Identity Name -> Core Identity Name
sha256V a = primOp OpSHA256 [a]

serialise :: ArgType -> Core Identity Name-> Core Identity Name
serialise ty a = primOp (OpToBytes ty) [a]

deserialise :: ArgType -> Core Identity Name -> Core Identity Name
deserialise ty a = primOp (OpFromBytes ty) [a]

appendBytes :: Core Identity Name -> Core Identity Name -> Core Identity Name
appendBytes a b = primOp OpBytesAppend [a, b]

progHash :: ByteString -> Core Identity Name
progHash bs = equals BytesT (sha256V $ bytes bs) (bytes $ getSha256 bs)

progHashAppend :: ByteString -> Int64 -> Core Identity Name
progHashAppend bs n
  = equals BytesT
    (sha256V $ appendBytes (bytes bs) (serialise IntArg $ int n))
    (bytes $ getSha256 $ bs <> serialiseInt n)

progConvertId :: (a -> Core Identity Name) -> ArgType -> a -> Core Identity Name
progConvertId con ty n
  = equals (argTypeToCore ty) (deserialise ty $ serialise ty $ con n) (con n)

