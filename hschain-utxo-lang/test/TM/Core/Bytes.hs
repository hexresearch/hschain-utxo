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

sha256V :: ExprCore -> ExprCore
sha256V a = primOp OpSHA256 [a]

serialise :: ArgType -> ExprCore -> ExprCore
serialise ty a = primOp (OpToBytes ty) [a]

deserialise :: ArgType -> ExprCore -> ExprCore
deserialise ty a = primOp (OpFromBytes ty) [a]

appendBytes :: ExprCore -> ExprCore -> ExprCore
appendBytes a b = primOp OpBytesAppend [a, b]

progHash :: ByteString -> ExprCore
progHash bs = equals BytesT (sha256V $ bytes bs) (bytes $ getSha256 bs)

progHashAppend :: ByteString -> Int64 -> ExprCore
progHashAppend bs n
  = equals BytesT
    (sha256V $ appendBytes (bytes bs) (serialise IntArg $ int n))
    (bytes $ getSha256 $ bs <> serialiseInt n)

progConvertId :: (a -> ExprCore) -> ArgType -> a -> ExprCore
progConvertId con ty n
  = equals (argTypeToCore ty) (deserialise ty $ serialise ty $ con n) (con n)

