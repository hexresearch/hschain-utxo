module TM.Core.Bytes(
    tests
) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Expr  (Box(..),BoxId(..),Script(..))
import Hschain.Utxo.Lang.Types (InputEnv(..))
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.RefEval
import Examples.SKI

import Hschain.Utxo.Lang.Pretty
import qualified Data.Text.IO as T
import qualified Hschain.Utxo.Lang.Const as Const

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

progHash :: ByteString -> CoreProg
progHash bs = mainProg $
  Typed
    (equals BytesT (sha256V $ bytes bs) (bytes $ getSha256 bs))
    BoolT

progHashAppend :: ByteString -> Int64 -> CoreProg
progHashAppend bs n = mainProg $
  Typed
    (equals BytesT
        (sha256V $ appendBytesV (bytes bs) (serialiseIntV $ int n))
        (bytes $ getSha256 $ bs <> serialiseInt n))
    BoolT


progConvertIdInt :: Int64 -> CoreProg
progConvertIdInt n = mainProg $
  Typed
    (equals IntT (deserialiseIntV $ serialiseIntV $ int n) (int n))
    BoolT

progConvertIdText :: Text -> CoreProg
progConvertIdText n = mainProg $
  Typed
    (equals TextT (deserialiseIntV $ serialiseIntV $ text n) (text n))
    BoolT

---------------------------------------
-- utils

mainProg :: Typed TypeCore ExprCore -> CoreProg
mainProg expr = CoreProg [mkMain expr]

testTypeCheckCase :: [Char] -> CoreProg -> TestTree
testTypeCheckCase testName prog =
  testCase testName $ do
    let tc = typeCheck prog
    mapM_ (T.putStrLn . renderText) tc
    Nothing @=? tc

testProgram :: String -> CoreProg -> Prim -> TestTree
testProgram name prog res = testGroup name
  [ testTypeCheckCase "typecheck" prog
  , testCase          "simple"    $ EvalPrim res @=? evalProg env prog
  ]

env :: InputEnv
env = InputEnv
  { inputEnv'height   = 123
  , inputEnv'self     = Box
    { box'id     = BoxId $ hashBlob ""
    , box'value  = 100
    , box'script = Script ""
    , box'args   = mempty
    }
  , inputEnv'inputs   = mempty
  , inputEnv'outputs  = mempty
  , inputEnv'args     = mempty
  }
