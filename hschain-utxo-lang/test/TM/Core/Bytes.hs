{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module TM.Core.Bytes(
    tests
  , run
) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Utils.ByteString

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine
import qualified Hschain.Utxo.Lang.Core.Data.Output as O
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
sha256V = EAp (EVar $ Typed "sha256" (arrowT bytesT bytesT))

serialiseIntV :: ExprCore -> ExprCore
serialiseIntV = EAp (EVar $ Typed "serialiseInt" (arrowT intT bytesT))

deserialiseIntV :: ExprCore -> ExprCore
deserialiseIntV = EAp (EVar $ Typed "deserialiseInt" (arrowT bytesT intT))

appendBytesV :: ExprCore -> ExprCore -> ExprCore
appendBytesV a b = ap name [a, b]
  where
    name = EVar $ Typed Const.appendBytes (funT [bytesT, bytesT] bytesT)

progHash :: ByteString -> CoreProg
progHash bs = mainProg $
  Typed
    (equals bytesT (sha256V $ bytes bs) (bytes $ getSha256 bs))
    boolT

progHashAppend :: ByteString -> Int64 -> CoreProg
progHashAppend bs n = mainProg $
  Typed
    (equals bytesT
        (sha256V $ appendBytesV (bytes bs) (serialiseIntV $ int n))
        (bytes $ getSha256 $ bs <> serialiseInt n))
    boolT


progConvertIdInt :: Int64 -> CoreProg
progConvertIdInt n = mainProg $
  Typed
    (equals intT (deserialiseIntV $ serialiseIntV $ int n) (int n))
    boolT

progConvertIdText :: Text -> CoreProg
progConvertIdText n = mainProg $
  Typed
    (equals textT (deserialiseIntV $ serialiseIntV $ text n) (text n))
    boolT

---------------------------------------
-- utils

mainProg :: Typed ExprCore -> CoreProg
mainProg expr = CoreProg [mkMain expr]

testTypeCheckCase :: [Char] -> CoreProg -> TestTree
testTypeCheckCase testName prog =
  testCase testName $ do
    let tc = typeCheck preludeTypeContext prog
    mapM_ (T.putStrLn . renderText) tc
    Nothing @=? tc

testProgram :: String -> CoreProg -> Prim -> TestTree
testProgram name prog res = testGroup name
  [ testTypeCheckCase "typecheck" prog
  , testCase          "eval"      $ Right [res] @=? run prog
  ]

run :: CoreProg -> Either Error [Prim]
run
  = fmap (O.toList . gmachine'output)
  . eval
  . compile . (CoreProg primitives <> )
