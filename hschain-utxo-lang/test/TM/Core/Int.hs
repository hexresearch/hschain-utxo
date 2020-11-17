module TM.Core.Int(
    tests
) where

import Data.Int
import Test.Tasty

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Types
import TM.Core.Common


tests :: TestTree
tests = testGroup "arithmetics"
  [ testAdd 2 2
  , testMul 4 5
  , testSub 10 1
  , testDiv 10 3
  , testProgramFail "Div zero" divZeroProg
  ]
  where
    testAdd = testNumOp OpAdd (+)
    testMul = testNumOp OpMul (*)
    testSub = testNumOp OpSub (\x y -> x - y)
    testDiv = testNumOp OpDiv div

    divZeroProg = primOp OpDiv [int 1, int 0]

testNumOp :: PrimOp TypeCore -> (Int64 -> Int64 -> Int64) -> Int64 -> Int64 -> TestTree
testNumOp op eval a b = testProgram msg prog res
  where
    msg  = unwords [show op, show a, show b]
    prog = primOp op [int a, int b]
    res  = PrimInt (a `eval` b)

