module TM.Core.Cost(
    tests
) where

import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Core.Cost
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Types

tests :: TestTree
tests = testGroup "test-cost"
  [ {-testCase "Cost works" $ (isJust $ getProgCost $ progEquality $ PrimInt 0) @=? True-}
  ]


progEquality :: Prim -> ExprCore
progEquality p = (EPrimOp (OpEQ ty) `EAp` EPrim p) `EAp` EPrim p
  where
    ty = primToType p

