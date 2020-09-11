module TM.Core.Cost(
    tests
) where

import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Expr (intT,boolT,textT,bytesT,boxT)
import Hschain.Utxo.Lang.Core.Cost
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Data.Prim

import Examples.SKI

import Debug.Trace

tests :: TestTree
tests = testGroup "test-cost"
  [ testCase "Cost works" $ (isJust $ trace' $ getProgCost $ progEquality $ PrimInt 0) @=? True ]
  where
    trace' x = trace (show x) x

progEquality :: Prim -> CoreProg
progEquality p = CoreProg
  [ mkMain $ Typed
    { typed'value = (EPrimOp (OpEQ ty) `EAp` EPrim p) `EAp` EPrim p
    , typed'type  = boolT
    }
  ]
  where
    ty = primToType p

