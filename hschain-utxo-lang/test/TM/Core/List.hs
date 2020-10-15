module TM.Core.List(
    tests
  , progListAt
  , progConcatList
  , progMapList
  , progSumList
  , progOrList
  , listConsts
) where

import Data.Fix
import Data.Int

import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.RefEval
import TM.Core.Common
import Examples.SKI   (let_)

tests :: TestTree
tests = testGroup "core-lists"
  [ testGroup "list-functions"
    [ testProgramL    "listAt 0"               (progListAt 0) [PrimInt 1]
    , testProgramL    "listAt 1"               (progListAt 1) [PrimInt 2]
    , testProgramFail "listAt out of bound"    (progListAt 4)
    , testProgramL    "Typecheck concat lists" progConcatList (fmap PrimInt [1..6])
    , testProgramL    "Typecheck map lists"    progMapList (fmap PrimInt [10, 20, 30])
    , testProgramL    "Typecheck sum lists"    progSumList [PrimInt 21]
    , testProgramL    "Typecheck or lists"     (progOrList 2) [PrimBool True]
    , testProgramL    "Or lists is false"      (progOrList (-2)) [PrimBool False]
    , testProgramL    "Any list"               (progAnyList 2) [PrimBool True]
    , testProgramL    "All list"               (progAllList 2) [PrimBool False]
    , testProgramL    "All sigma list"         progSigmaAllList
      [PrimSigma (Fix (SigmaAnd [Fix (SigmaBool True), Fix (SigmaBool False), Fix (SigmaBool True)]))]
    ]
  ]

listToExpr :: TypeCore -> [Core BindName Name] -> Core BindName Name
listToExpr ty = foldr cons nil
  where
    nil      = EConstr (ListT ty) 0
    cons a b = ap (EConstr (ListT ty) 1) [a, b]

listConsts :: Core BindName Name -> Core BindName Name
listConsts
  = let_ "xs" (nums xs)
  . let_ "ys" (nums ys)
  . let_ "zs" (nums zs)
  . let_ "bs" (bools bs)
  where
    nums  values = listToExpr IntT  $ fmap (EPrim . PrimInt)  values
    bools values = listToExpr BoolT $ fmap (EPrim . PrimBool) values
    xs = [1,2,3]
    ys = [4,5,6]
    zs = xs ++ ys
    bs = [True, False, True]


-- | Index to list.
-- We index the list [1,2,3] with given index.
-- Out of bound should terminate with BottomTerm.
progListAt :: Int64 -> Core BindName Name
progListAt n
  = listConsts
  $ listAt IntT "xs" (int n)

-- | Concatenation of two lists.
progConcatList :: Core BindName Name
progConcatList
  = listConsts
  $ appendList IntT "xs" "ys"

-- | Map over list
progMapList :: Core BindName Name
progMapList
  = listConsts
  $ mapList IntT IntT (EAp (EPrimOp OpMul) (int 10)) "xs"

progSumList :: Core BindName Name
progSumList
  = listConsts
  $ ap (EPrimOp OpListSum) ["zs"]

progOrList :: Int64 -> Core BindName Name
progOrList n
  = listConsts
  $ ap (EPrimOp OpListOr) [mapList IntT BoolT (isIntV n) "zs"]

isIntV :: Int64 -> Core BindName Name
isIntV n = EAp (EPrimOp (OpEQ IntT)) (int n)

progAnyList :: Int64 -> Core BindName Name
progAnyList n
  = listConsts
  $ ap (EPrimOp (OpListAny IntT)) [isIntV n, "xs"]

progAllList :: Int64 -> Core BindName Name
progAllList n
  = listConsts
  $ ap (EPrimOp (OpListAll IntT)) [isIntV n, "xs"]

progSigmaAllList :: Core BindName Name
progSigmaAllList
  = listConsts
  $ ap (EPrimOp (OpSigListAll BoolT)) [EPrimOp OpSigBool, "bs"]
