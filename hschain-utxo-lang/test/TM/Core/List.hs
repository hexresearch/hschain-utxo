module TM.Core.List(
    tests
  , progListAt
  , progConcatList
  , progMapList
  , progSumList
  , progOrList
  , listConsts
  , withBigList
) where

import Data.Int

import Test.Tasty

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Types
import TM.Core.Common
import Examples.SKI (let_)

tests :: TestTree
tests = testGroup "core-lists"
  [ testGroup "list-functions"
    [ testProgram     "listAt 0"               (progListAt 0) (PrimInt 1)
    , testProgram     "listAt 1"               (progListAt 1) (PrimInt 2)
    , testProgramFail "listAt out of bound"    (progListAt 4)
    , testProgramL    "Typecheck concat lists" progConcatList (fmap PrimInt [1..6])
    , testProgramL    "Typecheck map lists"    progMapList (fmap PrimInt [10, 20, 30])
    , testProgram     "Typecheck sum lists"    progSumList (PrimInt 21)
    , testProgram     "Typecheck or lists"     (progOrList 2) (PrimBool True)
    , testProgram     "Or lists is false"      (progOrList (-2)) (PrimBool False)
    , testProgram     "Any list"               (progAnyList 2) (PrimBool True)
    , testProgram     "All list"               (progAllList 2) (PrimBool False)
    , testProgram     "All sigma list"         progSigmaAllList
      (PrimSigma $ AND () [sigmaB True, sigmaB False, sigmaB True])
    , testProgramFail "Too many reductions"     (progBigListReduce bigSize)
    , testProgram     "Ok amount of reductions" (progBigListReduce okSize) (PrimInt (sum ([0 .. okSize] :: [Int64])))
    ]
  ]
  where
    sigmaB = Leaf () . Left
    bigSize = 1000000
    okSize  = 1000

listToExpr :: TypeCore -> [Core Name] -> Core Name
listToExpr ty = foldr cons nil
  where
    nil      = EConstr (ConNil ty)
    cons a b = ap (EConstr (ConCons ty)) [a, b]

listConsts :: Core Name -> Core Name
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


withBigList :: Int64 -> Core Name -> Core Name
withBigList size =
  let_ "hugeList" (listToExpr IntT $ fmap (EPrim . PrimInt) [0 .. size])

-- | Index to list.
-- We index the list [1,2,3] with given index.
-- Out of bound should terminate with BottomTerm.
progListAt :: Int64 -> Core Name
progListAt n
  = listConsts
  $ listAt IntT "xs" (int n)

-- | Concatenation of two lists.
progConcatList :: Core Name
progConcatList
  = listConsts
  $ appendList IntT "xs" "ys"

-- | Map over list
progMapList :: Core Name
progMapList
  = listConsts
  $ mapList IntT IntT (EAp (EPrimOp OpMul) (int 10)) "xs"

progSumList :: Core Name
progSumList
  = listConsts
  $ ap (EPrimOp OpListSum) ["zs"]

progOrList :: Int64 -> Core Name
progOrList n
  = listConsts
  $ ap (EPrimOp OpListOr) [mapList IntT BoolT (isIntV n) "zs"]

isIntV :: Int64 -> Core Name
isIntV n = EAp (EPrimOp (OpEQ IntT)) (int n)

progAnyList :: Int64 -> Core Name
progAnyList n
  = listConsts
  $ ap (EPrimOp (OpListAny IntT)) [isIntV n, "xs"]

progAllList :: Int64 -> Core Name
progAllList n
  = listConsts
  $ ap (EPrimOp (OpListAll IntT)) [isIntV n, "xs"]

progSigmaAllList :: Core Name
progSigmaAllList
  = listConsts
  $ ap (EPrimOp (OpSigListAll BoolT)) [EPrimOp OpSigBool, "bs"]

progBigListReduce :: Int64 -> Core Name
progBigListReduce size = withBigList size $ ap (EPrimOp OpListSum)  ["hugeList"]
