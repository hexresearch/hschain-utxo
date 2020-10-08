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

tests :: TestTree
tests = testGroup "core-lists"
  [ testGroup "list-functions"
    [ testProgram     "listAt 0"               (progListAt 0) [PrimInt 1]
    , testProgram     "listAt 1"               (progListAt 1) [PrimInt 2]
    , testProgramFail "listAt out of bound"    (progListAt 4)
    , testProgram     "Typecheck concat lists" progConcatList (fmap PrimInt [1..6])
    , testProgram     "Typecheck map lists"    progMapList (fmap PrimInt [10, 20, 30])
    , testProgram     "Typecheck sum lists"    progSumList [PrimInt 21]
    , testProgram     "Typecheck or lists"     (progOrList 2) [PrimBool True]
    , testProgram     "Or lists is false"      (progOrList (-2)) [PrimBool False]
    , testProgram     "Any list"               (progAnyList 2) [PrimBool True]
    , testProgram     "All list"               (progAllList 2) [PrimBool False]
    , testProgram     "All sigma list"         progSigmaAllList
      [PrimSigma (Fix (SigmaAnd [Fix (SigmaBool True), Fix (SigmaBool False), Fix (SigmaBool True)]))]
    ]
  ]

testProgram :: String -> ExprCore -> [Prim] -> TestTree
testProgram nm prog res = testProgramBy nm prog (Right res)

testProgramFail :: String -> ExprCore -> TestTree
testProgramFail nm prog = testProgramBy nm prog (Left ())

testProgramBy :: String -> ExprCore -> Either e [Prim] -> TestTree
testProgramBy nm prog res = testGroup nm
  [ testCase "typecheck" $ case typeCheck prog of
      Left  e -> assertFailure $ show e
      Right _ -> pure ()
  , testCase "simple" $ case res of
      Left  _   -> return ()
      Right [r] -> EvalPrim r @=? evalProg env prog
      Right r   -> EvalList r @=? evalProg env prog
  ]


listToExpr :: TypeCore -> [ExprCore] -> ExprCore
listToExpr ty = foldr cons nil
  where
    nil      = EConstr (ListT ty) 0
    cons a b = ap (EConstr (ListT ty) 1) [a, b]

listConsts :: ExprCore -> ExprCore
listConsts
  = ELet "xs" (nums xs)
  . ELet "ys" (nums ys)
  . ELet "zs" (nums zs)
  . ELet "bs" (bools bs)
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
progListAt :: Int64 -> ExprCore
progListAt n
  = listConsts
  $ listAt IntT "xs" (int n)

-- | Concatenation of two lists.
progConcatList :: ExprCore
progConcatList
  = listConsts
  $ appendList IntT "xs" "ys"

-- | Map over list
progMapList :: ExprCore
progMapList
  = listConsts
  $ mapList IntT IntT (EAp (EPrimOp OpMul) (int 10)) "xs"

progSumList :: ExprCore
progSumList
  = listConsts
  $ ap (EPrimOp OpListSum) ["zs"]

progOrList :: Int64 -> ExprCore
progOrList n
  = listConsts
  $ ap (EPrimOp OpListOr) [mapList IntT BoolT (isIntV n) "zs"]

isIntV :: Int64 -> ExprCore
isIntV n = EAp (EPrimOp (OpEQ IntT)) (int n)

progAnyList :: Int64 -> ExprCore
progAnyList n
  = listConsts
  $ ap (EPrimOp (OpListAny IntT)) [isIntV n, "xs"]

progAllList :: Int64 -> ExprCore
progAllList n
  = listConsts
  $ ap (EPrimOp (OpListAll IntT)) [isIntV n, "xs"]

progSigmaAllList :: ExprCore
progSigmaAllList
  = listConsts
  $ ap (EPrimOp (OpSigListAll BoolT)) [EPrimOp OpSigBool, "bs"]
