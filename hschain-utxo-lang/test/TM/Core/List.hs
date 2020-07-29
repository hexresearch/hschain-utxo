{-# LANGUAGE OverloadedStrings #-}
module TM.Core.List(
    tests
  , progListAt
  , progConcatList
  , progMapList
  , progSumList
  , progOrList
  , run
  , listConsts
) where

import Data.Int

import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine
import qualified Hschain.Utxo.Lang.Core.Data.Output as O
import Examples.SKI

import Hschain.Utxo.Lang.Pretty
import qualified Data.Text.IO as T

tests :: TestTree
tests = testGroup "core-lists"
  [ testGroup "list-functions"
    [ testProgram     "listAt 0"               (progListAt 0) [PrimInt 1]
    , testProgram     "listAt 1"               (progListAt 1) [PrimInt 2]
    , testProgramFail "listAt out of bound"    (progListAt 4) BottomTerm
    , testProgram     "Typecheck concat lists" progConcatList (fmap PrimInt [1..6])
    , testProgram     "Typecheck map lists"    progMapList (fmap PrimInt [10, 20, 30])
    , testProgram     "Typecheck sum lists"    progSumList [PrimInt 21]
    , testProgram     "Typecheck or lists"     (progOrList 2) [PrimBool True]
    , testProgram     "Or lists is false"      (progOrList (-2)) [PrimBool False]
    , testProgram     "Any list"               (progAnyList 2) [PrimBool True]
    , testProgram     "All list"               (progAllList 2) [PrimBool False]
    , testProgram     "All sigma list"         progSigmaAllList [PrimBool False]
    ]
  ]

testProgram :: String -> CoreProg -> [Prim] -> TestTree
testProgram nm prog res = testProgramBy nm prog (Right res)

testProgramFail :: String -> CoreProg -> Error -> TestTree
testProgramFail nm prog res = testProgramBy nm prog (Left res)

testProgramBy :: String -> CoreProg -> Either Error [Prim] -> TestTree
testProgramBy nm prog res = testGroup nm
  [ testTypeCheckCase "typecheck" prog
  , testCase "eval" $ res      @=? run prog
  ]

testTypeCheckCase :: [Char] -> CoreProg -> TestTree
testTypeCheckCase testName prog =
  testCase testName $ do
    let tc = typeCheck preludeTypeContext prog
    mapM_ (T.putStrLn . renderText) tc
    Nothing @=? tc

listToExpr :: TypeCore -> [ExprCore] -> ExprCore
listToExpr ty = foldr cons nil
  where
    nil      = EConstr nilTy 0 0
    cons a b = ap (EConstr consTy 1 2) [a, b]

    nilTy = listT ty
    consTy = funT [ty, listT ty] (listT ty)

listConsts :: CoreProg
listConsts = CoreProg
  [ nums "xs"  xs
  , nums "ys"  ys
  , nums "zs"  zs
  , bools "bs" bs
  ]
  where
    nums  name values  = constantComb name (listT intT)  $ listToExpr intT $ fmap (EPrim . PrimInt) values
    bools name values = constantComb name (listT boolT) $ listToExpr boolT $ fmap (EPrim . PrimBool) values
    xs = [1,2,3]
    ys = [4,5,6]
    zs = xs ++ ys
    bs = [True, False, True]


-- | Index to list.
-- We index the list [1,2,3] with given index.
-- Out of bound should terminate with BottomTerm.
progListAt :: Int64 -> CoreProg
progListAt n = mainProg $ Typed (listAt intT "xs" (int n)) intT

-- | Concatenation of two lists.
progConcatList :: CoreProg
progConcatList = mainProg $ Typed (appendList intT "xs" "ys") (listT intT)

-- | Map over list
progMapList :: CoreProg
progMapList = mainProg $ Typed (mapList intT intT (EAp "*" (int 10)) "xs") (listT intT)

progSumList :: CoreProg
progSumList = mainProg $ Typed (ap "sum" ["zs"]) intT

progOrList :: Int64 -> CoreProg
progOrList n = listConsts <>
  CoreProg [mkMain orExpr]
  where
    orExpr = Typed (ap "or" [mapList intT boolT (isIntV n) "zs"]) boolT

isIntV :: Int64 -> ExprCore
isIntV n = EAp "Int.equals" (int n)

progAnyList :: Int64 -> CoreProg
progAnyList n = mainProg $ Typed (ap (EPolyVar "any" [intT]) [isIntV n, "xs"]) boolT

progAllList :: Int64 -> CoreProg
progAllList n = mainProg $ Typed (ap (EPolyVar "all" [intT]) [isIntV n, "xs"]) boolT

progSigmaAllList :: CoreProg
progSigmaAllList = mainProg $ Typed (ap (EPolyVar "sigmaAll" [boolT]) ["toSigma", "bs"]) sigmaT

mainProg :: Typed ExprCore -> CoreProg
mainProg expr = listConsts <> CoreProg [mkMain expr]

run :: CoreProg -> Either Error [Prim]
run
  = fmap (O.toList . gmachine'output)
  . eval
  . compile . (CoreProg primitives <> )
