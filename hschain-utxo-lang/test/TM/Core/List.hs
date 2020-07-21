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

primComb :: Name -> TypeCore -> ExprCore -> Scomb
primComb name ty expr = Scomb
  { scomb'name = name
  , scomb'args = mempty
  , scomb'body = Typed expr ty
  }

listToExpr :: TypeCore -> [ExprCore] -> ExprCore
listToExpr ty = foldr cons nil
  where
    nil      = EConstr nilTy 0 0
    cons a b = ap (EConstr consTy 1 2) [a, b]

    nilTy = listT ty
    consTy = funT [ty, listT ty] (listT ty)

listConsts :: CoreProg
listConsts = CoreProg
  [ nums "xs" xs
  , nums "ys" ys
  , nums "zs" zs
  ]
  where
    nums name values = primComb name (listT intT) $ listToExpr intT $ fmap (EPrim . PrimInt) values
    xs = [1,2,3]
    ys = [4,5,6]
    zs = xs ++ ys


xsV :: ExprCore
xsV = EVar $ Typed "xs" (listT intT)

ysV :: ExprCore
ysV = EVar $ Typed "ys" (listT intT)

zsV :: ExprCore
zsV = EVar $ Typed "zs" (listT intT)

-- | Index to list.
-- We index the list [1,2,3] with given index.
-- Out of bound should terminate with BottomTerm.
progListAt :: Int64 -> CoreProg
progListAt n = listConsts <>
  CoreProg [mkMain $ listAtExpr ]
  where
    listAtExpr = Typed (ap listAtV [int n, xsV]) intT

    listAtV = EVar $ Typed "listAt" (listAtT intT)

    listAtT ty = funT [intT, listT ty] ty

-- | Concatenation of two lists.
progConcatList :: CoreProg
progConcatList = listConsts <>
  CoreProg [mkMain concatExpr]
  where
    concatExpr = Typed (ap concatV [xsV, ysV]) (listT intT)
    concatV = EVar $ Typed "++" concatT
    concatT = funT [listT intT, listT intT] (listT intT)

-- | Map over list
progMapList :: CoreProg
progMapList = listConsts <>
  CoreProg [mkMain mapExpr]
  where
    mapExpr = Typed (ap mapV [EAp mulV (int 10), xsV]) (listT intT)
    mapV = EVar $ Typed "map" mapT
    mapT = funT [arrowT intT intT, listT intT] (listT intT)
    mulV = EVar $ Typed "*" mulT
    mulT = funT [intT, intT] intT

progSumList :: CoreProg
progSumList = listConsts <>
  CoreProg [mkMain sumExpr]
  where
    sumExpr = Typed (ap sumV [zsV]) intT
    sumV = EVar $ Typed "sum" sumT
    sumT = arrowT (listT intT) intT

progOrList :: Int64 -> CoreProg
progOrList n = listConsts <>
  CoreProg [mkMain orExpr]
  where
    orExpr = Typed (ap orV [ap mapV [isTwoV, zsV]]) boolT
    orV = EVar $ Typed "or" orT
    orT = arrowT (listT boolT) boolT

    isTwoV = EAp intEqV (int n)

    intEqV = EVar $ Typed "Int.equals" (funT [intT, intT] boolT)

    mapV = EVar $ Typed "map" mapT
    mapT = funT [arrowT intT boolT, listT intT] (listT boolT)

run :: CoreProg -> Either Error [Prim]
run
  = fmap (O.toList . gmachine'output)
  . eval
  . compile . (CoreProg primitives <> )
