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

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Expr  (Box(..),BoxId(..),Script(..))
import Hschain.Utxo.Lang.Types (InputEnv(..))
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.RefEval
import Examples.SKI

import Hschain.Utxo.Lang.Pretty
import qualified Data.Text.IO as T

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

testProgram :: String -> CoreProg -> [Prim] -> TestTree
testProgram nm prog res = testProgramBy nm prog (Right res)

testProgramFail :: String -> CoreProg -> TestTree
testProgramFail nm prog = testProgramBy nm prog (Left ())

testProgramBy :: String -> CoreProg -> Either e [Prim] -> TestTree
testProgramBy nm prog res = testGroup nm
  [ testTypeCheckCase "typecheck" prog
  , testCase "simple" $ case res of
      Left  _   -> return ()
      Right [r] -> EvalPrim r @=? evalProg env prog
      Right r   -> EvalList r @=? evalProg env prog
  ]

testTypeCheckCase :: [Char] -> CoreProg -> TestTree
testTypeCheckCase testName prog =
  testCase testName $ do
    let tc = typeCheck prog
    mapM_ (T.putStrLn . renderText) tc
    Nothing @=? tc

listToExpr :: TypeCore -> [ExprCore] -> ExprCore
listToExpr ty = foldr cons nil
  where
    nil      = EConstr (ListT ty) 0
    cons a b = ap (EConstr (ListT ty) 1) [a, b]

listConsts :: CoreProg
listConsts = CoreProg
  [ nums "xs"  xs
  , nums "ys"  ys
  , nums "zs"  zs
  , bools "bs" bs
  ]
  where
    nums  name values = constantComb name (ListT IntT)  $ listToExpr IntT $ fmap (EPrim . PrimInt) values
    bools name values = constantComb name (ListT BoolT) $ listToExpr BoolT $ fmap (EPrim . PrimBool) values
    xs = [1,2,3]
    ys = [4,5,6]
    zs = xs ++ ys
    bs = [True, False, True]


-- | Index to list.
-- We index the list [1,2,3] with given index.
-- Out of bound should terminate with BottomTerm.
progListAt :: Int64 -> CoreProg
progListAt n = mainProg $ Typed (listAt IntT "xs" (int n)) IntT

-- | Concatenation of two lists.
progConcatList :: CoreProg
progConcatList = mainProg $ Typed (appendList IntT "xs" "ys") (ListT IntT)

-- | Map over list
progMapList :: CoreProg
progMapList = mainProg $ Typed (mapList IntT IntT (EAp (EPrimOp OpMul) (int 10)) "xs") (ListT IntT)

progSumList :: CoreProg
progSumList = mainProg $ Typed (ap (EPrimOp OpListSum) ["zs"]) IntT

progOrList :: Int64 -> CoreProg
progOrList n = listConsts <>
  CoreProg [mkMain orExpr]
  where
    orExpr = Typed (ap (EPrimOp OpListOr) [mapList IntT BoolT (isIntV n) "zs"]) BoolT

isIntV :: Int64 -> ExprCore
isIntV n = EAp (EPrimOp (OpEQ IntT)) (int n)

progAnyList :: Int64 -> CoreProg
progAnyList n = mainProg $ Typed (ap (EPrimOp (OpListAny IntT)) [isIntV n, "xs"]) BoolT

progAllList :: Int64 -> CoreProg
progAllList n = mainProg $ Typed (ap (EPrimOp (OpListAll IntT)) [isIntV n, "xs"]) BoolT

progSigmaAllList :: CoreProg
progSigmaAllList = mainProg $ Typed
  (ap (EPrimOp (OpSigListAll BoolT)) [EPrimOp OpSigBool, "bs"]) SigmaT

mainProg :: Typed TypeCore ExprCore -> CoreProg
mainProg expr = listConsts <> CoreProg [mkMain expr]

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
