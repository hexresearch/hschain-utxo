{-# LANGUAGE OverloadedLists #-}
module TM.Core.Box(
    tests
) where

import Data.Int

import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Crypto (Hash(..))
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.RefEval
import Hschain.Utxo.Lang.Utils.ByteString
import TM.Core.Common (mkBoxInput, mkBoxOutput)
import Examples.SKI (alt)

import qualified Data.Vector as V

blockChainHeight :: Int64
blockChainHeight = 10

txEnv :: InputEnv
txEnv = InputEnv
  { inputEnv'height     = blockChainHeight
  , inputEnv'self       = in2
  , inputEnv'inputs     = [in1, in2]
  , inputEnv'outputs    = [out1]
  , inputEnv'dataInputs = [din1, din2]
  }
  where
    in1 = mkBoxInput (mkBoxId "box-1") Box
      { box'value  = 1
      , box'script = Script "in1"
      , box'args   = toArgs ([4,5] :: [Int64])
      }

    in2 = (mkBoxInput (mkBoxId "box-2") Box
      { box'value  = 2
      , box'script = Script "in2"
      , box'args   = toArgs (([6,7], ["john", "neil"]) :: ([Int64], [Text]))
      })
      { boxInput'args = toArgs (([1,2,3], ["alice", "bob"], [True, False]) :: ([Int64], [Text], [Bool])) }

    out1 = mkBoxOutput blockChainHeight  (mkBoxId "box-3") Box
      { box'value  = 3
      , box'script = Script "out1"
      , box'args   = toArgs ([8,9] :: [Int64])
      }

    din1 = mkBoxOutput blockChainHeight  (mkBoxId "box-4") Box
      { box'value  = 2
      , box'script = Script "out4"
      , box'args   = toArgs ([9, 10] :: [Int64])
      }

    din2 = mkBoxOutput blockChainHeight  (mkBoxId "box-5") Box
      { box'value  = 2
      , box'script = Script "out5"
      , box'args   = toArgs (([11, 12], ["kate"]) :: ([Int64], [Text]))
      }
    mkBoxId s = BoxId (TxId (Hash s)) 0


tests :: TestTree
tests = testGroup "core-boxes"
    [ testProg "get height"         (PrimInt blockChainHeight) progGetHeight
    , testProg "get self id"        (PrimBytes "box-2\0\0\0\0") progGetSelfId
    , testProg "get self script"    (PrimBytes "in2")  progGetSelfScript
    , testProg "get tx arg"         (PrimInt 2)        progGetTxArg
    , testProg "get input id"       (PrimBytes "box-1\0\0\0\0") progGetInputId
    , testProg "get output id"      (PrimBytes "box-3\0\0\0\0") progGetOutputId
    , testProg "get output arg"     (PrimInt 9) progGetOutputLastIntArg
    , testProg "get input text arg" (PrimText "neil") progGetInputLastTextArg
    , testProg "get data-input id"  (PrimBytes "box-4\0\0\0\0") progGetDataInputId
    , testProg "get data-input text"(PrimText "kate") progGetDataInputLastTextArg
    ]

testProg :: String -> Prim -> Core Name -> TestTree
testProg name res prog = testGroup name
  [ testCase "typecheck" $ case typeCheck prog of
      Left  e -> assertFailure $ show e
      Right _ -> pure ()
  , testCase          "simple"    $ Right (PrimVal res)  @=? evalProg txEnv prog
  ]

progGetHeight :: Core Name
progGetHeight = getHeight

progGetSelfId :: Core Name
progGetSelfId = getBoxId getSelf

progGetSelfScript :: Core Name
progGetSelfScript = getBoxScript getSelf

progGetTxArg :: Core Name
progGetTxArg = ECase (getArgs (TupleT [ListT IntT, ListT TextT, ListT BoolT]))
  [ alt (ConTuple (V.fromList [ListT IntT, ListT TextT, ListT BoolT])) ["a", "b", "c"] (listAt IntT (EVar "a") (int 1))
  ]

progGetInputId :: Core Name
progGetInputId = getBoxId $ listAt BoxT getInputs (int 0)

progGetOutputId :: Core Name
progGetOutputId = getBoxId $ listAt BoxT getOutputs (int 0)

progGetDataInputId :: Core Name
progGetDataInputId = getBoxId $ listAt BoxT getDataInputs (int 0)

progGetOutputLastIntArg :: Core Name
progGetOutputLastIntArg = listAt IntT (getBoxArgs (ListT IntT) $ listAt BoxT getOutputs (int 0)) (int 1)

progGetInputLastTextArg :: Core Name
progGetInputLastTextArg = ECase (getBoxArgs (TupleT [ListT IntT, ListT TextT]) $ listAt BoxT getInputs (int 1))
  [ alt (ConTuple $ V.fromList [ListT IntT, ListT TextT]) ["ints", "texts"] (listAt TextT (EVar "texts") (int 1))
  ]

progGetDataInputLastTextArg :: Core Name
progGetDataInputLastTextArg = ECase (getBoxArgs (TupleT [ListT IntT, ListT TextT]) $ listAt BoxT getDataInputs (int 1))
  [ alt (ConTuple $ V.fromList [ListT IntT, ListT TextT]) ["ints", "texts"] (listAt TextT (EVar "texts") (int 0))
  ]
