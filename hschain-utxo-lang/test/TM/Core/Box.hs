{-# LANGUAGE OverloadedLists #-}
module TM.Core.Box(
    tests
) where

import Data.Default
import Data.Int

import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Crypto (Hash(..))
import Hschain.Utxo.Lang.Expr (intArgs, textArgs, boolArgs)
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.RefEval
import TM.Core.Common (mkBoxInput, mkBoxOutput)

blockChainHeight :: Int64
blockChainHeight = 10

txEnv :: InputEnv
txEnv = InputEnv
  { inputEnv'height     = blockChainHeight
  , inputEnv'self       = in2
  , inputEnv'inputs     = [in1, in2]
  , inputEnv'outputs    = [out1]
  , inputEnv'dataInputs = [din1, din2]
  , inputEnv'args       = intArgs [1,2,3] <> textArgs ["alice", "bob"] <> boolArgs [True, False]
  , inputEnv'sigs       = []
  , inputEnv'sigMsg     = def
  }
  where
    in1 = mkBoxInput (BoxId $ Hash "box-1") Box
      { box'value  = 1
      , box'script = Script "in1"
      , box'args   = intArgs [4,5]
      }

    in2 = mkBoxInput (BoxId $ Hash "box-2") Box
      { box'value  = 2
      , box'script = Script "in2"
      , box'args   = intArgs [6,7] <> textArgs ["john", "neil"]
      }

    out1 = mkBoxOutput blockChainHeight  (BoxId $ Hash "box-3") Box
      { box'value  = 3
      , box'script = Script "out1"
      , box'args   = intArgs [8,9]
      }

    din1 = mkBoxOutput blockChainHeight  (BoxId $ Hash "box-4") Box
      { box'value  = 2
      , box'script = Script "out4"
      , box'args   = intArgs [9, 10]
      }

    din2 = mkBoxOutput blockChainHeight  (BoxId $ Hash "box-5") Box
      { box'value  = 2
      , box'script = Script "out5"
      , box'args   = intArgs [11, 12] <> textArgs ["kate"]
      }

tests :: TestTree
tests = testGroup "core-boxes"
    [ testProg "get height"         (PrimInt blockChainHeight) progGetHeight
    , testProg "get self id"        (PrimBytes "box-2") progGetSelfId
    , testProg "get self script"    (PrimBytes "in2")  progGetSelfScript
    , testProg "get tx arg"         (PrimInt 2)        progGetTxArg
    , testProg "get input id"       (PrimBytes "box-1") progGetInputId
    , testProg "get output id"      (PrimBytes "box-3") progGetOutputId
    , testProg "get output arg"     (PrimInt 9) progGetOutputLastIntArg
    , testProg "get input text arg" (PrimText "neil") progGetInputLastTextArg
    , testProg "get data-input id"  (PrimBytes "box-4") progGetDataInputId
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
progGetTxArg = listAt IntT getIntArgs (int 1)

progGetInputId :: Core Name
progGetInputId = getBoxId $ listAt BoxT getInputs (int 0)

progGetOutputId :: Core Name
progGetOutputId = getBoxId $ listAt BoxT getOutputs (int 0)

progGetDataInputId :: Core Name
progGetDataInputId = getBoxId $ listAt BoxT getDataInputs (int 0)

progGetOutputLastIntArg :: Core Name
progGetOutputLastIntArg
  = listAt IntT (getBoxIntArgs $ listAt BoxT getOutputs (int 0)) (int 1)

progGetInputLastTextArg :: Core Name
progGetInputLastTextArg
  = listAt TextT (getBoxTextArgs $ listAt BoxT getInputs (int 1)) (int 1)

progGetDataInputLastTextArg :: Core Name
progGetDataInputLastTextArg
  = listAt TextT (getBoxTextArgs $ listAt BoxT getDataInputs (int 1)) (int 0)
