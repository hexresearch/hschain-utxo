module TM.Core.Box(
    tests
) where

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

blockChainHeight :: Int64
blockChainHeight = 10

txEnv :: InputEnv
txEnv = InputEnv
  { inputEnv'height  = blockChainHeight
  , inputEnv'self    = in2
  , inputEnv'inputs  = [in1, in2]
  , inputEnv'outputs = [out1]
  , inputEnv'args    = intArgs [1,2,3] <> textArgs ["alice", "bob"] <> boolArgs [True, False]
  }
  where
    post box = PostBox box 1

    in1 = post $ Box
      { box'id     = BoxId $ Hash "box-1"
      , box'value  = 1
      , box'script = Script "in1"
      , box'args   = intArgs [4,5]
      }

    in2 = post $ Box
      { box'id     = BoxId $ Hash "box-2"
      , box'value  = 2
      , box'script = Script "in2"
      , box'args   = intArgs [6,7] <> textArgs ["john", "neil"]
      }

    out1 = Box
      { box'id     = BoxId $ Hash "box-3"
      , box'value  = 3
      , box'script = Script "out1"
      , box'args   = intArgs [8,9]
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
    ]

testProg :: String -> Prim -> ExprCore -> TestTree
testProg name res prog = testGroup name
  [ testCase "typecheck" $ case typeCheck prog of
      Left  e -> assertFailure $ show e
      Right _ -> pure ()
  , testCase          "simple"    $ EvalPrim res  @=? evalProg txEnv prog
  ]

progGetHeight :: ExprCore
progGetHeight = getHeight

progGetSelfId :: ExprCore
progGetSelfId = getBoxId getSelf

progGetSelfScript :: ExprCore
progGetSelfScript = getBoxScript getSelf

progGetTxArg :: ExprCore
progGetTxArg = listAt IntT getIntArgs (int 1)

progGetInputId :: ExprCore
progGetInputId = getBoxId $ listAt BoxT getInputs (int 0)

progGetOutputId :: ExprCore
progGetOutputId = getBoxId $ listAt BoxT getOutputs (int 0)

progGetOutputLastIntArg :: ExprCore
progGetOutputLastIntArg
  = listAt IntT (getBoxIntArgs $ listAt BoxT getOutputs (int 0)) (int 1)

progGetInputLastTextArg :: ExprCore
progGetInputLastTextArg
  = listAt TextT (getBoxTextArgs $ listAt BoxT getInputs (int 1)) (int 1)
