module TM.Core.Box(
    tests
) where

import Data.Int

import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Expr (Box(..), intArgs, textArgs, boolArgs, Script(..), BoxId(..))
import Hschain.Utxo.Lang.Types

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.RefEval
import Examples.SKI

import Hschain.Utxo.Lang.Pretty
import qualified Data.Text.IO as T

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
    in1 = Box
      { box'id     = BoxId "box-1"
      , box'value  = 1
      , box'script = Script "in1"
      , box'args   = intArgs [4,5]
      }

    in2 = Box
      { box'id     = BoxId "box-2"
      , box'value  = 2
      , box'script = Script "in2"
      , box'args   = intArgs [6,7] <> textArgs ["john", "neil"]
      }

    out1 = Box
      { box'id     = BoxId "box-3"
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

testTypeCheckCase :: [Char] -> CoreProg -> TestTree
testTypeCheckCase testName prog =
  testCase testName $ do
    let tc = typeCheck mempty prog
    mapM_ (T.putStrLn . renderText) tc
    Nothing @=? tc

testProg :: String -> Prim -> CoreProg -> TestTree
testProg name res prog = testGroup name
  [ testTypeCheckCase "typecheck" prog
  , testCase          "simple"    $ EvalPrim res  @=? evalProg txEnv prog
  ]

progGetHeight :: CoreProg
progGetHeight = mainProg $ Typed getHeight intT

progGetSelfId :: CoreProg
progGetSelfId = mainProg $ Typed (getBoxId getSelf) bytesT

progGetSelfScript :: CoreProg
progGetSelfScript = mainProg $ Typed (getBoxScript getSelf) bytesT

progGetTxArg :: CoreProg
progGetTxArg = mainProg $ Typed (listAt intT getIntArgs (int 1)) intT

progGetInputId :: CoreProg
progGetInputId = mainProg $ Typed (getBoxId $ listAt boxT getInputs (int 0)) bytesT

progGetOutputId :: CoreProg
progGetOutputId = mainProg $ Typed (getBoxId $ listAt boxT getOutputs (int 0)) bytesT

progGetOutputLastIntArg :: CoreProg
progGetOutputLastIntArg = mainProg $
  Typed (listAt intT (getBoxIntArgs $ listAt boxT getOutputs (int 0)) (int 1)) intT

progGetInputLastTextArg :: CoreProg
progGetInputLastTextArg = mainProg $
  Typed (listAt textT (getBoxTextArgs $ listAt boxT getInputs (int 1)) (int 1)) textT

mainProg :: Typed ExprCore -> CoreProg
mainProg expr = CoreProg [mkMain expr]
