-- |
module TM.Core.Common
  ( env
  , testProgram
  , testProgramBy
  , testProgramFail
  , mkBoxInput
  , mkBoxOutput
  ) where

import Data.Default
import Data.Int

import Test.Tasty
import Test.Tasty.HUnit
import HSChain.Crypto (hash)
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.RefEval
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Types

env :: InputEnv
env = InputEnv
  { inputEnv'height   = 123
  , inputEnv'self     = mkBoxInput (BoxId $ hash ()) Box
      { box'value  = 100
      , box'script = Script ""
      , box'args   = mempty
      }
  , inputEnv'inputs     = mempty
  , inputEnv'outputs    = mempty
  , inputEnv'dataInputs = mempty
  , inputEnv'args       = mempty
  , inputEnv'sigs       = mempty
  , inputEnv'sigMsg     = def
  }

mkBoxInput :: BoxId -> Box -> BoxInput
mkBoxInput bid box = BoxInput
  { boxInput'id      = bid
  , boxInput'box     = PostBox
                          { postBox'content = box
                          , postBox'height  = 1
                          }
  , boxInput'args    = mempty
  , boxInput'proof   = Nothing
  , boxInput'sigs    = mempty
  , boxInput'sigMask = SigAll
  , boxInput'sigMsg  = SigMessage (hash ())
  }

mkBoxOutput :: Int64 -> BoxId -> Box -> BoxOutput
mkBoxOutput height bid box = BoxOutput
  { boxOutput'id  = bid
  , boxOutput'box = PostBox
                        { postBox'content = box
                        , postBox'height  = height
                        }
  }

testProgram :: String -> ExprCore -> Prim -> TestTree
testProgram nm prog res = testGroup nm
  [ testCase "typecheck" $ case typeCheck prog of
      Left  e -> assertFailure $ show e
      Right _ -> pure ()
  , testCase "simple"    $ EvalPrim res  @=? evalProg env prog
  ]

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

testProgramFail :: String -> ExprCore -> TestTree
testProgramFail nm prog = testProgramBy nm prog (Left ())
