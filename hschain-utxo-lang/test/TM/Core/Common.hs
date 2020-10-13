-- |
module TM.Core.Common
  ( env
  , testProgram
  , mkBoxInput
  ) where

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
  , inputEnv'inputs   = mempty
  , inputEnv'outputs  = mempty
  , inputEnv'args     = mempty
  }

mkBoxInput :: BoxId -> Box -> BoxInput
mkBoxInput bid box = BoxInput
  { boxInput'id      = bid
  , boxInput'box     = box
  , boxInput'args    = mempty
  , boxInput'proof   = Nothing
  , boxInput'sigMask = SigAll
  , boxInput'sigMsg  = SigMessage (hash ())
  }

testProgram :: String -> ExprCore -> Prim -> TestTree
testProgram nm prog res = testGroup nm
  [ testCase "typecheck" $ case typeCheck prog of
      Left  e -> assertFailure $ show e
      Right _ -> pure ()
  , testCase "simple"    $ EvalPrim res  @=? evalProg env prog
  ]
