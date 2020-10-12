-- |
module TM.Core.Common
  ( env
  , testProgram
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Types (InputEnv(..),Box(..),BoxId(..),Script(..), PostBox(..))
import Hschain.Utxo.Lang.Core.RefEval
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Types

env :: InputEnv
env = InputEnv
  { inputEnv'height   = 123
  , inputEnv'self     = post $ Box
    { box'id     = BoxId $ hashBlob ""
    , box'value  = 100
    , box'script = Script ""
    , box'args   = mempty
    }
  , inputEnv'inputs   = mempty
  , inputEnv'outputs  = mempty
  , inputEnv'args     = mempty
  }
  where
    post box = PostBox box 1


testProgram :: String -> ExprCore -> Prim -> TestTree
testProgram nm prog res = testGroup nm
  [ testCase "typecheck" $ case typeCheck prog of
      Left  e -> assertFailure $ show e
      Right _ -> pure ()
  , testCase "simple"    $ EvalPrim res  @=? evalProg env prog
  ]
