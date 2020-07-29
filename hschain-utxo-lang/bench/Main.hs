{-# LANGUAGE OverloadedStrings #-}
import Gauge.Main

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Gmachine
import Hschain.Utxo.Lang.Core.RefEval
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Expr
import Examples.SKI
import Examples.Simple

main :: IO ()
main = defaultMain
  [ benchProgram "SKK3"    exampleSKK3
  , benchProgram "Additon" progAddition
  ]

benchProgram :: String -> CoreProg -> Benchmark
benchProgram name prog = bgroup name
  [ bench "typeCheck"      $ nf (typeCheck preludeTypeContext) prog
  , bench "compile"        $ nf compile prog
  , bench "eval.GMachine"  $ nf (fmap gmachine'output . eval . compile) prog
  , bench "eval.reference" $ nf (evalProg inputenv) prog
  ]

inputenv :: InputEnv
inputenv = InputEnv
  { inputEnv'height   = 123
  , inputEnv'self     = Box
    { box'id     = BoxId ""
    , box'value  = 100
    , box'script = Script ""
    , box'args   = mempty
    }
  , inputEnv'inputs   = mempty
  , inputEnv'outputs  = mempty
  , inputEnv'args     = mempty
  }

