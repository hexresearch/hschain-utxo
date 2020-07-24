import Gauge.Main

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Gmachine
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
  ]
