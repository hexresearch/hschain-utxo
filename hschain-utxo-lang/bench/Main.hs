import Gauge.Main

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine
import Examples.SKI

main :: IO ()
main = defaultMain
  [ bench "SKK3.typecheck" $ nf (typeCheck preludeTypeContext) exampleSKK3
  , bench "SKK3.compile"   $ nf compile exampleSKK3
  , bench "SKK3.eval"      $ nf (fmap gmachine'output . eval . compile) exampleSKK3
  ]
