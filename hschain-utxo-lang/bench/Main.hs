import Gauge.Main

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine
import Examples.SKI

main :: IO ()
main = defaultMain
  [ bench "SKK3.typecheck" $ nf (typeCheck preludeTypeContext) exampleSKK3
  ]
