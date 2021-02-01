-- | Scripts with imports and exports
module TM.Lang.Imports(
  tests
) where

import Hschain.Utxo.Lang
import Test.Tasty
import TM.Core.Common (testScript)

tests :: TestTree
tests = testGroup "Scripts with imports"
  [ testScript "imports"    importScript
  ]

importScript :: Module
importScript = [utxoModule|
module Main(
    main
  , User(..)
  , Color(Blue)
  , module A
) where

import A
import B(f, g)
import qualified C as R
import D hiding (f)

data User = User Text Int
data Color = Red | Green | Blue

main = True
|]

