-- |
module Main where

import Test.Tasty
import qualified TM.Infer

main :: IO ()
main = defaultMain $ testGroup "HM"
  [ TM.Infer.tests
  ]
