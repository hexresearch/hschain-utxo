module Main where

import Test.Hspec
import Hschain.Utxo.Test.Client.Proc
import Hschain.Utxo.Test.Client.Scripts.XorGame

main :: IO ()
main = hspec =<< runTestProc xorGame


