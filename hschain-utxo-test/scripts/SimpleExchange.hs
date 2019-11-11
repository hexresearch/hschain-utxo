module Main where

import Test.Hspec
import Hschain.Utxo.Test.Client.Proc
import Hschain.Utxo.Test.Client.Scripts.SimpleExchange

main :: IO ()
main = hspec =<< runTestProc simpleExchange

