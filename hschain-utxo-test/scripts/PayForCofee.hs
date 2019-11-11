module Main where

import Test.Hspec
import Control.Timeout

import Hschain.Utxo.Test.Client.Proc
import Hschain.Utxo.Test.Client.Scripts.PayForCofee

main :: IO ()
main = do
  t1 <- runTestProc payForCofeeBob
  t2 <- runTestProc payForCofeeAlice
  hspec $ t1 >> t2

