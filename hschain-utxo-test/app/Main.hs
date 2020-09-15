module Main where

import Test.Hspec
import Hschain.Utxo.Test.Client.Proc
import Hschain.Utxo.Test.Client.Scripts.PayForCofee
import Hschain.Utxo.Test.Client.Scripts.SimpleExchange
import Hschain.Utxo.Test.Client.Scripts.XorGame
import Hschain.Utxo.Test.Client.Scripts.AtomicSwap

main :: IO ()
main = do
  exchangeTest   <- runTestProc simpleExchange
  xorTest        <- runTestProc xorGame
  payTest1       <- runTestProc payForCofeeAlice
  payTest2       <- runTestProc payForCofeeBob
  atomicSwapTest <- runTestProc atomicSwap
  hspec $ do
    exchangeTest
    payTest1
    payTest2
    xorTest
    atomicSwapTest
