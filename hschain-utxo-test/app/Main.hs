module Main where

import Test.Hspec
import Hschain.Utxo.Test.Client.Proc
import Hschain.Utxo.Test.Client.Scripts.PayForCofee
import Hschain.Utxo.Test.Client.Scripts.SimpleExchange
import Hschain.Utxo.Test.Client.Scripts.XorGame
import Hschain.Utxo.Test.Client.Scripts.AtomicSwap
import Hschain.Utxo.Test.Client.Scripts.MultiSig
import Hschain.Utxo.Test.Client.Scripts.Channel

main :: IO ()
main = do
{-
  exchangeTest   <- runTestProc simpleExchange
  xorTest        <- runTestProc xorGame
  payTest1       <- runTestProc payForCofeeAlice
  payTest2       <- runTestProc payForCofeeBob
  atomicSwapTest <- runTestProc atomicSwap
  multiSigTest   <- runTestProc multiSigExchange
-}
  channelTest <- runTestProc channelExchange
  hspec $ do
{-    exchangeTest
    payTest1
    payTest2
    xorTest
    atomicSwapTest
    multiSigTest
-}
    channelTest
