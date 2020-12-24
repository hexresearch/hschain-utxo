module Main where

import Test.Hspec
import Hschain.Utxo.Test.Client.Proc
import Hschain.Utxo.Test.Client.Scripts.SimpleExchange
import Hschain.Utxo.Test.Client.Scripts.AtomicSwap
import Hschain.Utxo.Test.Client.Scripts.MultiSig
import Hschain.Utxo.Test.Client.Scripts.Channel
import Hschain.Utxo.Test.Client.Scripts.Lightning.Example

main :: IO ()
main = do
  exchangeTest   <- runTestProc simpleExchange
  atomicSwapTest <- runTestProc atomicSwap
  multiSigTest   <- runTestProc multiSigExchange
  channelTest1   <- runTestProc channelExchange
  channelTest2   <- runTestProc channelExchangeUnfair
  lightningTest  <- runTestProc lightningExample
  hspec $ do
    exchangeTest
    atomicSwapTest
    multiSigTest
    channelTest1
    channelTest2
    lightningTest

