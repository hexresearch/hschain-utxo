module Main where

import System.IO

import Hschain.Utxo.Pow.App

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  runApp

