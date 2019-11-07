module Main where

import qualified Hschain.Utxo.API.Client as C
import Hschain.Utxo.Test.Client.Scripts.PayForCofee

client = C.ClientSpec "127.0.0.1" 8181 False

main :: IO ()
main = payForCofeeAlice client

