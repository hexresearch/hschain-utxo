module Hschain.Utxo.Test.Client.Scripts.SimpleExchange where

import Control.Timeout

import Text.Show.Pretty

import Hschain.Utxo.Lang
import Hschain.Utxo.Test.Client.Proc
import Hschain.Utxo.Test.Client.Wallet

import qualified Hschain.Utxo.API.Client as C

import Hschain.Utxo.Test.Client.Monad
import Hschain.Utxo.Test.Client.Scripts.Utils

-- | Simple exchange test scenario. Master starts with many coins and then
-- sends 10 coins to Bob and Alice. Then Bob and Alice exchange coins.
-- Alice sends 1 coin to Bob and Bob sends 2 coins to Alice.
simpleExchange :: App ()
simpleExchange = do
  testTitle "Simple exchange. Alice and Bob echange funds."
  Scene{..} <- initUsers
  let alice     = user'wallet scene'alice
      bob       = user'wallet scene'bob
      aliceBox1 = user'box scene'alice
      bobBox1   = user'box scene'bob
  SendResult aliceBox2  bobBox2 _  <- debugSend True "Alice sends 1 to bob"     alice  aliceBox1     bob   1
  bobBalance <- getBoxBalance bobBox2
  testCase "Bob balance becomes 1" $ bobBalance == Just 1
  logTest "Tries to run invalid Tx. It should be rejected (Sends 2 coins from the box with 1 coin)"
  SendResult bobBox3 aliceBox3 _  <- debugSend False "Bob sends 2 to alice (fail case)"     bob    bobBox2       alice 2
  logTest "Now we can do it right. It sends from the box with 10 coins."
  SendResult bobBox4 aliceBox4 _  <- debugSend True "Bob sends 2 to alice"     bob    bobBox1       alice 2
  aliceBalance <- getBoxBalance aliceBox4
  testCase "Alice balance becomes 2" $ aliceBalance == Just 2
  return ()

