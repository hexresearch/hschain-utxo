module Hschain.Utxo.Test.Client.Scripts.SimpleExchange where

import Control.Timeout

import Text.Show.Pretty

import Hschain.Utxo.Lang
import Hschain.Utxo.Test.Client.Proc
import Hschain.Utxo.Test.Client.Wallet

import qualified Hschain.Utxo.API.Client as C

import Hschain.Utxo.Test.Client.Scripts.Utils

-- | Simple exchange test scenario. Master starts with many coins and then
-- sends 10 coins to Bob and Alice. Then Bob and Alice exchange coins.
-- Alice sends 1 coin to Bob and Bob sends 2 coins to Alice.
simpleExchange :: C.ClientSpec -> IO ()
simpleExchange client = do
  Scene{..} <- initUsers client
  let alice     = user'wallet scene'alice
      bob       = user'wallet scene'bob
      aliceBox1 = user'box scene'alice
      bobBox1   = user'box scene'bob
  SendResult aliceBox2  bobBox2   <- debugSend "Alice sends 1 to bob"     alice  aliceBox1     bob   1
  putStrLn "Tries to run invalid Tx. It should be rejected (Sends 2 coins from the box with 1 coin)"
  SendResult bobBox3 aliceBox3    <- debugSend "Bob sends 2 to alice"     bob    bobBox2       alice 2
  putStrLn "Now we can do it right. It sends from the box with 10 coins."
  SendResult bobBox4 aliceBox4    <- debugSend "Bob sends 2 to alice"     bob    bobBox1       alice 2
  return ()

