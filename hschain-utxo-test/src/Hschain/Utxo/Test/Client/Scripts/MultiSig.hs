module Hschain.Utxo.Test.Client.Scripts.MultiSig(
  multiSigExchange
) where

import Hschain.Utxo.Test.Client.Wallet

import Hschain.Utxo.Test.Client.Monad
import Hschain.Utxo.Test.Client.Scripts.Utils
import Hschain.Utxo.Lang

-- | Simple exchange test scenario. Master starts with many coins and then
-- sends 10 coins to Bob and Alice. Then Bob and Alice exchange coins.
-- Alice sends 1 coin to Bob and Bob sends 2 coins to Alice.
multiSigExchange :: App ()
multiSigExchange = do
  testTitle "Multisig exchange. Alice and Bob create shared box that is protected by both of their keys."
  Scene{..} <- initUsers
  let alice     = user'wallet scene'alice
      bob       = user'wallet scene'bob
      Just aliceBox1 = user'box scene'alice
      Just bobBox1   = user'box scene'bob

  SendResult _ bobBox2 _  <- debugSend True "Alice sends 1 to bob" alice  aliceBox1  bob   1
  bobBalance <- getBoxBalance bobBox2
  testCase "Bob balance becomes 1" $ bobBalance == Just 1
  logTest "Tries to run invalid Tx. It should be rejected (Sends 2 coins from the box with 1 coin)"
  SendResult _bobBox3 _aliceBox3 _  <- debugSend False "Bob sends 2 to alice (fail case)"     bob    bobBox2       alice 2
  logTest "Now we can do it right. It sends from the box with 10 coins."
  SendResult _bobBox4 aliceBox4 _  <- debugSend True "Bob sends 2 to alice"     bob    bobBox1       alice 2
  aliceBalance <- getBoxBalance aliceBox4
  testCase "Alice balance becomes 2" $ aliceBalance == Just 2
  return ()

getSharedBoxTx :: ProofEnv -> ProofEnv -> (Int, Int) -> (Int, Int) -> BoxId -> BoxId -> IO Tx
getSharedBoxTx aliceEnv bobEnv (aliceValue, aliceChange) (bobValue, bobChange) aliceBox bobBox = undefined
  where
    tx =

