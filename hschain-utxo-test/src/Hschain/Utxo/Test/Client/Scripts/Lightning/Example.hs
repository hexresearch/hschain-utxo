-- | minimal lightning implementation to see
-- what primitives we need and do we cover this case
module Hschain.Utxo.Test.Client.Scripts.Lightning.Example(
  lightningExample
) where

import Hex.Common.Delay (sleep)

import Hschain.Utxo.Test.Client.Monad
import Hschain.Utxo.Test.Client.Scripts.Utils (Scene(..))
import Hschain.Utxo.Test.Client.Scripts.Lightning.Network
import Hschain.Utxo.Test.Client.Scripts.Lightning.User
import Hschain.Utxo.Test.Client.Scripts.Lightning.Protocol
import qualified Hschain.Utxo.Test.Client.Scripts.Utils as Utils

lightningExample :: App ()
lightningExample = do
  testTitle "Example for lightning network echange."
  Scene{..} <- Utils.initUsers
  let aliceW         = Utils.user'wallet scene'alice
      bobW           = Utils.user'wallet scene'bob
      johnW          = Utils.user'wallet scene'john
      Just aliceBox1 = Utils.user'box scene'alice
      Just bobBox1   = Utils.user'box scene'bob
      Just johnBox1  = Utils.user'box scene'john
  net <- newNetwork
  alice <- registerUser net (UserId "alice") aliceW [aliceBox1]
  bob   <- registerUser net (UserId "bob")   bobW   [bobBox1]
  john  <- registerUser net (UserId "john")  johnW  [johnBox1]
  ch1 <- openChan alice john 5
  ch2 <- openChan john  bob  5
  testCase "First chan is open"  =<< waitForChanToOpen 10 ch1 alice john
  testCase "Second chan is open" =<< waitForChanToOpen 10 ch2 bob   john
  -- send 2 from alice to bob over john
  send alice bob =<< initTestRoute 2 [ch1, ch2]
  sleep 2
  closeChan ch1 alice john
  closeChan ch2 bob   john
  closeNetwork net
  showUser alice
  showUser bob
  showUser john

