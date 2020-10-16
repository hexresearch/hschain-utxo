-- | minimal lightning implementation to see
-- what primitives we need and do we cover this case
module Hschain.Utxo.Test.Client.Scripts.Lightning.Example(
  lightningExample
) where

import Hschain.Utxo.Lang

import Hschain.Utxo.Test.Client.Scripts.Lightning.Network
import Hschain.Utxo.Test.Client.Scripts.Lightning.Protocol

lightningExample :: IO ()
lightningExample = do
  net <- newNetwork
  alice <- registerUser net (UserId "alice") =<< newUser
  bob   <- registerUser net (UserId "bob")   =<< newUser
  john  <- registerUser net (UserId "john")  =<< newUser
  ch1 <- openChan alice john 10
  ch2 <- openChan bob   john 10
  send alice bob 2
  send bob   alice 1
  closeChan ch1 alice john
  closeChan ch2 bob   john
  closeNetwork net

newUser :: IO PublicKey
newUser = fmap getPublicKey newSecret

