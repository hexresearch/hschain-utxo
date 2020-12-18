module Hschain.Utxo.Test.Client.Scripts.ReversibleAddress(
  reversibleAddressScript
) where

import Data.ByteString (ByteString)
import Hschain.Utxo.Lang

withdrawScript :: ByteString -> Script
withdrawScript carol = [utxo|

  main =  (pk bob &&* (getHeight >* bobDeadline))
      ||* (pk $(carol) &&* (getHeight <=* bobDeadline))
    where
      bob         = fst (getBoxArgs getSelf)
      bobDeadline = snd (getBoxArgs getSelf)
|]

reversibleAddressScript :: Int -> ByteString -> ByteString -> Script -> Int -> Script
reversibleAddressScript blocksIn24h alice carol feeProposition maxFee = [utxo|

  getBobDeadline box = getBoxArgs box

  isChange   out = getBoxScript out == getBoxScript getSelf
  isWithdraw out = getBobDeadline out >= (getHeight + $(blocksIn24h))
                 && getBoxScript out == $(withdrawProg)
  isFee out = getBoxScript out == $(feeProposition)
  isValid out = isChange out || isWithdraw out || isFee out
  totalFee = foldl (\x b -> if (isFee b) then (x + getBoxValue b) else x) 0 getOutputs

  main = pk $(alice) &&* toSigma (all isValid getOutputs) &&* (totalFee <* $(maxFee))

|]
  where
    withdrawProg = withdrawScript carol

