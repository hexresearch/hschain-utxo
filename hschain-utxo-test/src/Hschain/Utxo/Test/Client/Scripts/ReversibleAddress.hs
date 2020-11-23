module Hschain.Utxo.Test.Client.Scripts.ReversibleAddress(
  reversibleAddressScript
) where

import Data.ByteString (ByteString)
import Hschain.Utxo.Lang

withdrawScript :: ByteString -> Script
withdrawScript carol = [utxo|

  bob         = listAt (getBoxBytesArgs getSelf) 0
  bobDeadline = listAt (getBoxIntArgs getSelf) 0

  main =  (pk bob &&* toSigma (getHeight > bobDeadline))
      ||* (pk $(carol) &&* toSigma (getHeight <= bobDeadline))

|]

reversibleAddressScript :: Int -> ByteString -> ByteString -> Script -> Int -> Script
reversibleAddressScript blocksIn24h alice carol feeProposition maxFee = [utxo|

  getBobDeadline box = listAt (getBoxIntArgs box) 0

  isChange   out = getBoxScript out == getBoxScript getSelf
  isWithdraw out = getBobDeadline out >= (getHeight + $(blocksIn24h))
                 && getBoxScript out == $(withdrawProg)
  isFee out = getBoxScript out == $(feeProposition)
  isValid out = isChange out || isWithdraw out || isFee out
  totalFee = foldl (\x b -> if (isFee b) then (x + getBoxValue b) else x) 0 getOutputs

  main = pk $(alice) &&* toSigma (all isValid getOutputs) &&* toSigma (totalFee < $(maxFee))

|]
  where
    withdrawProg = withdrawScript carol

