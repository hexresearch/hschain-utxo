module TM.Lang.Monomorphic(
  mkScript
) where

import Data.Int
import Hschain.Utxo.Lang

mkScript :: IO Module
mkScript = do
  senderPk   <- fmap getPublicKey newSecret
  receiverPk <- fmap getPublicKey newSecret
  let spendHeight = 10
  return $ coffeeScript spendHeight senderPk receiverPk


coffeeScript :: Int64 -> PublicKey -> PublicKey -> Module
coffeeScript spendHeight senderPk receiverPk = [utxoModule|

    receiverScript = pk $(receiverPk) &&* ($(spendHeight) <*  getHeight)
    refundScript   = pk $(senderPk)   &&* ($(spendHeight) >=* getHeight)

    main = receiverScript ||* refundScript
|]
