module Hschain.Utxo.Test.Client.Scripts.ReversibleAddress(

) where

import Prelude hiding ((<*))

import Data.Fix
import Data.Text (Text)

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

bobDeadline :: Text
bobDeadline = "bob-deadline"

bobPubKey :: Text
bobPubKey = "bob-public-key"

getBobDeadline :: Expr Box -> Expr Int
getBobDeadline box = getBoxArg box (text bobDeadline)

getBobPubKey :: Expr Box -> Expr Text
getBobPubKey box = getBoxArg box (text bobPubKey)

withdrawScript :: Expr Text -> Expr Bool
withdrawScript carol =
  "bob"         =: getBobPubKey getSelf    $ \bob         ->
  "bobDeadline" =: getBobDeadline getSelf  $ \bobDeadline ->
  (pk bob &&* getHeight >* bobDeadline) ||* (pk carol &&* getHeight <=* bobDeadline)


reversibleAddressScript blocksIn24h carol feeProposition maxFee =
  "isChange"   =: (lam "out" $ \out -> getBoxScript out ==* getBoxScript getSelf) $ \isChange ->
  "isWithdraw" =: (lam "out" $ \out ->
                              getBobDeadline out >=* getHeight + blocksIn24h
                          &&* getBoxScript out ==* (toScriptBytes $ withdrawScript carol)
                      ) $ \isWithdraw ->
  "isFee"      =: (lam "out" $ \out ->
                        getBoxScript out ==* (toScriptBytes feeProposition)
                      ) $ \isFee ->
  "isValid"    =: (lam "out" $ \out ->
                        app isChange out ||* app isWithdraw out ||* app isFee out
                      ) $ \isValid ->
  "totalFee"   =: (foldVec (lam2 "x" "b" $ \x b -> ifB (app isFee b) (x + getBoxValue b) x) 0 getOutputs) $ \totalFee ->
  pk "alice" &&* allVec (mapVec isValid getOutputs) &&* (totalFee <* maxFee)

