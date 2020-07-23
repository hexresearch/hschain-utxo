module Hschain.Utxo.Test.Client.Scripts.ReversibleAddress(
  reversibleAddressScript
) where

import Prelude hiding ((<*))

import Data.Text (Text)

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

-- | Bob's id for deadline in the list of ints
bobDeadlineId :: Expr Int
bobDeadlineId = int 0

-- | Bob's id for public key in the list of text args
bobPubKeyId :: Expr Int
bobPubKeyId = int 0

getBobDeadline :: Expr Box -> Expr Int
getBobDeadline box = listAt (getBoxIntArgList box) bobDeadlineId

getBobPubKey :: Expr Box -> Expr Text
getBobPubKey box = listAt (getBoxTextArgList box) bobPubKeyId

withdrawScript :: Expr Text -> Expr SigmaBool
withdrawScript carol =
  "bob"         =: getBobPubKey getSelf    $ \bob         ->
  "bobDeadline" =: getBobDeadline getSelf  $ \bobDeadline ->
  (pk bob &&* toSigma (getHeight >* bobDeadline)) ||* (pk carol &&* (toSigma $ getHeight <=* bobDeadline))


reversibleAddressScript :: Expr Int -> Expr Text -> Expr SigmaBool -> Expr Int -> Expr SigmaBool
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
  pk "alice" &&* (toSigma $ allVec (mapVec isValid getOutputs) &&* (totalFee <* maxFee))





