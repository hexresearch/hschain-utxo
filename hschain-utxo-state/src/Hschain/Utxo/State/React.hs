module Hschain.Utxo.State.React(
    react
  , execInBoxChain
) where

import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

react :: Tx -> BoxChain -> (Either Text (TxHash, BoxChain), Text)
react tx bch
  | isValid   = (Right $ appendHash $ updateBoxChain tx bch, debugMsg)
  | otherwise = (Left "Tx is invalid", debugMsg)
  where
    (isValid, debugMsg) = case toTxArg bch tx of
        Right txArg -> checkTxArg txArg
        Left err    -> (False, err)

    checkTxArg txArg
      | isValidTx           = (inputsAreValid, debugMsgInputs)
      | not inputsAreValid  = (False, "Tx inputs are invalid")
      | not outputsAreValid = (False, mconcat ["Tx output is invalid: ", fromMaybe "" mInvalidOutputId])
      where
        isValidTx = inputsAreValid && outputsAreValid

        (inputsAreValid, debugMsgInputs) = exec txArg
        mInvalidOutput = L.find (isLeft . fst . execToSigma) $ checkOutputTxArg txArg
        mInvalidOutputId = (\TxArg{..} -> fmap (\x -> unBoxId $ box'id x) $ txArg'inputs V.!? 0) =<< mInvalidOutput

        outputsAreValid = isNothing mInvalidOutput

    -- todo: replace me with real hash
    fakeHash = TxHash . mappend "tx-" . T.pack . show . boxChain'height

    appendHash x = (fakeHash x, x)

updateBoxChain :: Tx -> BoxChain -> BoxChain
updateBoxChain Tx{..} = incrementHeight . insertOutputs . removeInputs
  where
    removeInputs = updateBoxes $ appEndo (foldMap (Endo . M.delete) tx'inputs)

    insertOutputs = updateBoxes $ appEndo (foldMap (\box -> Endo $ M.insert (box'id box) box) tx'outputs)

    updateBoxes f bch@BoxChain{..} = bch { boxChain'boxes = f boxChain'boxes }

    incrementHeight bch@BoxChain{..} = bch { boxChain'height = 1 + boxChain'height }


execInBoxChain :: Tx -> BoxChain -> (Either Text BoolExprResult, Text)
execInBoxChain tx bch = case toTxArg bch tx of
  Right txArg -> execToSigma txArg
  Left err    -> (Left err, "No message")

-- | We move outputs to inputs to check that expressions of outputs
-- are all valid and produce sigma expressions or booleans.
checkOutputTxArg :: TxArg -> [TxArg]
checkOutputTxArg tx@TxArg{..} = V.toList $ fmap subst txArg'outputs
  where
    subst x = tx
      { txArg'inputs  = V.singleton x
      , txArg'outputs = V.empty
      }


