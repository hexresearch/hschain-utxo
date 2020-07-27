-- | Defines state transitions for blockchain
module Hschain.Utxo.State.React(
    react
  , execInBoxChain
  , applyTxs
) where

import Control.Monad

import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Core.Compile.Expr (coreProgFromScript)
import Hschain.Utxo.State.Types

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Core.Eval as Core

-- | React to single input transaction.
-- It updates blockchain or reports error on commit of the transaction.
-- Also it returns the text that contains debug-log for transaction execution.
react :: Tx -> BoxChain -> (Either Text BoxChain, Text)
react tx bch
  | isValid   = (Right $ updateBoxChain tx bch, debugMsg)
  | otherwise = (Left "Tx is invalid", debugMsg)
  where
    (isValid, debugMsg) = case toTxArg bch tx of
        Right txArg -> checkTxArg txArg
        Left err    -> (False, err)

    checkTxArg txArg
      | isValidTx           = (inputsAreValid, debugMsgInputs)
      | not inputsAreValid  = (False, "Tx inputs are invalid")
      | not outputsAreValid = (False, mconcat ["Tx output is invalid: ", fromMaybe "" mInvalidOutputId])
      | otherwise           = (False, "TX is invalid")
      where
        isValidTx = inputsAreValid && outputsAreValid

        (inputsAreValid, debugMsgInputs) = Core.evalProveTx txArg
        -- todo: check here that script evaluates to boolean with type checker.
        --       for now we check only that it parses
        mInvalidOutput = L.find (isNothing . coreProgFromScript . box'script) $ checkOutputTxArg txArg
        mInvalidOutputId = fmap (unBoxId . box'id) mInvalidOutput

        outputsAreValid = isNothing mInvalidOutput

updateBoxChain :: Tx -> BoxChain -> BoxChain
updateBoxChain Tx{..} = incrementHeight . insertOutputs . removeInputs
  where
    removeInputs = updateBoxes $ appEndo (foldMap (Endo . M.delete) tx'inputs)

    insertOutputs = updateBoxes $ appEndo (foldMap (\box -> Endo $ M.insert (box'id box) box) tx'outputs)

    updateBoxes f bch@BoxChain{..} = bch { boxChain'boxes = f boxChain'boxes }

    incrementHeight bch@BoxChain{..} = bch { boxChain'height = 1 + boxChain'height }


-- | Run transaction in the current state of blockchain
-- to get the sigma-expression of the evaluation of the transaction script.
--
-- Also it returns debug-log for transaction execution.
execInBoxChain :: Tx -> BoxChain -> (Either Text BoolExprResult, Text)
execInBoxChain tx bch = case toTxArg bch tx of
  Right txArg ->  (either (Left . renderText) Right $ Core.evalToSigma txArg, fakeDebug)
  Left err    -> (Left err, "No message")
  where
    -- | TODO: implement debug in core
    fakeDebug = "no-debug"

-- | We move outputs to inputs to check that expressions of outputs
-- are all valid and produce sigma expressions or booleans.
checkOutputTxArg :: TxArg -> [Box]
checkOutputTxArg TxArg{..} = V.toList txArg'outputs

-- | Applies list of transactions to blockchain.
applyTxs :: [Tx] -> BoxChain -> Either Text BoxChain
applyTxs txs = foldl (>=>) pure $ fmap (fmap fst . react) txs

