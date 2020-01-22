module Hschain.Utxo.State.React(
    react
  , execInBoxChain
) where

import Data.Monoid
import Data.Text (Text)

import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types

import qualified Data.Map.Strict as M
import qualified Data.Text as T

react :: Tx -> BoxChain -> (Either Text (TxHash, BoxChain), Text)
react tx bch
  | isValid   = (Right $ appendHash $ updateBoxChain tx bch, debugMsg)
  | otherwise = (Left "Tx is invalid", debugMsg)
  where
    (isValid, debugMsg) = case toTxArg bch tx of
        Right txArg -> exec txArg
        Left err    -> (False, err)

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

