-- | Defines state transitions for blockchain
module Hschain.Utxo.State.React(
    react
  , execInBoxChain
  , applyTxs
) where

import Control.Monad
import Control.Lens

import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Core.Compile.Expr (coreProgFromScript)
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (typeCheck)
import Hschain.Utxo.State.Types

import qualified Data.Map.Strict as M
import qualified Hschain.Utxo.Lang.Core.Eval as Core

-- | React to single input transaction.
-- It updates blockchain or reports error on commit of the transaction.
-- Also it returns the text that contains debug-log for transaction execution.
react :: Tx -> BoxChain -> Either Text BoxChain
react tx bch = do
  txArg <- toTxArg bch tx
  -- Inputs are valid
  evalProveTx txArg
  -- BoxId in output are valid
  unless (validateOutputBoxIds tx) $ Left "Invalid box ID"
  -- Spend scripts in outputs are decodable
  forM_ (txArg'outputs txArg) $ \Box{..} -> do
    case coreProgFromScript box'script of
      Nothing -> Left "Undecodable script"
      Just prog -> case typeCheck prog of
        Right SigmaT -> pure ()
        Right BoolT  -> pure ()
        Right _      -> Left "Invalid type in output script"
        Left  err    -> Left $ renderText err
  -- We're done
  return $ updateBoxChain tx bch

updateBoxChain :: Tx -> BoxChain -> BoxChain
updateBoxChain Tx{..} bch@BoxChain{..}
  = (boxChain'heightL %~ succ)
  $ insertOutputs
  $ removeInputs bch
  where
    removeInputs  = boxChain'boxesL %~ appEndo (foldMap (Endo . M.delete . boxInputRef'id) tx'inputs)
    insertOutputs = boxChain'boxesL %~ appEndo (foldMap (\box -> Endo $ M.insert (box'id box) (PostBox box height)) tx'outputs)

    height = succ boxChain'height



-- | Run transaction in the current state of blockchain
-- to get the sigma-expression of the evaluation of the transaction script.
--
-- Also it returns debug-log for transaction execution.
execInBoxChain :: Tx -> BoxChain -> Either Text (Vector BoolExprResult)
execInBoxChain tx bch = do
  txArg <- toTxArg bch tx
  either (Left . renderText) Right $ Core.evalToSigma txArg


-- | Applies list of transactions to blockchain.
applyTxs :: [Tx] -> BoxChain -> Either Text BoxChain
applyTxs = foldl (>=>) pure . fmap react

