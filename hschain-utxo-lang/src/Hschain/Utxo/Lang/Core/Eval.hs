-- | Evaluation of transaction
module Hschain.Utxo.Lang.Core.Eval(
    evalToSigma
  , evalProveTx
) where

import Data.Fix
import Data.Text
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.Prog
import Hschain.Utxo.Lang.Expr hiding (SigmaExpr(..))
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types

import qualified Data.Vector as V

-- | Evaluates transaction. TxArg is a transaction augmented with current blockchain state.
evalToSigma :: TxArg -> Either Error BoolExprResult
evalToSigma tx@TxArg{..} = fmap joinSigma $ mapM (evalBox tx) txArg'inputs

evalBox  :: TxArg -> Box -> Either Error (Sigma PublicKey)
evalBox tx box@Box{..} = case coreProgFromScript box'script of
  Just prog -> execScriptToSigma (getScriptEnv tx box) prog
  Nothing   -> Left $ ExecError FailedToDecodeScript

getScriptEnv :: TxArg -> Box -> TxEnv
getScriptEnv TxArg{..} box = TxEnv
  { txEnv'self    = box
  , txEnv'height  = env'height txArg'env
  , txEnv'inputs  = txArg'inputs
  , txEnv'outputs = txArg'outputs
  , txEnv'args    = txArg'args
  }

joinSigma :: Vector (Sigma PublicKey) -> BoolExprResult
joinSigma vs = either ConstBool SigmaResult $ eliminateSigmaBool $ Fix $ SigmaAnd $ V.toList vs

-- | We verify that expression is evaluated to the sigma-value that is
-- supplied by the proposer and then verify the proof itself.
evalProveTx :: TxArg -> (Bool, Text)
evalProveTx tx
  | txPreservesValue tx = case res of
        Right (SigmaResult sigmaWithBools) -> case eliminateSigmaBool sigmaWithBools of
          Right sigma -> maybe (False, "No proof submitted") (\proof -> (equalSigmaProof sigma proof && verifyProof proof (txArg'txBytes tx), debug)) mProof
          Left bool   -> (bool, "")
        Right (ConstBool bool)  -> (bool, "")
        Left err    -> (False, renderText err)
  | otherwise = (False, "Sum of inputs does not equal to sum of outputs")
  where
    res = evalToSigma tx
    -- todo: implement debug for core
    debug = ""
    mProof = txArg'proof tx

