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

evalToSigma :: TxArg -> Either Error (Vector BoolExprResult)
evalToSigma tx = mapM (evalInput . getInputEnv tx) $ txArg'inputs tx

evalInput :: InputEnv -> Either Error BoolExprResult
evalInput env =
  case coreProgFromScript $ box'script $ inputEnv'self env of
    Just prog -> fmap (either ConstBool SigmaResult . eliminateSigmaBool) $ execScriptToSigma env prog
    Nothing   -> Left $ ExecError FailedToDecodeScript

verifyInput :: TxArg -> BoxInput -> Either Error Bool
verifyInput txArg input@BoxInput{..} = fmap verifyResult $ evalInput $ getInputEnv txArg input
  where
    verifyResult = \case
      ConstBool b       -> b
      SigmaResult sigma -> case sigma of
        Fix (SigmaBool b) -> b
        _ | Just proof <- boxInput'proof
          , equalSigmaProof sigma proof
          , verifyProof proof boxInput'sigMsg
            -> True
        _   -> False

-- | We verify that expression is evaluated to the sigma-value that is
-- supplied by the proposer and then verify the proof itself.
evalProveTx :: TxArg -> (Bool, Text)
evalProveTx tx
  | txPreservesValue tx =
      case mapM (verifyInput tx) $ txArg'inputs tx of
        Right bs  -> (and bs, debug)
        Left err  -> (False, renderText err)
  | otherwise = (False, "Sum of inputs does not equal to sum of outputs")
  where
    -- todo: implement debug for core
    debug   = ""

