-- | Evaluation of transaction
module Hschain.Utxo.Lang.Core.Eval(
    evalToSigma
  , evalProveTx
) where

import Data.ByteString (ByteString)
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
evalToSigma tx = mapM (evalInput . snd) $ splitInputs tx

evalInput :: InputEnv -> Either Error BoolExprResult
evalInput env =
  case coreProgFromScript $ box'script $ inputEnv'self env of
    Just prog -> fmap (either ConstBool SigmaResult . eliminateSigmaBool) $ execScriptToSigma env prog
    Nothing   -> Left $ ExecError FailedToDecodeScript

verifyInput :: ByteString -> Proof -> InputEnv -> Either Error Bool
verifyInput message proof env = fmap verifyResult $ evalInput env
  where
    verifyResult = \case
      ConstBool b       -> b
      SigmaResult sigma -> equalSigmaProof sigma proof && verifyProof proof message

getInputEnv :: TxArg -> BoxInput -> InputEnv
getInputEnv TxArg{..} input = InputEnv
  { inputEnv'self    = boxInput'box input
  , inputEnv'height  = env'height txArg'env
  , inputEnv'inputs  = fmap boxInput'box txArg'inputs
  , inputEnv'outputs = txArg'outputs
  , inputEnv'args    = boxInput'args input
  }

splitInputs :: TxArg -> Vector (Proof, InputEnv)
splitInputs tx = fmap (\input -> (boxInput'proof input, getInputEnv tx input)) $ txArg'inputs tx

-- | We verify that expression is evaluated to the sigma-value that is
-- supplied by the proposer and then verify the proof itself.
evalProveTx :: TxArg -> (Bool, Text)
evalProveTx tx
  | txPreservesValue tx =
      case mapM (uncurry $ verifyInput message) $ splitInputs tx of
        Right bs  -> (and bs, debug)
        Left err  -> (False, renderText err)
  | otherwise = (False, "Sum of inputs does not equal to sum of outputs")
  where
    message = txArg'txBytes tx
    -- todo: implement debug for core
    debug   = ""

