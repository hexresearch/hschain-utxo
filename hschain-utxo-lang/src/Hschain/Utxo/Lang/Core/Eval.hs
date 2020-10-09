-- | Evaluation of transaction
module Hschain.Utxo.Lang.Core.Eval(
    evalToSigma
  , evalProveTx
) where

import Control.Monad
import Data.Fix
import Data.Text
import Data.Bifunctor
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

verifyInput :: TxArg -> BoxInput -> Either Text ()
verifyInput txArg input@BoxInput{..} = do
  r <- first renderText
     $ evalInput
     $ getInputEnv txArg input
  case r of
    -- Script evaluated to literal Bool
    ConstBool True  -> pure ()
    ConstBool False -> false
    SigmaResult sigma -> case sigma of
      Fix (SigmaBool True)  -> pure ()
      Fix (SigmaBool False) -> false
      -- Attempt to prove sigma expression
      _ | Just proof <- boxInput'proof
        , equalSigmaProof sigma proof
        , verifyProof proof boxInput'sigMsg
          -> pure ()
      -- Otherwise failure
      _ -> Left "Sigma expression proof is not valid"
  where
    false = Left "Script evaluated to False"

-- | We verify that expression is evaluated to the sigma-value that is
-- supplied by the proposer and then verify the proof itself.
evalProveTx :: TxArg -> Either Text ()
evalProveTx tx = do
  unless (txPreservesValue tx) $
    Left "Sum of inputs does not equal to sum of outputs"
  mapM_ (verifyInput tx) (txArg'inputs tx)

