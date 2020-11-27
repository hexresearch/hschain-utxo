-- | Evaluation of transaction
module Hschain.Utxo.Lang.Core.Eval(
    evalToSigma
  , evalProveTx
  , execScriptToSigma
) where

import Data.Fix
import Data.Text
import Data.Bifunctor
import Data.Void
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (typeCheck)
import Hschain.Utxo.Lang.Core.RefEval           (evalProg,EvalResult(..))
import Hschain.Utxo.Lang.Core.Types             (TypeCore(..),Prim(..))
import Hschain.Utxo.Lang.Expr                   (BoolExprResult(..))
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types


-- | Executes spend-script in transaction. Spend script should be
--   well-typed and evaluate to either sigma-expression or boolean.
execScriptToSigma :: InputEnv -> Core Void -> Either Error (Sigma PublicKey)
execScriptToSigma env prog = do
  -- Type check expression
  ty <- first (CoreScriptError . TypeCoreError)
      $ typeCheck prog
  case ty of
    SigmaT -> pure ()
    BoolT  -> pure ()
    _      -> Left $ CoreScriptError ResultIsNotSigma
  -- Evaluate script
  case evalProg env prog of
    EvalPrim (PrimBool  b) -> Right $ Fix $ SigmaBool b
    EvalPrim (PrimSigma s) -> case eliminateSigmaBool s of
      Left  b  -> Right $ Fix $ SigmaBool b
      Right s' -> Right   s'
    EvalFail _             -> Right $ Fix $ SigmaBool False
    _ -> error "Internal error:  Left $ E.CoreScriptError E.ResultIsNotSigma"


evalToSigma :: TxArg -> Either Error (Vector BoolExprResult)
evalToSigma tx = mapM (evalInput . getInputEnv tx) $ txArg'inputs tx

evalInput :: InputEnv -> Either Error BoolExprResult
evalInput env =
  case coreProgFromScript $ box'script $ postBox'content $ boxInput'box $ inputEnv'self env of
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
evalProveTx tx = mapM_ (verifyInput tx) (txArg'inputs tx)

