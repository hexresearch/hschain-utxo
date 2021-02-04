-- | Evaluation of transaction
module Hschain.Utxo.Lang.Core.Eval(
    evalToSigma
  , evalProveTx
  , execScriptToSigma
) where

import Data.Text
import Data.Bifunctor
import Data.Void
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (typeCheck)
import Hschain.Utxo.Lang.Core.RefEval           (evalProg)
import Hschain.Utxo.Lang.Core.Types             (TypeCore(..),Prim(..))
import Hschain.Utxo.Lang.Expr                   (ScriptEvalResult(..))
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types

import qualified Data.Text as T

-- | Executes spend-script in transaction. Spend script should be
--   well-typed and evaluate to either sigma-expression or boolean.
execScriptToSigma :: InputEnv -> Core Void -> Either Error ScriptEvalResult
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
    Right (PrimVal (PrimBool  b)) -> pure $ ConstBool b
    Right (PrimVal (PrimSigma s)) -> case eliminateSigmaBool s of
      Left  b  -> pure $ ConstBool   b
      Right s' -> pure $ SigmaResult s'
    Left _ -> Right $ ConstBool False
    _ -> error "Internal error:  Left $ E.CoreScriptError E.ResultIsNotSigma"


evalToSigma :: TxArg -> Either Error (Vector ScriptEvalResult)
evalToSigma tx = mapM (evalInput . getInputEnv tx) $ txArg'inputs tx

evalInput :: InputEnv -> Either Error ScriptEvalResult
evalInput env =
  case coreProgFromScript $ box'script $ postBox'content $ boxInput'box $ inputEnv'self env of
    Just prog -> execScriptToSigma env prog
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
    SigmaResult sigma
      | Just proof <- boxInput'proof
      , equalSigmaProof sigma proof
      , verifyProof proof boxInput'sigMsg
        -> pure ()
      | otherwise -> Left $ T.unlines ["Sigma expression proof is not valid"]
  where
    false = Left "Script evaluated to False"

-- | We verify that expression is evaluated to the sigma-value that is
-- supplied by the proposer and then verify the proof itself.
evalProveTx :: TxArg -> Either Text ()
evalProveTx tx = mapM_ (verifyInput tx) (txArg'inputs tx)

