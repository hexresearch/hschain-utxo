-- | Evaluation of transaction
module Hschain.Utxo.Lang.Core.Eval(
  evalTx
) where

import Data.Vector (Vector)

import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.Prog
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Types

import qualified Data.Vector as V

-- | Evaluates transaction. TxArg is a transaction augmented with current blockchain state.
evalTx :: TxArg -> Either Error SigmaExpr
evalTx tx@TxArg{..} = fmap joinSigma $ mapM (evalBox tx) txArg'inputs

evalBox  :: TxArg -> Box -> Either Error SigmaExpr
evalBox tx box@Box{..} = case coreProgFromText $ unScript box'script of
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

joinSigma :: Vector SigmaExpr -> SigmaExpr
joinSigma vs
  | null qs        = SigmaBool True
  | any isFalse qs = SigmaBool False
  | otherwise      = SigmaAnd qs
  where
    isTrue a  = a == SigmaBool True
    isFalse a = a == SigmaBool False

    qs = filter (not . isTrue) $ V.toList vs

