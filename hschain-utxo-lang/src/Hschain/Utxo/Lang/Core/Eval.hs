-- | Evaluation of transaction
module Hschain.Utxo.Lang.Core.Eval(
  evalTx
) where

import Data.Fix
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.Prog
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types

import qualified Data.Vector as V

-- | Evaluates transaction. TxArg is a transaction augmented with current blockchain state.
evalTx :: TxArg -> Either Error (Sigma PublicKey)
evalTx tx@TxArg{..} = fmap joinSigma $ mapM (evalBox tx) txArg'inputs

evalBox  :: TxArg -> Box -> Either Error (Sigma PublicKey)
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

joinSigma :: Vector (Sigma PublicKey) -> Sigma PublicKey
joinSigma vs
  | null qs        = Fix $ SigmaBool True
  | any isFalse qs = Fix $ SigmaBool False
  | otherwise      = Fix $ SigmaAnd qs
  where
    isTrue a  = a == (Fix $ SigmaBool True)
    isFalse a = a == (Fix $ SigmaBool False)

    qs = filter (not . isTrue) $ V.toList vs

