-- | Functions to compile core progrmamms to instructions of G-machine
module Hschain.Utxo.Lang.Core.Compile.Prog(
    execScriptToSigma
  ) where

import Data.Bifunctor (first)
import Data.Fix

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (InputEnv)

import qualified Hschain.Utxo.Lang.Error as E
import qualified Hschain.Utxo.Lang.Core.RefEval as Ref


-- | Executes spend-script in transaction. Spend script should be
--   well-typed and evaluate to either sigma-expression or boolean.
execScriptToSigma :: InputEnv -> ExprCore -> Either E.Error (Sigma PublicKey)
execScriptToSigma env prog = do
  -- Type check expression
  ty <- first (E.CoreScriptError . E.TypeCoreError)
      $ typeCheck prog
  case ty of
    SigmaT -> pure ()
    BoolT  -> pure ()
    _      -> Left $ E.CoreScriptError E.ResultIsNotSigma
  -- Evaluate script
  case Ref.evalProg env prog of
    Ref.EvalPrim (PrimBool  b) -> Right $ Fix $ SigmaBool b
    Ref.EvalPrim (PrimSigma s) -> case eliminateSigmaBool s of
      Left  b  -> Right $ Fix $ SigmaBool b
      Right s' -> Right   s'
    _ -> error "Internal error:  Left $ E.CoreScriptError E.ResultIsNotSigma"
