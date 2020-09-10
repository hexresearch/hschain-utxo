-- | Functions to compile core progrmamms to instructions of G-machine
module Hschain.Utxo.Lang.Core.Compile.Prog(
    coreProgTerminates
  , isSigmaScript
  , execScriptToSigma
) where

import Control.Applicative
import Data.Fix

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.RecursionCheck
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (InputEnv)

import qualified Data.List       as L
import qualified Data.Vector     as V

import qualified Hschain.Utxo.Lang.Error as E
import qualified Hschain.Utxo.Lang.Core.RefEval as Ref

-- | Executes script to sigma-expression.
--
-- Sigma script should contain main function that
-- returns sigma-expression. The script should be well-typed and
-- contain no recursion.
execScriptToSigma :: InputEnv -> CoreProg -> Either E.Error (Sigma PublicKey)
execScriptToSigma env prog = case isSigmaScript prog of
  Nothing  -> refRes
  Just err -> Left err
  where
    refRes = case Ref.evalProg env prog of
      Ref.EvalPrim (PrimBool  b) -> Right $ Fix $ SigmaBool b
      Ref.EvalPrim (PrimSigma s) -> case eliminateSigmaBool s of
        Left  b  -> Right $ Fix $ SigmaBool b
        Right s' -> Right $ s'
      _                  -> Left $ E.CoreScriptError E.ResultIsNotSigma

-- | the program is sigma script if
--
-- * it terminates
-- * main function returns sigma-expression
isSigmaScript :: CoreProg -> Maybe E.Error
isSigmaScript prog =
      coreProgTerminates prog
  <|> mainIsSigma prog

-- | Check that program terminates.
--
-- It should
--
-- * be well typed
-- * has no recursion
coreProgTerminates :: CoreProg -> Maybe E.Error
coreProgTerminates prog =
      coreTypeError   (typeCheck prog)
  <|> recursiveScript (recursionCheck prog)
  where
    coreTypeError   = fmap (E.CoreScriptError . E.TypeCoreError)
    recursiveScript = E.wrapBoolError (E.CoreScriptError E.RecursiveScript )

mainIsSigma :: CoreProg -> Maybe E.Error
mainIsSigma (CoreProg prog) =
  case L.find (\sc -> scomb'name sc == "main") prog of
    Just mainComb -> resultIsNotSigma $ hasNoArgs mainComb && resultIsSigma mainComb
    Nothing       -> Just $ E.CoreScriptError E.NoMainFunction
  where
    resultIsNotSigma = E.wrapBoolError (E.CoreScriptError E.ResultIsNotSigma)
    hasNoArgs Scomb{..} = V.null scomb'args
    resultIsSigma Scomb{..} = sigmaT == typed'type scomb'body


