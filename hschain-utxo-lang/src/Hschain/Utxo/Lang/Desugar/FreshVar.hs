module Hschain.Utxo.Lang.Desugar.FreshVar(
    MonadFreshVar(..)
  , getFreshVar
  , FreshVar(..)
  , runFreshVar
) where

import Control.Monad.State.Strict

import Data.String
import Data.Text (Text)

import Hschain.Utxo.Lang.Expr

class Monad m => MonadFreshVar m where
  getFreshVarName :: m Text

getFreshVar :: MonadFreshVar m => Loc -> m VarName
getFreshVar loc = fmap (VarName loc) getFreshVarName

--------------------------------------------

newtype FreshVar a = FreshVar (State Int a)
  deriving newtype (Functor, Applicative, Monad, MonadState Int)

instance MonadFreshVar FreshVar where
  getFreshVarName = do
    freshIdx <- get
    modify' (+ 1)
    return $ toName freshIdx
    where
      toName = fromString . ('$' : ) . show

runFreshVar :: FreshVar a -> a
runFreshVar (FreshVar st) = evalState st 0

