-- | Module for allocation of fresh variables.
-- Fresh variables are used in the process of desugaring of expressions
-- and convertion to core-languiage for type-checking.
module Hschain.Utxo.Lang.Desugar.FreshVar(
    MonadFreshVar(..)
  , getFreshVar
  , FreshVar(..)
  , runFreshVar
) where

import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Trans

import Data.String
import Data.Text (Text)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Error

-- | Monad that allows us to allocate fresh variables
class Monad m => MonadFreshVar m where
  getFreshVarName :: m Text

-- | Allocate fresh variable with given source code location.
-- We keep source code locations for error reports.
getFreshVar :: MonadFreshVar m => Loc -> m VarName
getFreshVar loc = fmap (VarName loc) getFreshVarName

--------------------------------------------

-- | Monad for allocation of fresh variables.
-- It uses ascending integer counter for the allocation.
newtype FreshVar m a = FreshVar (StateT Int m a)
  deriving newtype (Functor, Applicative, Monad, MonadState Int)

instance MonadTrans FreshVar where
  lift = FreshVar . lift

instance MonadError Error (FreshVar (Either Error)) where
  throwError err = lift $ Left err
  catchError (FreshVar m) f = FreshVar $ catchError m ((\(FreshVar a) -> a) . f)

instance Monad m => MonadFreshVar (FreshVar m) where
  getFreshVarName = do
    freshIdx <- get
    modify' (+ 1)
    return $ toName freshIdx
    where
      toName = fromString . ('$' : ) . show

-- | Runs @FreshVar@ monad.
runFreshVar :: Monad m => FreshVar m a -> m a
runFreshVar (FreshVar st) = evalStateT st 0

