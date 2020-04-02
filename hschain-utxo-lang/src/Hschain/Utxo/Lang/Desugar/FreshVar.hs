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

class Monad m => MonadFreshVar m where
  getFreshVarName :: m Text

getFreshVar :: MonadFreshVar m => Loc -> m VarName
getFreshVar loc = fmap (VarName loc) getFreshVarName

--------------------------------------------

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

runFreshVar :: Monad m => FreshVar m a -> m a
runFreshVar (FreshVar st) = evalStateT st 0

