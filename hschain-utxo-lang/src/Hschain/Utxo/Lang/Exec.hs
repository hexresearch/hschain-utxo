-- | Module defines reduction of the expression (expression execution).
--
-- We reduce expression to the primitive value.
-- For blockchain transaction verification it is going
-- to be sigma-expression.
--
-- For now it is done with simple algorithm of substitution of
-- values (application of lambda abstractions and substitution of subexpressions).
module Hschain.Utxo.Lang.Exec(
    evalModule
  , runExec
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict

import Data.String
import Data.Text (Text)

import Hschain.Utxo.Lang.Build()
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Exec.Module


-- | Context of execution
data Ctx = Ctx
  { ctx'debug      :: !Text                -- ^ debug log for executed expression
  , ctx'freshVarId :: !Int                 -- ^ counter for allocation of fresh variables
  }

-- | Execution monad.
newtype Exec a = Exec (StateT Ctx (Either Error) a)
  deriving newtype (MonadState Ctx, Monad, Functor, Applicative, MonadError Error)

instance Alternative Exec where
  empty = throwError $ ExecError $ Undefined noLoc
  Exec stA <|> Exec stB =
    Exec $ StateT $ \s ->
      let eRes = runStateT stA s
      in  case eRes of
            Right res -> return res
            Left _    -> runStateT stB s

instance MonadFreshVar Exec where
  getFreshVarName = do
    idx <- fmap ctx'freshVarId get
    modify' $ \st -> st { ctx'freshVarId = ctx'freshVarId st + 1 }
    return $ toName idx
    where
      toName n = fromString $ '$' : show n

instance MonadLang Exec where

-- | Run execution monad.
runExec :: Exec a -> Either Error (a, Text)
runExec (Exec st) =
  fmap (second ctx'debug) $ runStateT st emptyCtx
  where
    emptyCtx = Ctx mempty 0
