-- | G-machine code execution
module Hschain.Utxo.Lang.Core.Gmachine.Monad(
    Gmachine(..)
  , Exec
  , runExec
  -- * Errors
  , Error(..)
  , stackIsEmpty
  , notFound
  , badType
  , badAddr
  , fromError
  -- * State proxies
  -- ** Globals
  , getGlobals
  -- ** Stats
  , modifyStats
  -- ** Code
  , getCode
  , putCode
  , modifyCode
  -- ** Heap
  , getHeap
  , stateHeap
  -- ** Stack
  , stateStack
  , modifyStack
  , getStack
  -- * Re-exports
  , module X
) where

import Control.Monad.State.Strict
import Control.Monad.Except        as X

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..))
import Hschain.Utxo.Lang.Core.Data.Heap (Heap, Node(..), Globals)
import Hschain.Utxo.Lang.Core.Data.Stack (Stack)
import Hschain.Utxo.Lang.Core.Data.Stat (Stat)
import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Hschain.Utxo.Lang.Core.Data.Code  as Code
import qualified Hschain.Utxo.Lang.Core.Data.Heap  as Heap
import qualified Hschain.Utxo.Lang.Core.Data.Stack as Stack
import qualified Hschain.Utxo.Lang.Core.Data.Stat  as Stat


-- | G-machine is FSM for fast graph reduction
data Gmachine = Gmachine
  { gmachine'heap    :: Heap       -- ^ heap of the program
  , gmachine'stack   :: Stack      -- ^ current stack
  , gmachine'code    :: Code       -- ^ code to be executed
  , gmachine'globals :: Globals    -- ^ global definitions
  , gmachine'stats   :: Stat       -- ^ statistics of execution
  }

-- | Errors of execution
data Error
  = BadType         -- ^ encountered illegal application
  | BadAddr Addr    -- ^ address is not defined on the heap
  | NotFound Name   -- ^ can not find global name
  | StackIsEmpty    -- ^ Need to read element from stack but it is empty
  deriving (Show, Eq)

badType :: Exec a
badType = throwError BadType

badAddr :: Addr -> Exec a
badAddr addr = throwError (BadAddr addr)

notFound :: Name -> Exec a
notFound name = throwError (NotFound name)

stackIsEmpty :: Exec a
stackIsEmpty = throwError StackIsEmpty

-- | Monad for execution of Gmachine code
newtype Exec a = Exec { unExec :: StateT Gmachine (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState Gmachine, MonadError Error)

-- | Run exec-monad
runExec :: Exec a -> Code -> Globals -> Either Error Gmachine
runExec act code globals = runExcept $ execStateT (unExec act) initState
  where
    initState = Gmachine
      { gmachine'stack   = mempty
      , gmachine'heap    = Heap.empty
      , gmachine'code    = code
      , gmachine'globals = globals
      , gmachine'stats   = Stat.empty
      }

--------------------------------------------------------------
-- manage state
--
-- MonadState proxy functions for various components of G-machine state.

-- code

getGlobals :: Exec Globals
getGlobals = fmap gmachine'globals get

getCode :: Exec Code
getCode = fmap gmachine'code get

putCode :: Code -> Exec ()
putCode code = modify' $ \st -> st { gmachine'code = code }

modifyCode :: (Code -> Code) -> Exec ()
modifyCode f = modify' $ \st -> st { gmachine'code = f $ gmachine'code st }

-- stats

modifyStats :: (Stat -> Stat) -> Exec ()
modifyStats update = modify' $ \st -> st
  { gmachine'stats = update $ gmachine'stats st }

-- heap

stateHeap :: (Heap -> (a, Heap)) -> Exec a
stateHeap = toState getHeap putHeap

getHeap :: Exec Heap
getHeap = fmap gmachine'heap get

putHeap :: Heap -> Exec ()
putHeap heap = modify' $ \st -> st { gmachine'heap = heap }

-- stack

modifyStack :: (Stack -> Stack) -> Exec ()
modifyStack update = modify' $ \st -> st
  { gmachine'stack = update $ gmachine'stack st }

stateStack :: (Stack -> (a, Stack)) -> Exec a
stateStack = toState getStack putStack

getStack :: Exec Stack
getStack = fmap gmachine'stack get

putStack :: Stack -> Exec ()
putStack stack = modify' $ \st -> st { gmachine'stack = stack }

-- generic

toState :: Exec a -> (a -> Exec ()) -> (a -> (b, a)) -> Exec b
toState getter putter f = do
  (a, st) <- fmap f getter
  putter st
  return a

fromError :: Error -> Exec (Maybe a) -> Exec a
fromError err a = maybe (throwError err) pure =<< a

