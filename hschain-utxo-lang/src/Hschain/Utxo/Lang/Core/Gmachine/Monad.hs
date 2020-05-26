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
  , dumpIsEmpty
  , missingCase
  , fromError
  -- * State proxies
  -- ** Globals
  , getGlobals
  , modifyGlobals
  -- ** Stats
  , modifyStats
  -- ** Code
  , getCode
  , putCode
  , modifyCode
  -- ** Heap
  , getHeap
  , stateHeap
  , modifyHeap
  -- ** Stack
  , stateStack
  , modifyStack
  , getStack
  , putStack
  , modifyDump
  -- ** Dump
  , getDump
  , putDump
  -- ** Output
  , modifyOutput
  -- ** Vstack
  , stateVstack
  , modifyVstack
  -- * Re-exports
  , module X
) where

import Control.Monad.State.Strict
import Control.Monad.Except        as X

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..))
import Hschain.Utxo.Lang.Core.Data.Dump (Dump)
import Hschain.Utxo.Lang.Core.Data.Heap (Heap, Node(..), Globals)
import Hschain.Utxo.Lang.Core.Data.Output (Output)
import Hschain.Utxo.Lang.Core.Data.Stack (Stack)
import Hschain.Utxo.Lang.Core.Data.Stat (Stat)
import Hschain.Utxo.Lang.Core.Data.Vstack (Vstack)
import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Hschain.Utxo.Lang.Core.Data.Code   as Code
import qualified Hschain.Utxo.Lang.Core.Data.Dump   as Dump
import qualified Hschain.Utxo.Lang.Core.Data.Heap   as Heap
import qualified Hschain.Utxo.Lang.Core.Data.Output as Output
import qualified Hschain.Utxo.Lang.Core.Data.Stack  as Stack
import qualified Hschain.Utxo.Lang.Core.Data.Stat   as Stat
import qualified Hschain.Utxo.Lang.Core.Data.Vstack as Vstack


-- | G-machine is FSM for fast graph reduction
data Gmachine = Gmachine
  { gmachine'heap    :: Heap       -- ^ heap of the program
  , gmachine'stack   :: Stack      -- ^ current stack
  , gmachine'code    :: Code       -- ^ code to be executed
  , gmachine'globals :: Globals    -- ^ global definitions
  , gmachine'dump    :: Dump       -- ^ stack to save frame of the execution (code, currentStack)
  , gmachine'stats   :: Stat       -- ^ statistics of execution
  , gmachine'vstack  :: Vstack     -- ^ stack for integer operations
                                   -- we place arguments here to save heap allocations
  , gmachine'output  :: Output     -- ^ output of the execution
  } deriving (Show, Eq)

-- | Errors of execution
data Error
  = BadType         -- ^ encountered illegal application
  | BadAddr Addr    -- ^ address is not defined on the heap
  | NotFound Name   -- ^ can not find global name
  | StackIsEmpty    -- ^ Need to read element from stack but it is empty
  | VstackIsEmpty   -- ^ Need to read element from Vstack but it is empty
  | DumpIsEmpty     -- ^ Attempt to read empty dump
  | MissingCase     -- ^ missing case-alternative
  deriving (Show, Eq)

badType :: Exec a
badType = throwError BadType

badAddr :: Addr -> Exec a
badAddr addr = throwError (BadAddr addr)

notFound :: Name -> Exec a
notFound name = throwError (NotFound name)

stackIsEmpty :: Exec a
stackIsEmpty = throwError StackIsEmpty

dumpIsEmpty :: Exec a
dumpIsEmpty = throwError DumpIsEmpty

missingCase :: Exec a
missingCase = throwError MissingCase

-- | Monad for execution of Gmachine code
newtype Exec a = Exec { unExec :: StateT Gmachine (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState Gmachine, MonadError Error)

-- | Run exec-monad
runExec :: Exec a -> Gmachine -> Either Error Gmachine
runExec act initState = runExcept $ execStateT (unExec act) initState

--------------------------------------------------------------
-- manage state
--
-- MonadState proxy functions for various components of G-machine state.

-- code

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

modifyHeap :: (Heap -> Heap) -> Exec ()
modifyHeap update = modify' $ \st -> st
  { gmachine'heap = update $ gmachine'heap st }

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

-- dump

getDump :: Exec Dump
getDump = fmap gmachine'dump get

putDump :: Dump -> Exec ()
putDump dump = modify' $ \st -> st { gmachine'dump = dump }

modifyDump :: (Dump -> Dump) -> Exec ()
modifyDump update = modify' $ \st -> st
  { gmachine'dump = update $ gmachine'dump st }

-- globals

getGlobals :: Exec Globals
getGlobals = fmap gmachine'globals get

modifyGlobals :: (Globals -> Globals) -> Exec ()
modifyGlobals update = modify' $ \st -> st
  { gmachine'globals = update $ gmachine'globals st }

-- output

modifyOutput :: (Output -> Output) -> Exec ()
modifyOutput update = modify' $ \st -> st
  { gmachine'output = update $ gmachine'output st }

-- vstack

modifyVstack :: (Vstack -> Vstack) -> Exec ()
modifyVstack update = modify' $ \st -> st
  { gmachine'vstack = update $ gmachine'vstack st }

stateVstack :: (Vstack -> (a, Vstack)) -> Exec a
stateVstack = toState getVstack putVstack

getVstack :: Exec Vstack
getVstack = fmap gmachine'vstack get

putVstack :: Vstack -> Exec ()
putVstack stack = modify' $ \st -> st { gmachine'vstack = stack }

-- generic

toState :: Exec a -> (a -> Exec ()) -> (a -> (b, a)) -> Exec b
toState getter putter f = do
  (a, st) <- fmap f getter
  putter st
  return a

fromError :: Error -> Exec (Maybe a) -> Exec a
fromError err a = maybe (throwError err) pure =<< a

