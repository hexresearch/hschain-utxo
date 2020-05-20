-- | Evaluation of G-machine code
module Hschain.Utxo.Lang.Core.Gmachine.Eval(
  eval
) where

import Hschain.Utxo.Lang.Core.Gmachine.Monad

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..))
import Hschain.Utxo.Lang.Core.Data.Heap (Heap, Node(..), Globals)
import Hschain.Utxo.Lang.Core.Data.Stack (Stack)
import Hschain.Utxo.Lang.Core.Data.Stat (Stat)
import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Hschain.Utxo.Lang.Core.Data.Code  as Code
import qualified Hschain.Utxo.Lang.Core.Data.Heap  as Heap
import qualified Hschain.Utxo.Lang.Core.Data.Stack as Stack
import qualified Hschain.Utxo.Lang.Core.Data.Stat  as Stat


-- | Evaluates code for Gmachine and returns the final state
-- and possible errors. If there are no errors then code was successfully executed.
eval :: Code -> Globals -> Either Error Gmachine
eval code globals = runExec loop code globals
  where
    loop = fix $ \rec -> do
      mCode <- getNextInstr
      case mCode of
        Just code -> do
          updateStats
          dispatch code
          rec
        Nothing   -> stop

    stop = return ()

-- | React on single instruction
dispatch :: Instr -> Exec ()
dispatch = \case
  PushGlobal n -> pushGlobal n
  PushInt n    -> pushInt n
  Push n       -> push n
  Mkap         -> mkap
  Slide n      -> slide n
  Unwind       -> unwind

pushGlobal :: Name -> Exec ()
pushGlobal name = do
  addr <- fromError (NotFound name) $ fmap (Heap.lookupGlobal name) getGlobals
  putAddr addr

pushInt :: Int -> Exec ()
pushInt num = do
  addr <- alloc (NodeInt num)
  putAddr addr

mkap :: Exec ()
mkap = do
  a1  <- popAddr
  a2  <- popAddr
  res <- alloc (Ap a1 a2)
  putAddr res

push :: Int -> Exec ()
push n = do
  addr <- lookupAddr (n + 1)
  node <- lookupHeap addr
  case node of
    Ap _ appAddr -> putAddr appAddr
    _            -> badType

slide :: Int -> Exec ()
slide n = modifyStack (Stack.slide n)

unwind :: Exec ()
unwind = do
  addr <- peekAddr
  node <- lookupHeap addr
  newState node
  where
    newState = \case
      NodeInt n -> pure ()
      Ap a1 _   -> do
        putAddr a1
        modifyCode (Code.singleton Unwind <>)
      Fun size code -> onFun size code

    onFun size code = do
      stackSize <- getStackSize
      if stackSize < size
        then stackIsEmpty
        else modifyCode (code <> )


-------------------------------------------------------------
-- state update proxies for G-machine units

-- stack

-- | Puts address on top of the stack
putAddr :: Addr -> Exec ()
putAddr addr = modifyStack (Stack.put addr)

-- | Read top of the stack and remove that element from the stack
popAddr :: Exec Addr
popAddr = fromError StackIsEmpty $
  stateStack Stack.pop

-- | Read top of the stack without modifying it.
peekAddr :: Exec Addr
peekAddr = fromError StackIsEmpty $
  fmap Stack.peek getStack

-- | Read stack by index.
lookupAddr :: Int -> Exec Addr
lookupAddr n = fromError StackIsEmpty $
  fmap (Stack.lookup n) getStack

-- | Read stack size
getStackSize :: Exec Int
getStackSize = fmap Stack.length getStack

-- code

-- | Is final state of G-machine reached (no code left to execute)
isFinal :: Exec Bool
isFinal = fmap Code.null getCode

-- | read next code instruction to execute
getNextInstr :: Exec (Maybe Instr)
getNextInstr = do
  (mInstr, code') <- fmap Code.next getCode
  putCode code'
  return mInstr

-- stats

-- | Updates program execution statistics.
-- Call it on every command execution.
updateStats :: Exec ()
updateStats = modifyStats Stat.update

-- heap

-- | allocate new cell for the node on the heap,
alloc :: Node -> Exec Addr
alloc node = stateHeap (Heap.alloc node)

-- | Get the node stored in the heap by its address.
lookupHeap :: Addr -> Exec Node
lookupHeap addr = fromError (BadAddr addr) $
  fmap (Heap.lookup addr) getHeap

