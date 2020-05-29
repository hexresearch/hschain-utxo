-- | Functions to work with stack
module Hschain.Utxo.Lang.Core.Gmachine.Eval.Stack(
   pop
 , push
 , slide
 , putAddr
 , popAddr
 , popAddrList
 , peekAddr
 , peekBottomAddr
 , lookupAddr
 , getStackSize
) where

import Data.Sequence (Seq)

import Hschain.Utxo.Lang.Core.Gmachine.Monad
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Data.Stack (Stack)

import qualified Hschain.Utxo.Lang.Core.Data.Stack as Stack

push :: Int -> Exec ()
push n =
  putAddr =<< lookupAddr n

slide :: Int -> Exec ()
slide n = modifyStack (Stack.slide n)

pop :: Int -> Exec ()
pop n = modifyStack $ Stack.drop n

-- | Puts address on top of the stack
putAddr :: Addr -> Exec ()
putAddr addr = modifyStack (Stack.put addr)

-- | Read top of the stack and remove that element from the stack
popAddr :: Exec Addr
popAddr = fromError StackIsEmpty $
  stateStack Stack.pop

-- | Pops specified number of arguments
--
-- TODO: re-implement with stack operation directly (for efficient execution)
popAddrList :: Int -> Exec (Seq Addr)
popAddrList size = stateStack $ Stack.popN size

-- | Read top of the stack without modifying it.
peekAddr :: Exec Addr
peekAddr = fromError StackIsEmpty $
  fmap Stack.peek getStack

peekBottomAddr :: Exec Addr
peekBottomAddr = fromError StackIsEmpty $
  fmap Stack.peekBottom getStack

-- | Read stack by index.
lookupAddr :: Int -> Exec Addr
lookupAddr n = fromError StackIsEmpty $
  fmap (Stack.lookup n) getStack

-- | Read stack size
getStackSize :: Exec Int
getStackSize = fmap Stack.length getStack

