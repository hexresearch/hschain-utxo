-- | This module defines execution stack
module Hschain.Utxo.Lang.Core.Data.Stack(
    Stack
  , put
  , pop
  , peek
  , peekBottom
  , popN
  , peekN
  , lookup
  , drop
  , slide
  , length
  , singleton
  , appendSeq
  , rearrange
) where

import Prelude hiding (lookup, length, drop)

import Control.Monad

import Data.Sequence hiding (lookup, length, drop, singleton)

import Hschain.Utxo.Lang.Core.Data.Heap (Heap)
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Sequence as S

import qualified Hschain.Utxo.Lang.Core.Data.Node as Node
import qualified Hschain.Utxo.Lang.Core.Data.Heap as Heap

-- | Current stack of the G-machine
newtype Stack = Stack (Seq Addr)
  deriving newtype (Semigroup, Monoid, Show, Eq)

-- | Put address on top of the stack
put :: Addr -> Stack -> Stack
put a (Stack xs) = Stack (a <| xs)

-- | Pop element from top of the stack
pop :: Stack -> (Maybe Addr, Stack)
pop (Stack xs) =
  case viewl xs of
    EmptyL  -> (Nothing, Stack xs)
    a :< as -> (Just a, Stack as)

-- | Drops Nth elements from top of the stack
drop :: Int -> Stack -> Stack
drop n (Stack xs) = Stack $ S.drop n xs

-- | Pop N elements from the stack.
popN :: Int -> Stack -> (Seq Addr, Stack)
popN n (Stack xs) = (S.take n xs, Stack $ S.drop n xs)

-- | Reads top element of tthe stack and keeps it on the stack.
peek :: Stack -> Maybe Addr
peek (Stack xs) =
  case viewl xs of
    EmptyL -> Nothing
    a :< _ -> Just a

-- | Peek N elements from the stack.
peekN :: Int -> Stack -> (Seq Addr, Stack)
peekN n (Stack xs) = (S.take n xs, Stack xs)

-- | Appends sequence of elements to top of the stack
-- The first element of the sequence becomes top element of the stack.
appendSeq :: Seq Addr -> Stack -> Stack
appendSeq xs (Stack st) = Stack (xs <> st)

-- | Lookup Nth element of the stack
lookup :: Int -> Stack -> Maybe Addr
lookup n (Stack xs) = S.lookup n xs

-- | Slide keeps top of the stack and removes
-- Nth elements after it
--
-- > slide [a0, a1, ..., aN, aN+1, ...] => [a0, aN+1, ...]
slide :: Int -> Stack -> Stack
slide n st = case mTop of
  Just top -> put top $ remove n st1
  Nothing  -> st
  where
    (mTop, st1) = pop st

    remove m (Stack xs) = Stack $ S.drop m xs

-- | Get the size of the stack.
length :: Stack -> Int
length (Stack xs) = S.length xs


rearrange :: Int -> Heap -> Stack -> Maybe Stack
rearrange n heap (Stack xs) = fmap (\as -> Stack $ S.take n as <> S.drop n xs) mAs
  where
    mAs = mapM (Node.getNodeApArg <=< (\addr -> Heap.lookup addr heap)) =<< tailM xs

tailM :: Seq a -> Maybe (Seq a)
tailM x = case viewl x of
  EmptyL   -> Nothing
  _ :< res -> Just res

-- | Create stack wich contains single value.
singleton :: Addr -> Stack
singleton a = Stack (S.singleton a)

-- | Peeks the bottom element of the stack (in current implementation it is O(1))
peekBottom :: Stack -> Maybe Addr
peekBottom (Stack xs) =
  case S.viewr xs of
    EmptyR  -> Nothing
    _ :> a  -> Just a








