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

import Data.Foldable (toList)
import Data.Sequence hiding (lookup, length, drop, singleton)

import Hschain.Utxo.Lang.Core.Data.Heap (Heap)
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Sequence as S

import qualified Hschain.Utxo.Lang.Core.Data.Node as Node
import qualified Hschain.Utxo.Lang.Core.Data.Heap as Heap

-- | Current stack of the G-machine
newtype Stack = Stack { unStack :: Seq Addr }
  deriving newtype (Semigroup, Monoid, Show, Eq)

-- | Put address on top of the stack
put :: Addr -> Stack -> Stack
put a (Stack seq) = Stack (a <| seq)

-- | Pop element from top of the stack
pop :: Stack -> (Maybe Addr, Stack)
pop (Stack seq) =
  case viewl seq of
    EmptyL  -> (Nothing, Stack seq)
    a :< as -> (Just a, Stack as)

-- | Drops Nth elements from top of the stack
drop :: Int -> Stack -> Stack
drop n (Stack seq) = Stack $ S.drop n seq

-- | Pop N elements from the stack.
popN :: Int -> Stack -> (Seq Addr, Stack)
popN n (Stack seq) = (S.take n seq, Stack $ S.drop n seq)

-- | Reads top element of tthe stack and keeps it on the stack.
peek :: Stack -> Maybe Addr
peek (Stack seq) =
  case viewl seq of
    EmptyL -> Nothing
    a :< _ -> Just a

-- | Peek N elements from the stack.
peekN :: Int -> Stack -> (Seq Addr, Stack)
peekN n (Stack seq) = (S.take n seq, Stack seq)

-- | Appends sequence of elements to top of the stack
-- The first element of the sequence becomes top element of the stack.
appendSeq :: Seq Addr -> Stack -> Stack
appendSeq seq (Stack st) = Stack (seq <> st)

-- | Lookup Nth element of the stack
lookup :: Int -> Stack -> Maybe Addr
lookup n (Stack seq) = S.lookup n seq

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

    remove n (Stack seq) = Stack $ S.drop n seq

-- | Get the size of the stack.
length :: Stack -> Int
length (Stack seq) = S.length seq


rearrange :: Int -> Heap -> Stack -> Maybe Stack
rearrange n heap (Stack seq) = fmap (\as -> Stack $ S.take n as <> S.drop n seq) mAs
  where
    mAs = mapM (Node.getNodeApArg <=< (\addr -> Heap.lookup addr heap)) =<< tailM seq

tailM :: Seq a -> Maybe (Seq a)
tailM x = case viewl x of
  EmptyL   -> Nothing
  _ :< res -> Just res

-- | Create stack wich contains single value.
singleton :: Addr -> Stack
singleton a = Stack (S.singleton a)

-- | Peeks the bottom element of the stack (in current implementation it is O(1))
peekBottom :: Stack -> Maybe Addr
peekBottom (Stack seq) =
  case S.viewr seq of
    EmptyR  -> Nothing
    as :> a -> Just a








