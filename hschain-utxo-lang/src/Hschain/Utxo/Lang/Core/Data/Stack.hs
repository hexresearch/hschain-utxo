-- | This module defines execution stack
module Hschain.Utxo.Lang.Core.Data.Stack(
    Stack
  , put
  , pop
  , peek
  , lookup
  , slide
  , length
) where

import Prelude hiding (lookup, length)

import Data.Sequence hiding (lookup, length)

import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Data.Sequence as S

-- | Current stack of the G-machine
newtype Stack = Stack { unStack :: Seq Addr }
  deriving newtype (Semigroup, Monoid)

-- | Put address on top of the stack
put :: Addr -> Stack -> Stack
put a (Stack seq) = Stack (a <| seq)

-- | Pop element from top of the stack
pop :: Stack -> (Maybe Addr, Stack)
pop (Stack seq) =
  case viewl seq of
    EmptyL  -> (Nothing, Stack seq)
    a :< as -> (Just a, Stack as)

peek :: Stack -> Maybe Addr
peek (Stack seq) =
  case viewl seq of
    EmptyL -> Nothing
    a :< _ -> Just a

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


