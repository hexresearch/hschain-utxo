-- | Module defines type for sequence of Code instructions
module Hschain.Utxo.Lang.Core.Data.Code(
    Code
  , Instr(..)
  , init
  , next
  , null
  , singleton
  , fromList
) where

import Prelude hiding (null, init)

import Data.Sequence (Seq)

import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Data.Sequence as S

-- | Code to feed to machine
newtype Code = Code { unCode :: Seq Instr }
  deriving newtype (Semigroup, Monoid, Show, Eq)

-- | Instructions for G-machine
data Instr
  = Unwind
  -- ^ finish the execution of the combinator body
  | PushGlobal !Name
  -- ^ save the global name of supercombinator
  | PushInt !Int
  -- ^ save the constant int
  | Push !Int
  -- ^ push address on stack
  | Mkap
  -- ^ make application, removes two top elements from stack
  -- creates application on heap, and puts address of the result on stack
  | Update !Int
  -- ^ overwrite (N+1)'th element of the stack with indirection node on top of the stack
  | Pop !Int
  -- ^ pop N elements from top of the stack
  | Slide !Int
  -- ^ removes N elements after the top element
  | Alloc !Int
  -- ^ Allocates N place-holder nodes on the heap
  | Eval
  -- ^ Evaluate the expression which is referenced from the top of the stack to WHNF
  | Add | Sub | Mul | Div | Neg
  -- ^ Arithmetic operations
  | Eq | Ne | Lt | Le | Gt | Ge
  -- ^ Comparison operations
  | Cond !Code !Code
  -- ^ low-level implementation of if
  deriving (Show, Eq)

-- | Initial code for start of the program
init :: Code
init = Code $ S.fromList [PushGlobal "main", Eval]

-- | reads next instruction from the code sequence
-- and removes it from the sequence
next :: Code -> (Maybe Instr, Code)
next (Code st) =
  case S.viewl st of
    S.EmptyL  -> (Nothing, Code st)
    a S.:< as -> (Just a, Code as)

-- | Test for emptyness of the code sequence
null :: Code -> Bool
null = S.null . unCode

-- | Constructs code sequence with one instruction.
singleton :: Instr -> Code
singleton a = Code $ S.singleton a

fromList :: [Instr] -> Code
fromList as = Code $ S.fromList as


