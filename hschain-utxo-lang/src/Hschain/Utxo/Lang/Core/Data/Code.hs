-- | Module defines type for sequence of Code instructions
module Hschain.Utxo.Lang.Core.Data.Code(
    Code
  , Instr(..)
  , next
  , null
  , singleton
) where

import Prelude hiding (null)

import Data.Sequence (Seq)

import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Data.Sequence as S

-- | Code to feed to machine
newtype Code = Code { unCode :: Seq Instr }
  deriving newtype (Semigroup, Monoid)

-- | Instructions for G-machine
data Instr
  = Unwind
  | PushGlobal !Name
  | PushInt !Int
  | Push !Int
  | Mkap
  | Slide !Int
  deriving (Show, Eq)

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

