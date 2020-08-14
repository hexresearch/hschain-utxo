-- | Module defines type for sequence of Code instructions
module Hschain.Utxo.Lang.Core.Data.Code(
    Code
  , Instr(..)
) where

import Prelude hiding (null, init)

import Control.DeepSeq
import Data.IntMap (IntMap)
import Data.Sequence (Seq)
import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Expr (ArgType)
import Hschain.Utxo.Lang.Core.Data.Prim

-- | Code to feed to machine
newtype Code = Code { unCode :: Seq Instr }
  deriving newtype (Semigroup, Monoid, Show, Eq, NFData)

-- | Instructions for G-machine
data Instr
  = Add | Sub | Mul | Div | Neg
  -- ^ Arithmetic operations
  | Eq | Ne | Lt | Le | Gt | Ge
  -- ^ Comparison operations
  | And | Or | Not | Xor
  -- ^ boolean operators
  | SigAnd | SigOr | SigPk | SigBool
  -- ^ sigma operators
  | TextLength | TextAppend | HashBlake| HashSha | ShowInt | ShowBool
  -- ^ text operators
  | BytesAppend | ToBytes ArgType | FromBytes ArgType | Sha256
  -- ^ bytes operations
  | Bottom
  -- ^ Failed termination
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

type CaseMap = IntMap Code

data GlobalName
  = GlobalName !Name
  -- ^ name of the global supercombinator
  | ConstrName !Int !Int
  -- ^ name of the global constructor (int tag, arity)
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

