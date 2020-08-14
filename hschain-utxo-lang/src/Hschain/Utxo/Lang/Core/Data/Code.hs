-- | Module defines type for sequence of Code instructions
module Hschain.Utxo.Lang.Core.Data.Code(
  Instr(..)
) where

import Prelude hiding (null, init)

import Control.DeepSeq
import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Expr (ArgType)

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

