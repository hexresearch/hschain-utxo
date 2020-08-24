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
  = SigAnd | SigOr | SigPk | SigBool
  -- ^ sigma operators
  | TextLength | TextAppend | HashSha | ShowInt | ShowBool
  -- ^ text operators
  | BytesAppend | ToBytes ArgType | FromBytes ArgType | Sha256
  -- ^ bytes operations
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

