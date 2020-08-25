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
  = ToBytes ArgType
  | FromBytes ArgType
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

