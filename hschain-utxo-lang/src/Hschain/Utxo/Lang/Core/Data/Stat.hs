-- | This module defines statistics for G-machine execution
module Hschain.Utxo.Lang.Core.Data.Stat(
    Stat
  , empty
  , update
) where

-- | Statistics of program execution
data Stat = Stat
  { stat'execCounter :: !Int  -- ^ Counts the number of executions
  } deriving (Show, Eq)

-- | Initialize statistics.
empty :: Stat
empty = Stat { stat'execCounter = 0 }

-- | Update statistics of execution
-- We should call it on every execution of single instruction.
update :: Stat -> Stat
update st = bumpExecCounter st

-- | Add one to  execution counter
bumpExecCounter :: Stat -> Stat
bumpExecCounter st =
  st { stat'execCounter = stat'execCounter st + 1 }


