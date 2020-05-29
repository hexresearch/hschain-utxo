-- | Dump is used to save and restore execution frames
-- when we jump to execution of expression that needs to be in WHNF.
module Hschain.Utxo.Lang.Core.Data.Dump(
    Dump
  , put
  , pop
) where

import Hschain.Utxo.Lang.Core.Data.Code (Code)
import Hschain.Utxo.Lang.Core.Data.Stack (Stack)

-- | Structure to save current state of the stack
newtype Dump = Dump [(Code, Stack)]
  deriving newtype (Show, Eq, Semigroup, Monoid)

-- | Save code and stack to the dump.
put :: Code -> Stack -> Dump -> Dump
put c s (Dump xs) = Dump ((c, s) : xs)

-- | Get top element of the Dump
pop :: Dump -> (Maybe (Code, Stack), Dump)
pop (Dump xs) = case xs of
  []   -> (Nothing, Dump xs)
  a:as -> (Just a, Dump as)

