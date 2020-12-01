{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- SKI calculus examples
module Examples.Simple where

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Types

-- | Addition of two integers
progAddition :: Core a
progAddition
  =     EPrimOp OpAdd
  `EAp` EPrim (PrimInt 1)
  `EAp` EPrim (PrimInt 100)

