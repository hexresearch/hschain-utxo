-- | Check that program has no recursion.
module Hschain.Utxo.Lang.Core.Compile.RecursionCheck(
  recursionCheck
) where

import Hschain.Utxo.Lang.Core.Compile.Expr

-- | Check that program has no recursion
-- We should check all top level bindings and let-expressions.
recursionCheck :: CoreProg -> Bool
recursionCheck = undefined




