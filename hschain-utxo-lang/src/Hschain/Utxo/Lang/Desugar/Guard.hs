-- | Module defines function to desugar guard-expressions
module Hschain.Utxo.Lang.Desugar.Guard(
  fromGuardedRhs
) where

import Data.Fix

import Hschain.Utxo.Lang.Expr

import qualified Language.HM as H

-- | Converts guarded expression to single expression with explicit
-- if-then-else checks.
fromGuardedRhs :: Rhs Lang -> Lang
fromGuardedRhs rhs = case rhs of
  UnguardedRhs a -> a
  GuardedRhs  as -> foldr toGuardExpr failCase as
  where
    loc = H.getLoc rhs

    failCase = Fix $ FailCase loc

    toGuardExpr Guard{..} other =
      Fix $ If (H.getLoc guard'predicate)
              guard'predicate
              guard'rhs
              other





