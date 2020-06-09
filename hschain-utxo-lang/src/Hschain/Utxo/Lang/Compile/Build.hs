-- | Helpers to build expressions
module Hschain.Utxo.Lang.Compile.Build(
    ap1, ap2, ap3
  , var
) where

import Data.Fix

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

ap1 :: Expr a -> Expr a -> Expr a
ap1 a b = Fix $ EAp a b

ap2 :: Expr a -> Expr a -> Expr a -> Expr a
ap2 f a b = Fix $ EAp (ap1 f a) b

ap3:: Expr a -> Expr a -> Expr a -> Expr a -> Expr a
ap3 f a b c = Fix $ EAp (ap2 f a b) c

var :: Name -> Expr a
var = Fix . EVar



