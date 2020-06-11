-- | Helpers to build expressions
module Hschain.Utxo.Lang.Compile.Build(
    ap1, ap2, ap3
  , var
  , fun
  , prim
) where

import Data.Fix

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.List as L

-- | Apply unary function to its argument
ap1 :: Expr a -> Expr a -> Expr a
ap1 a b = Fix $ EAp a b

-- | Apply binary function to its arguments
ap2 :: Expr a -> Expr a -> Expr a -> Expr a
ap2 f a b = Fix $ EAp (ap1 f a) b

-- | Apply ternary function to its arguments
ap3:: Expr a -> Expr a -> Expr a -> Expr a -> Expr a
ap3 f a b c = Fix $ EAp (ap2 f a b) c

-- | Apply function to the list of arguments
fun :: Expr a -> [Expr a] -> Expr a
fun f args = L.foldl' ap1 f args

-- | Build a variable
var :: Name -> Expr a
var = Fix . EVar

-- | Build a primitive value
prim :: Prim -> Expr a
prim = Fix . EPrim

