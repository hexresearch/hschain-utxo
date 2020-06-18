-- | Helpers to build expressions
module Hschain.Utxo.Lang.Compile.Build(
    ap1, ap2, ap3
  , var
  , fun
  , prim
) where

import Data.Fix

import Hschain.Utxo.Lang.Expr (Loc)
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.List as L

-- | Apply unary function to its argument
ap1 :: Loc -> Expr a -> Expr a -> Expr a
ap1 loc a b = Fix $ EAp loc a b

-- | Apply binary function to its arguments
ap2 :: Loc -> Expr a -> Expr a -> Expr a -> Expr a
ap2 loc f a b = Fix $ EAp loc (ap1 loc f a) b

-- | Apply ternary function to its arguments
ap3:: Loc -> Expr a -> Expr a -> Expr a -> Expr a -> Expr a
ap3 loc f a b c = Fix $ EAp loc (ap2 loc f a b) c

-- | Apply function to the list of arguments
fun :: Loc -> Expr a -> [Expr a] -> Expr a
fun loc f args = L.foldl' (ap1 loc) f args

-- | Build a variable
var :: Loc -> Name -> Expr a
var loc = Fix . EVar loc

-- | Build a primitive value
prim :: Loc -> Prim -> Expr a
prim loc = Fix . EPrim loc

