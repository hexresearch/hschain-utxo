{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- SKI calculus examples
module Examples.SKI where

import Control.Monad (guard)
import Data.List (elemIndex)
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Types


----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

lam :: Eq a => a -> TypeCore -> Core a -> Core a
lam  nm ty  = ELam ty . abstract1 nm

let_ :: Eq a => a -> Core a -> Core a -> Core a
let_ nm e   = ELet e  . abstract1 nm

alt :: Eq a => Int -> [a] -> Core a -> CaseAlt a
alt i names = CaseAlt i (length names) . abstractN names

-- | I Combinator. We should use monomorphic types as arguments.
--
-- > I :: a -> a
-- > I x = x
skiI :: TypeCore -> Core Name
skiI ty
  = lam  "x" ty
  $ EVar "x"

-- | K combinator. We should use monomorphic types as arguments.
--
-- > K :: a -> b -> a
-- > K x y = x
skiK :: TypeCore -> TypeCore -> Core Name
skiK tyX tyY
  = lam  "x" tyX
  $ lam  "y" tyY
  $ EVar "x"

-- | S combinator. We should use monomorphic types as arguments
--
-- > S :: (a -> b -> c) -> (a -> b) -> a -> c
-- > S x y z = x z (y z)
skiS :: TypeCore -> TypeCore -> TypeCore -> Core Name
skiS tyA tyB tyC
  = lam  "x" tyX
  $ lam  "y" tyY
  $ lam  "z" tyZ
  $ (EAp "x" "z") `EAp` (EAp "y" "z")
  where
    tyX = tyA :-> tyB :-> tyC
    tyY = tyA :-> tyB
    tyZ = tyA


----------------------------------------------------------------
-- Example programs
----------------------------------------------------------------

-- | Example of program
--
-- > S K K 3
exampleSKK3 :: Core Name
exampleSKK3
  = let_ "K_intT" (skiK IntT IntT)
  $ let_ "K_funT" (skiK IntT (IntT :-> IntT))
  $ let_ "S"      (skiS IntT (IntT :-> IntT) IntT)
  $ ((("S" `EAp` "K_funT") `EAp` "K_intT") `EAp` EPrim (PrimInt 3))
