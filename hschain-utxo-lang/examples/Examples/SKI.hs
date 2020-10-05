{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- SKI calculus examples
module Examples.SKI where

import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Types


----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

-- | I Combinator. We should use monomorphic types as arguments.
--
-- > I :: a -> a
-- > I x = x
skiI :: TypeCore -> ExprCore
skiI ty
  = ELam "x" ty
  $ EVar "x"

-- | K combinator. We should use monomorphic types as arguments.
--
-- > K :: a -> b -> a
-- > K x y = x
skiK :: TypeCore -> TypeCore -> ExprCore
skiK tyX tyY
  = ELam "x" tyX
  $ ELam "y" tyY
  $ EVar "x"

-- | S combinator. We should use monomorphic types as arguments
--
-- > S :: (a -> b -> c) -> (a -> b) -> a -> c
-- > S x y z = x z (y z)
skiS :: TypeCore -> TypeCore -> TypeCore -> ExprCore
skiS tyA tyB tyC
  = ELam "x" tyX
  $ ELam "y" tyY
  $ ELam "z" tyZ
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
exampleSKK3 :: ExprCore
exampleSKK3
  = ELet "K_intT" (skiK IntT IntT)
  $ ELet "K_funT" (skiK IntT (IntT :-> IntT))
  $ ELet "S"      (skiS IntT (IntT :-> IntT) IntT)
  $ ((("S" `EAp` "K_funT") `EAp` "K_intT") `EAp` EPrim (PrimInt 3))
