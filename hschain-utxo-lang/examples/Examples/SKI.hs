{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- SKI calculus examples
module Examples.SKI where

import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Data.Prim


----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

-- | I Combinator. We should use monomorphic types as arguments.
--
-- > I :: a -> a
-- > I x = x
skiI :: Name -> TypeCore -> Scomb
skiI name ty = Scomb
  { scomb'name   = "skiI." <> name
  , scomb'args   = [Typed "x" ty]
  , scomb'body   = Typed "x" ty
  }

-- | K combinator. We should use monomorphic types as arguments.
--
-- > K :: a -> b -> a
-- > K x y = x
skiK :: Text -> TypeCore -> TypeCore -> Scomb
skiK name tyX tyY = Scomb
  { scomb'name   = "skiK." <> name
  , scomb'args   = [Typed "x" tyX, Typed "y" tyY]
  , scomb'body   = Typed "x" tyX
  }

-- | S combinator. We should use monomorphic types as arguments
--
-- > S :: (a -> b -> c) -> (a -> b) -> a -> c
-- > S x y z = x z (y z)
skiS :: Text -> TypeCore -> TypeCore -> TypeCore -> Scomb
skiS name tyA tyB tyC = Scomb
  { scomb'name   = "skiS." <> name
  , scomb'args   = [ Typed "x" tyX
                   , Typed "y" tyY
                   , Typed "z" tyZ
                   ]
  , scomb'body   = Typed
                   ((EAp "x" "z") `EAp` (EAp "y" "z"))
                   tyC
  }
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
exampleSKK3 :: CoreProg
exampleSKK3 = CoreProg
  [ skiK "intT" IntT IntT
  , skiK "funT" IntT (IntT :-> IntT)
  , skiS ""     IntT (IntT :-> IntT) IntT
  , mkMain $ Typed
    ((("skiS." `EAp` "skiK.funT") `EAp` "skiK.intT") `EAp` EPrim (PrimInt 3))
    IntT
  ]

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

mkMain :: Typed TypeCore ExprCore -> Scomb
mkMain s = Scomb
  { scomb'name   = "main"
  , scomb'args   = []
  , scomb'body   = s
  }
