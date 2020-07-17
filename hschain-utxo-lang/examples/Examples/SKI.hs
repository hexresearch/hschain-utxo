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

-- | I Combinator
--
-- > I :: a -> a
-- > I x = x
skiI :: Name -> TypeCore -> Scomb
skiI name ty = Scomb
  { scomb'name = "skiI." <> name
  , scomb'args = [Typed "x" ty]
  , scomb'body = Typed (EVar (Typed "x" ty)) ty
  }

-- | K combinator
--
-- > K :: a -> b -> a
-- > K x y = x
skiK :: Text -> TypeCore -> TypeCore -> Scomb
skiK name tyX tyY = Scomb
  { scomb'name = "skiK." <> name
  , scomb'args = [Typed "x" tyX, Typed "y" tyY]
  , scomb'body = Typed (EVar (Typed "x" tyX)) tyX
  }

-- | S combinator
--
-- > S :: (a -> b -> c) -> (a -> b) -> a -> c
-- > S x y z = x z (y z)
skiS :: Text -> TypeCore -> TypeCore -> TypeCore -> Scomb
skiS name tyA tyB tyC = Scomb
  { scomb'name = "skiS." <> name
  , scomb'args = [ Typed "x" tyX
                 , Typed "y" tyY
                 , Typed "z" tyZ
                 ]
  , scomb'body = Typed
                 ((EAp (EVar (Typed "x" tyX)) (EVar (Typed "z" tyZ))) `EAp` (EAp (EVar (Typed "y" tyY)) (EVar (Typed "z" tyZ))))
                 tyC
  }
  where
    tyX = tyA `arrowT` (tyB `arrowT` tyC)
    tyY = tyA `arrowT` tyB
    tyZ = tyA


----------------------------------------------------------------
-- Example programs
----------------------------------------------------------------

-- | Example of program
--
-- > S K K 3
exampleSKK3 :: CoreProg
exampleSKK3 = CoreProg
  [ skiK "intT" intT intT
  , skiK "funT" intT (intT `arrowT` intT)
  , skiS ""     intT (intT `arrowT` intT) intT
  , mkMain $ Typed
    (((EVar (Typed "skiS." tySkiS) `EAp` EVar (Typed "skiK.funT" tySkiK_funT)) `EAp` EVar (Typed "skiK.intT" tySkiK_intT)) `EAp` EPrim (PrimInt 3))
    intT
  ]
  where
    tySkiK_intT = funT [intT, intT] intT
    tySkiK_funT = funT [intT, intT `arrowT` intT] intT
    tySkiS      = funT [a `arrowT` (b `arrowT` c), a `arrowT` b, a] c
      where
        a = intT
        b = intT `arrowT` intT
        c = intT


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

mkMain :: Typed ExprCore -> Scomb
mkMain s = Scomb
  { scomb'name = "main"
  , scomb'args = mempty
  , scomb'body = s
  }
