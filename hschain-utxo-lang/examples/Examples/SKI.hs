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
skiI :: Name -> Type -> Scomb
skiI name ty = Scomb
  { scomb'name = "skiI." <> name
  , scomb'args = [Typed "x" ty]
  , scomb'body = Typed (EVar "x") ty
  }

-- | K combinator
--
-- > K :: a -> b -> a
-- > K x y = x
skiK :: Text -> Type -> Type -> Scomb
skiK name tyX tyY = Scomb
  { scomb'name = "skiK." <> name
  , scomb'args = [Typed "x" tyX, Typed "y" tyY]
  , scomb'body = Typed (EVar "x") tyX
  }

-- | S combinator
--
-- > S :: (a -> b -> c) -> (a -> b) -> a -> c
-- > S x y z = x z (y z)
skiS :: Text -> Type -> Type -> Type -> Scomb
skiS name tyA tyB tyC = Scomb
  { scomb'name = "skiS." <> name
  , scomb'args = [ Typed "x" (tyA `arrowT` (tyB `arrowT` tyC))
                 , Typed "y" (tyA `arrowT` tyB)
                 , Typed "z" tyA
                 ]
  , scomb'body = Typed
                 ((EAp (EVar "x") (EVar "z")) `EAp` (EAp (EVar "y") (EVar "z")))
                 tyC
  }


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
    (((EVar "skiS." `EAp` EVar "skiK.funT") `EAp` EVar "skiK.intT") `EAp` EPrim (PrimInt 3))
    intT
  ]


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

mkMain :: Typed Expr -> Scomb
mkMain s = Scomb
  { scomb'name = "main"
  , scomb'args = mempty
  , scomb'body = s
  }
