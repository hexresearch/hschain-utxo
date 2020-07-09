{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module TM.Core ( tests )where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine
import qualified Hschain.Utxo.Lang.Core.Data.Output as O

tests :: TestTree
tests = testGroup "core"
  [ testGroup "simple"
    [ testCase "spend to key" $ do
        assertBool "typecheck" $ typeCheck preludeTypeContext progSpendToKey
        Right [PrimSigma (SigmaBool True)] @=? run progSpendToKey
      --
    , testCase "Addition" $ do
        assertBool "typecheck" $ typeCheck preludeTypeContext progAddition
        Right [PrimInt 101] @=? run progAddition
    , testCase "SKK3" $ do
        assertBool "typecheck" $ typeCheck preludeTypeContext exampleSKK3
        Right [PrimInt 3] @=? run exampleSKK3
    ]
  ]


-- Trivial
progSpendToKey :: [Scomb]
progSpendToKey =
  [ mkMain $ Typed
    { typed'value = EPrim (PrimSigma (SigmaBool True))
    , typed'type  = sigmaT
    }
  ]

-- Addition of two integers
progAddition :: [Scomb]
progAddition =
  [ mkMain $ Typed
    { typed'value = EAp
                    (EAp (EVar "+") (EPrim (PrimInt 1)))
                    (EPrim (PrimInt 100))
    , typed'type  = intT
    }
  ]

mkMain :: Typed Expr -> Scomb
mkMain s = Scomb
  { scomb'name = "main"
  , scomb'args = mempty
  , scomb'body = s
  }


----------------------------------------------------------------
-- SKI-calculus

-- I :: a -> a
-- I x = x
skiI :: Type -> Scomb
skiI ty = Scomb
  { scomb'name = "skiI" -- ++ show ty
  , scomb'args = [Typed "x" ty]
  , scomb'body = Typed (EVar "x") ty
  }

-- K :: a -> b -> a
-- K x y = x
skiK :: Text -> Type -> Type -> Scomb
skiK name tyX tyY = Scomb
  { scomb'name = "skiK." <> name
  , scomb'args = [Typed "x" tyX, Typed "y" tyY]
  , scomb'body = Typed (EVar "x") tyX
  }

-- S :: (a -> b -> c) -> (a -> b) -> a -> c
-- S x y z = x z (y z)
skiS :: Type -> Type -> Type -> Scomb
skiS tyA tyB tyC = Scomb
  { scomb'name = "skiS"
  , scomb'args = [ Typed "x" (tyA `arrowT` (tyB `arrowT` tyC))
                 , Typed "y" (tyA `arrowT` tyB)
                 , Typed "z" tyA
                 ]
  , scomb'body = Typed
                 ((EAp (EVar "x") (EVar "z")) `EAp` (EAp (EVar "y") (EVar "z")))
                 tyC
  }

-- Example of program
--
-- > S K K 3
exampleSKK3 :: [Scomb]
exampleSKK3 =
  [ skiK "intT" intT intT
  , skiK "funT" intT (intT `arrowT` intT)
  , skiS        intT (intT `arrowT` intT) intT
  , mkMain $ Typed
    (((EVar "skiS" `EAp` EVar "skiK.funT") `EAp` EVar "skiK.intT") `EAp` EPrim (PrimInt 3))
    intT
  ]




run :: CoreProg -> Either Error [Prim]
run
  = fmap (O.toList . gmachine'output)
  . eval
  . compile
