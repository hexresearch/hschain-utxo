{-# LANGUAGE OverloadedStrings #-}
-- |
module TM.Core ( tests )where

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
    , testCase "AdditionPrim" $ do
        assertBool "typecheck" $ typeCheck preludeTypeContext progAdditionPrim
        Right [PrimInt 101] @=? run progAddition
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

-- Addition of two integers
progAdditionPrim :: [Scomb]
progAdditionPrim =
  [ mkMain $ Typed
    { typed'value = EPrimOp (OpAdd (EPrim (PrimInt 1)) (EPrim (PrimInt 100)))
    , typed'type  = intT
    }
  ]

mkMain :: Typed Expr -> Scomb
mkMain s = Scomb
  { scomb'name = "main"
  , scomb'args = mempty
  , scomb'body = s
  }

run :: CoreProg -> Either Error [Prim]
run
  = fmap (O.toList . gmachine'output)
  . eval
  . compile
