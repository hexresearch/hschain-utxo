{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module TM.Core ( tests )where

import Data.Fix

import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine
import qualified Hschain.Utxo.Lang.Core.Data.Output as O
import Examples.SKI


tests :: TestTree
tests = testGroup "core"
  [ testGroup "simple"
    [ testCase "spend to key" $ do
        assertBool "typecheck" $ typeCheck preludeTypeContext progSpendToKey
        Right [PrimSigma (Fix $ SigmaBool True)] @=? run progSpendToKey
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
progSpendToKey :: CoreProg
progSpendToKey = CoreProg
  [ mkMain $ Typed
    { typed'value = EPrim (PrimSigma (Fix $ SigmaBool True))
    , typed'type  = sigmaT
    }
  ]

-- Addition of two integers
progAddition :: CoreProg
progAddition = CoreProg
  [ mkMain $ Typed
    { typed'value = EAp
                    (EAp (EVar (Typed "+" addT)) (EPrim (PrimInt 1)))
                    (EPrim (PrimInt 100))
    , typed'type  = intT
    }
  ]
  where
    addT = funT [intT, intT] intT

run :: CoreProg -> Either Error [Prim]
run
  = fmap (O.toList . gmachine'output)
  . eval
  . compile
