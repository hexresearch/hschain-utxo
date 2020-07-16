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
    [ testProgram "spend to key" progSpendToKey (PrimSigma (Fix $ SigmaBool True))
    , testProgram "Addition"     progAddition   (PrimInt 101)
    , testProgram "SKK3"         exampleSKK3    (PrimInt 3)
    ]
  ]

testProgram :: String -> CoreProg -> Prim -> TestTree
testProgram nm prog res = testGroup nm
  [ testCase "typecheck" $ Nothing     @=? typeCheck preludeTypeContext prog
  , testCase "eval"      $ Right [res] @=? run prog
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
