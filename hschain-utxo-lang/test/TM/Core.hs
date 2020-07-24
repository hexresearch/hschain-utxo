{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module TM.Core ( tests )where

import Data.Fix
import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Expr  (Box(..),BoxId(..),Script(..))
import Hschain.Utxo.Lang.Types (TxEnv(..))
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine
import qualified Hschain.Utxo.Lang.Core.Data.Output as O
import Examples.SKI
import Examples.Simple


tests :: TestTree
tests = testGroup "core"
  [ testGroup "simple"
    [ testProgram "spend to key" progSpendToKey (PrimBool True)
    , testProgram "Addition"     progAddition   (PrimInt 101)
    , testProgram "SKK3"         exampleSKK3    (PrimInt 3)
    ]
  , testGroup "primitives"
    [ testProgram "eq.Int"  (progEquality (PrimInt  12))    (PrimBool True)
    , testProgram "eq.Bool" (progEquality (PrimBool False)) (PrimBool True)
    , testProgram "eq.Int"  (progEquality (PrimText "12"))  (PrimBool True)
    ]
  , testGroup "env"
    [ testProgram "getHeight" progHeight (PrimInt 123)
    ]
  ]

testProgram :: String -> CoreProg -> Prim -> TestTree
testProgram nm prog res = testGroup nm
  [ testCase "typecheck" $ Nothing     @=? typeCheck preludeTypeContext prog
  , testCase "eval"      $ Right [res] @=? run (prog <> CoreProg (environmentFunctions env))
  ]


-- Trivial
progSpendToKey :: CoreProg
progSpendToKey = CoreProg
  [ mkMain $ Typed
    { typed'value = EPrim (PrimSigma (Fix $ SigmaBool True))
    , typed'type  = sigmaT
    }
  ]

progHeight :: CoreProg
progHeight = CoreProg
  [ mkMain $ Typed
    { typed'value = EVar (Typed "getHeight" intT)
    , typed'type  = intT
    }
  ]

progEquality :: Prim -> CoreProg
progEquality p = CoreProg
  [ mkMain $ Typed
    { typed'value =
        (EVar (Typed eq (funT [ty,ty] boolT)) `EAp` EPrim p) `EAp` EPrim p
    , typed'type  = boolT
    }
  ]
  where
    ty = primToType p
    eq = toCompareName ty "equals"


run :: CoreProg -> Either Error [Prim]
run
  = fmap (O.toList . gmachine'output)
  . eval
  . compile


----------------------------------------------------------------

env :: TxEnv
env = TxEnv
  { txEnv'height   = 123
  , txEnv'self     = Box
    { box'id     = BoxId ""
    , box'value  = 100
    , box'script = Script ""
    , box'args   = mempty
    }
  , txEnv'inputs   = mempty
  , txEnv'outputs  = mempty
  , txEnv'args     = mempty
  }

