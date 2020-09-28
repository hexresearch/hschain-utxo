-- |
module TM.Core ( tests )where

import Data.Fix
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Expr  (Box(..),BoxId(..),Script(..))
import Hschain.Utxo.Lang.Types (InputEnv(..))
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.RefEval
import Examples.SKI
import Examples.Simple


tests :: TestTree
tests = testGroup "core"
  [ testGroup "Literal"
    [ testProgram nm (progLiteral p) p
    | (nm,p) <- [ ("sigma", PrimSigma $ Fix $ SigmaBool True)
                , ("bool" , PrimBool False)
                , ("int",   PrimInt  123)
                , ("text",  PrimText "XX")
                , ("bytes", PrimBytes "XX")
                ]
    ]
  , testGroup "simple"
    [ testProgram "Addition"     progAddition   (PrimInt 101)
    , testProgram "SKK3"         exampleSKK3    (PrimInt 3)
    ]
  , testGroup "primitives"
    [ testProgram "eq.Int"  (progEquality (PrimInt  12))    (PrimBool True)
    , testProgram "eq.Bool" (progEquality (PrimBool False)) (PrimBool True)
    , testProgram "eq.Text" (progEquality (PrimText "12"))  (PrimBool True)
    ]
  , testGroup "env"
    [ testProgram "getHeight" progHeight (PrimInt 123)
    ]
  , testGroup "case"
    [ testProgram "case of list" progListCase (PrimInt 123)
    , shouldFail "List bad pattern" badListCase
    ]
  ]

shouldFail :: String -> CoreProg -> TestTree
shouldFail nm prog = testCase nm $ case typeCheck prog of
  Nothing -> assertFailure "Type checking should fail"
  Just _  -> return ()


testProgram :: String -> CoreProg -> Prim -> TestTree
testProgram nm prog res = testGroup nm
  [ testCase "typecheck" $ Nothing       @=? typeCheck prog
  , testCase "simple"    $ EvalPrim res  @=? evalProg env prog
  ]


-- Trivial
progLiteral :: Prim -> CoreProg
progLiteral p = CoreProg
  [ mkMain $ Typed
    { typed'value = EPrim p
    , typed'type  = primToType p
    }
  ]

progHeight :: CoreProg
progHeight = CoreProg
  [ mkMain $ Typed
    { typed'value = EPrimOp OpEnvGetHeight
    , typed'type  = IntT
    }
  ]

progEquality :: Prim -> CoreProg
progEquality p = CoreProg
  [ mkMain $ Typed
    { typed'value = (EPrimOp (OpEQ ty) `EAp` EPrim p) `EAp` EPrim p
    , typed'type  = BoolT
    }
  ]
  where
    ty = primToType p


progListCase :: CoreProg
progListCase = CoreProg
  [ mkMain $ Typed
    { typed'value =
        EPrimOp OpAdd
          `EAp` safeHead nil
          `EAp` safeHead (cons `EAp` EPrim (PrimInt 123) `EAp` nil)
    , typed'type  = IntT
    }
  ]
  where
    cons = EConstr (ListT IntT) 1
    nil  = EConstr (ListT IntT) 0
    safeHead e = ECase e
      [ CaseAlt 0 [] (EPrim (PrimInt 0))
      , CaseAlt 1 ["x", "xs"] (EVar "x")
      ]

badListCase :: CoreProg
badListCase = CoreProg
  [ mkMain $ Typed
    { typed'value = ECase nil
        [ CaseAlt 0 ["x"]       zero
        , CaseAlt 1 ["x", "xs"] zero
        ]
    , typed'type  = IntT
    }
  ]
  where
    zero = EPrim (PrimInt 0)
    nil  = EConstr (ListT IntT) 0


----------------------------------------------------------------

env :: InputEnv
env = InputEnv
  { inputEnv'height   = 123
  , inputEnv'self     = Box
    { box'id     = BoxId $ hashBlob ""
    , box'value  = 100
    , box'script = Script ""
    , box'args   = mempty
    }
  , inputEnv'inputs   = mempty
  , inputEnv'outputs  = mempty
  , inputEnv'args     = mempty
  }
