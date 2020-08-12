-- |
module TM.Core ( tests )where

import Data.Fix
import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Expr  (Box(..),BoxId(..),Script(..))
import Hschain.Utxo.Lang.Types (InputEnv(..))
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine
import Hschain.Utxo.Lang.Core.RefEval
import qualified Hschain.Utxo.Lang.Core.Data.Output as O
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
  ]

testProgram :: String -> CoreProg -> Prim -> TestTree
testProgram nm prog res = testGroup nm
  [ testCase "typecheck" $ Nothing     @=? typeCheck preludeTypeContext prog
  , testCase "eval"      $ Right [res] @=? run (prog <> CoreProg (environmentFunctions env))
  , testCase "simple"    $ Right res   @=? evalProg env prog
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
    { typed'value = EVar "getHeight"
    , typed'type  = intT
    }
  ]

progEquality :: Prim -> CoreProg
progEquality p = CoreProg
  [ mkMain $ Typed
    { typed'value =
        (EVar eq `EAp` EPrim p) `EAp` EPrim p
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

env :: InputEnv
env = InputEnv
  { inputEnv'height   = 123
  , inputEnv'self     = Box
    { box'id     = BoxId ""
    , box'value  = 100
    , box'script = Script ""
    , box'args   = mempty
    }
  , inputEnv'inputs   = mempty
  , inputEnv'outputs  = mempty
  , inputEnv'args     = mempty
  }

