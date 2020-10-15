-- |
module TM.Core ( tests )where

import Data.Fix
import Test.Tasty
import Test.Tasty.HUnit

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Types
import Examples.SKI
import Examples.Simple
import TM.Core.Common


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

shouldFail :: String -> Core BindName Name -> TestTree
shouldFail nm prog = testCase nm $ case typeCheck prog of
  Right _ -> assertFailure "Type checking should fail"
  Left  _ -> return ()


-- Trivial
progLiteral :: Prim -> Core BindName Name
progLiteral = EPrim

progHeight :: Core BindName Name
progHeight = EPrimOp OpEnvGetHeight

progEquality :: Prim -> Core BindName Name
progEquality p
  = EPrimOp (OpEQ ty) `EAp` EPrim p `EAp` EPrim p
  where
    ty = primToType p


progListCase :: Core BindName Name
progListCase
  =     EPrimOp OpAdd
  `EAp` safeHead nil
  `EAp` safeHead (cons `EAp` EPrim (PrimInt 123) `EAp` nil)
  where
    cons = EConstr (ListT IntT) 1
    nil  = EConstr (ListT IntT) 0
    safeHead e = ECase e
      [ alt 0 []          (EPrim (PrimInt 0))
      , alt 1 ["x", "xs"] (EVar "x")
      ]

badListCase :: Core BindName Name
badListCase = ECase nil
  [ alt 0 ["x"]       zero
  , alt 1 ["x", "xs"] zero
  ]
  where
    zero = EPrim   (PrimInt 0)
    nil  = EConstr (ListT IntT) 0
