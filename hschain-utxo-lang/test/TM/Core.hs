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

shouldFail :: String -> Core Name -> TestTree
shouldFail nm prog = testCase nm $ case typeCheck prog of
  Right _ -> assertFailure "Type checking should fail"
  Left  _ -> return ()


-- Trivial
progLiteral :: Prim -> Core Name
progLiteral = EPrim

progHeight :: Core Name
progHeight = EPrimOp OpEnvGetHeight

progEquality :: Prim -> Core Name
progEquality p
  = EPrimOp (OpEQ ty) `EAp` EPrim p `EAp` EPrim p
  where
    ty = primToType p


progListCase :: Core Name
progListCase
  =     EPrimOp OpAdd
  `EAp` safeHead nil
  `EAp` safeHead (cons `EAp` EPrim (PrimInt 123) `EAp` nil)
  where
    cons = EConstr (ConCons IntT)
    nil  = EConstr (ConNil  IntT)
    safeHead e = ECase e
      [ alt (ConNil IntT)  []          (EPrim (PrimInt 0))
      , alt (ConCons IntT) ["x", "xs"] (EVar "x")
      ]

badListCase :: Core Name
badListCase = ECase nil
  [ alt (ConNil IntT)  ["x"]       zero
  , alt (ConCons IntT) ["x", "xs"] zero
  ]
  where
    zero = EPrim   (PrimInt 0)
    nil  = EConstr (ConNil IntT)
