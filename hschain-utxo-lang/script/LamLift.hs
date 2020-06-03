{-# Language OverloadedStrings #-}
-- | Test that lambda-lifting is working
module LamLift where

import Data.Fix
import Data.String (IsString(..))

import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.LambdaLifting
import Hschain.Utxo.Lang.Compile.LambdaLifting.Abstract
import Hschain.Utxo.Lang.Compile.LambdaLifting.Collect
import Hschain.Utxo.Lang.Compile.LambdaLifting.FreeVars
import Hschain.Utxo.Lang.Compile.LambdaLifting.Rename

import Text.Show.Pretty hiding (Name)

main :: IO ()
main = checkProg prog3

instance Num (Expr Name) where
  (+) = app2 "+"
  (*) = app2 "*"
  negate = app "negate"
  abs = undefined
  signum = undefined
  fromInteger = Fix . EPrim . PrimInt . fromInteger

instance IsString (Expr Name) where
  fromString = Fix . EVar . fromString

app :: Expr Name -> Expr Name -> Expr Name
app f a = Fix $ EAp f a

app2 :: Expr Name -> Expr Name -> Expr Name -> Expr Name
app2 f a b = Fix $ EAp (app f a) b

lam :: [Name] -> Expr Name -> Expr Name
lam args body = Fix $ ELam args body

prog1 :: CoreProg
prog1 =
  [ Def
      { def'name = "mul"
      , def'args = ["x", "y"]
      , def'body = "x" * "y"
      }

  , Def
      { def'name = "f"
      , def'args = ["x"]
      , def'body = Fix $ ELet
                          [ ("g", lam ["y"]
                                     (app2 "mul" "x" ("x" + "y")))]
                          (app "g" 3 + app "g" 4)
      }

  , Def
      { def'name = "main"
      , def'args = []
      , def'body = app "f" 6
      }
  ]

prog2 :: CoreProg
prog2 =
  [ Def
      { def'name = "f"
      , def'args = []
      , def'body = lam ["x"] $ lam ["y"] (("x" + 1) * "y")
      }

  , Def
      { def'name = "main"
      , def'args = []
      , def'body = app2 "f" 1 2
      }
  ]

prog3 :: CoreProg
prog3 =
  [ Def
      { def'name = "f"
      , def'args = ["x"]
      , def'body = Fix $ ELet [("g", lam ["y"] ("y" + 1))] (app "g" (app "g" "x"))
      }

  , Def
      { def'name = "main"
      , def'args = []
      , def'body = app "f" 1
      }
  ]

checkProg :: CoreProg -> IO ()
checkProg prog = do
  stage "Initial program:"       prog
  stage "Fuse lambda arguments:" prog0
  stage "Find free vars:"        prog1
  stage "Abstract:"              prog2
  stage "Rename:"                prog3
  stage "Collect:"               prog4
  where
    prog0 = fuseLams prog
    prog1 = annotateFreeVars prog0
    prog2 = abstract prog1
    prog3 = rename prog2
    prog4 = collect prog3

    stage msg val = do
      putStrLn ""
      putStrLn "----------------------"
      putStrLn msg
      pPrint val


