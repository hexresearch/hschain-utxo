-- | Built-in language primitives
module Hschain.Utxo.Lang.Core.Compile.Primitives(
    primitives
  , isIntOp
  , builtInDiadic
) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Code (Instr(..))
import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Data.Map.Strict as M
import qualified Data.Set    as S
import qualified Data.Vector as V

-- | Built-in language primitives
primitives :: [Scomb]
primitives =
  -- numeric operators
  [ op2 "+"
  , op2 "*"
  , op2 "-"
  , op2 "/"
  -- comparision
  , op2 "=="
  , op2 "/="
  , op2 ">"
  , op2 ">="
  , op2 "<"
  , op2 "<="
  -- conditionals
  , op3 "if"
  ]

-- | Application of function to two arguments
ap2 :: Expr -> Expr -> Expr -> Expr
ap2 f a b = EAp (EAp f a) b

-- | Application of function to three arguments
ap3 :: Expr -> Expr -> Expr -> Expr -> Expr
ap3 f a b c = EAp (ap2 f a b) c

op1 :: Name -> Scomb
op1 name = Scomb
  { scomb'name = name
  , scomb'args = V.fromList ["x"]
  , scomb'body = EAp (EVar name) (EVar "x")
  }

op2 :: Name -> Scomb
op2 name = Scomb
  { scomb'name = name
  , scomb'args = V.fromList ["x", "y"]
  , scomb'body = ap2 (EVar name) (EVar "x") (EVar "y")
  }

op3 :: Name -> Scomb
op3 name = Scomb
  { scomb'name = name
  , scomb'args = V.fromList ["x", "y", "z"]
  , scomb'body = ap3 (EVar name) (EVar "x") (EVar "y") (EVar "z")
  }

-- | Check if primitive operation is defined on integers
isIntOp :: Name -> Bool
isIntOp name = S.member name intOps

intOps :: Set Name
intOps = S.fromList ["+", "*", "-", "/"]

builtInDiadic :: Map Name Instr
builtInDiadic = M.fromList
  [ ("+", Add)
  , ("*", Mul)
  , ("-", Sub)
  , ("/", Div)
  , ("==", Eq)
  , ("/=", Ne)
  , ("<", Lt)
  , ("<=", Le)
  , (">", Gt)
  , (">=", Ge)
  ]


