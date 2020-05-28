-- | Built-in language primitives
module Hschain.Utxo.Lang.Core.Compile.Primitives(
    primitives
  , builtInUnary
  , builtInDiadic
) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Code (Instr(..))
import Hschain.Utxo.Lang.Core.Data.Prim

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
  , op1 "negate"
  -- comparision
  , op2 "=="
  , op2 "/="
  , op2 ">"
  , op2 ">="
  , op2 "<"
  , op2 "<="
  -- conditionals
  , op3 "if"
  -- booleans
  , constant "true"  (PrimBool True)
  , constant "false" (PrimBool False)
  , op2 "&&"
  , op2 "||"
  , op2 "^^"
  , op1 "not"
  -- text
  , op1 "lengthText"
  , op2 "<>"
  , op1 "hashBlake"
  , op1 "hashSha"
  -- sigma-expressions
  , op2 "&"
  , op2 "|"
  , op1 "pk"
  , op1 "toSigma"
  ]

-- | Application of function to two arguments
ap2 :: Expr -> Expr -> Expr -> Expr
ap2 f a b = EAp (EAp f a) b

-- | Application of function to three arguments
ap3 :: Expr -> Expr -> Expr -> Expr -> Expr
ap3 f a b c = EAp (ap2 f a b) c

intConstant :: Name -> Int -> Scomb
intConstant name val = constant name (PrimInt val)

constant :: Name -> Prim -> Scomb
constant name val = Scomb
  { scomb'name = name
  , scomb'args = V.empty
  , scomb'body = EPrim val
  }

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
  , ("&&", And)
  , ("||", Or)
  , ("^^", Xor)
  , ("&", SAnd)
  , ("|", SOr)
  , ("<>", TextAppend)
  ]

builtInUnary :: Map Name Instr
builtInUnary = M.fromList
  [ ("negate", Neg)
  , ("not", Not)
  , ("pk", Pk)
  , ("toSigma", SBool)
  , ("lengthText", TextLength)
  , ("hashBlake", HashBlake)
  , ("hashSha", HashSha)
  ]

