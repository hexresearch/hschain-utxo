{-# OPTIONS_GHC -Wno-orphans #-}
-- | Built-in language primitives
module Hschain.Utxo.Lang.Core.Compile.Primitives(
    primitives
  , builtInUnary
  , builtInDiadic
) where

import Hex.Common.Text

import Data.Map.Strict (Map)
import Data.String

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Code (Instr(..))
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.List as L
import qualified Data.Map.Strict as M
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

  -- lists
  , cons
  , nil
  , map'
  , concat'
  , filter'
  , foldl'
  , foldr'
  , listAt
  , head'
  , tail'

  -- functions
  , id'
  , const'
  , compose

  -- boxes
  , boxCons
  , getBoxName
  , getBoxScript
  , getBoxValue
  , getBoxArgs

  -- environment
  , envCons
  , getHeight
  , getInputs
  , getOutputs
  , getArgs
  ] ++ tuples

ap :: Expr -> [Expr] -> Expr
ap f args = L.foldl' (\op a -> EAp op a) f args

-- | Application of function to two arguments
ap2 :: Expr -> Expr -> Expr -> Expr
ap2 f a b = EAp (EAp f a) b

-- | Application of function to three arguments
ap3 :: Expr -> Expr -> Expr -> Expr -> Expr
ap3 f a b c = EAp (ap2 f a b) c

int :: Int -> Expr
int = EPrim . PrimInt

instance IsString Expr where
  fromString = EVar . fromString

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

-- lists

nil :: Scomb
nil = Scomb
  { scomb'name = "Nil"
  , scomb'args = V.empty
  , scomb'body = EConstr 0 0
  }

cons :: Scomb
cons = Scomb
  { scomb'name = "Cons"
  , scomb'args = V.fromList ["a", "as"]
  , scomb'body = ap2 (EConstr 1 2) (EVar "a") (EVar "as")
  }

head' :: Scomb
head' = Scomb
  { scomb'name = "head"
  , scomb'args = V.fromList ["xs"]
  , scomb'body = ECase "xs" [ CaseAlt 1 ["a", "as"] "a"]
  }

tail' :: Scomb
tail' = Scomb
  { scomb'name = "head"
  , scomb'args = V.fromList ["xs"]
  , scomb'body = ECase "xs" [ CaseAlt 1 ["a", "as"] "as"]
  }

foldr' :: Scomb
foldr' = Scomb
  { scomb'name = "foldr"
  , scomb'args = V.fromList ["cons", "nil", "xs"]
  , scomb'body = ECase "xs"
      [ CaseAlt 0 []           "nil"
      , CaseAlt 1 ["a", "as"]  (ap2 "cons" "a" "as")
      ]
  }

map' :: Scomb
map' = Scomb
  { scomb'name = "map"
  , scomb'args = V.fromList ["f", "xs"]
  , scomb'body = ECase "xs"
      [ CaseAlt 0 []           "nil"
      , CaseAlt 1 ["a", "as"]  (ap2 "cons" (EAp "f" "a") (ap2 "map" "f" "as"))
      ]
  }

concat' :: Scomb
concat' = Scomb
  { scomb'name = "concat"
  , scomb'args = V.fromList ["as", "bs"]
  , scomb'body = ECase "as"
      [ CaseAlt 0 []           "bs"
      , CaseAlt 1 ["x", "xs"]  (ap2 "Cons" "x" (ap2 "concat" "xs" "bs"))
      ]
  }


filter' :: Scomb
filter' = Scomb
  { scomb'name = "filter"
  , scomb'args = V.fromList ["predicate", "xs"]
  , scomb'body =
      (ECase "xs" [ CaseAlt 1 [] "Nil"
                  , CaseAlt 2 ["p", "ps"] (ELet [("rest", (ap2 "filter" "predicate" "ps"))]
                                           (ap3 "if"
                                                 (EAp "predicate" "p")
                                                 (ap2 "Cons" "p" "rest")
                                                 "rest"
                                           ))
                  ])
  }

foldl' :: Scomb
foldl' = Scomb
  { scomb'name = "foldl"
  , scomb'args = V.fromList ["accum", "z", "xs"]
  , scomb'body = ECase "xs"
      [ CaseAlt 0 []  "z"
      , CaseAlt 1 ["a", "as"]  (ap3 "foldl" "accum" (ap2 "accum" "z" "a") "as")
      ]
  }

listAt :: Scomb
listAt = Scomb
  { scomb'name = "listAt"
  , scomb'args = V.fromList ["n", "xs"]
  , scomb'body = ECase "xs"
      [CaseAlt 1 ["a", "as"]
          (ap3 "if" (ap2 "<=" "n" (int 0))
               "a"
               (ap2 "listAt" (ap2 "-" "n" (int 1)) "as")
          )
      ]
  }

-- functions

id' :: Scomb
id' = Scomb
  { scomb'name = "id"
  , scomb'args = V.fromList ["x"]
  , scomb'body = "x"
  }

const' :: Scomb
const' = Scomb
  { scomb'name = "const"
  , scomb'args = V.fromList ["x", "y"]
  , scomb'body = "x"
  }

compose :: Scomb
compose = Scomb
  { scomb'name = "compose"
  , scomb'args = V.fromList ["f", "g", "x"]
  , scomb'body = EAp "f" (EAp "g" "x")
  }

-- tuples

tupleCons :: Int -> Scomb
tupleCons arity = Scomb
  { scomb'name = mappend "Tuple" (showt arity)
  , scomb'args = V.fromList vs
  , scomb'body = L.foldl' (\f arg -> EAp f (EVar arg)) (EConstr 0 arity) vs
  }
  where
    vs = fmap (\n -> mappend "v" (showt n)) [1 .. arity]

maxTupleSize :: Int
maxTupleSize = 13

tuples :: [Scomb]
tuples = fst' : snd' : fmap tupleCons [2 .. maxTupleSize]

fst' :: Scomb
fst' = Scomb
  { scomb'name = "fst"
  , scomb'args = V.fromList ["x"]
  , scomb'body = ECase "x" [CaseAlt 0 ["a", "b"] "a"]
  }

snd' :: Scomb
snd' = Scomb
  { scomb'name = "snd"
  , scomb'args = V.fromList ["x"]
  , scomb'body = ECase "x" [CaseAlt 0 ["a", "b"] "b"]
  }

-- boxes

-- | Low level representation of Box is a tuple of four elements:
-- > (name, script, value, args)
boxCons :: Scomb
boxCons = Scomb
  { scomb'name = "Box"
  , scomb'args = V.fromList boxArgs
  , scomb'body = ap (EConstr 0 4) boxArgs
  }

getBoxField :: Name -> Name -> Scomb
getBoxField name field = Scomb
  { scomb'name = name
  , scomb'args = V.fromList ["box"]
  , scomb'body = ECase (EVar "box") [CaseAlt 0 boxArgs (EVar field)]
  }
  where

boxArgs :: IsString a => [a]
boxArgs = ["name", "script", "value", "args"]

getBoxName :: Scomb
getBoxName = getBoxField "getBoxName" "name"

getBoxScript :: Scomb
getBoxScript = getBoxField "getBoxScript" "script"

getBoxValue :: Scomb
getBoxValue = getBoxField "getBoxValue" "value"

getBoxArgs :: Scomb
getBoxArgs = getBoxField "getBoxArgs" "args"

-- environment

-- | Low-level representation of environment is a tuple of four elements
-- > (blockchainHeight, inputList, outputList, argList)
envCons :: Scomb
envCons = Scomb
  { scomb'name = "Env"
  , scomb'args = V.fromList envArgs
  , scomb'body = ap (EConstr 0 4) envArgs
  }

envArgs :: IsString a => [a]
envArgs = ["height", "inputs", "outputs", "args"]

getEnvField :: Name -> Name -> Scomb
getEnvField name field = Scomb
  { scomb'name = name
  , scomb'args = V.fromList ["env"]
  , scomb'body = ECase "env" [CaseAlt 0 envArgs (EVar field)]
  }

getHeight :: Scomb
getHeight = getEnvField "getHeight" "height"

getInputs :: Scomb
getInputs = getEnvField "getInputs" "inputs"

getOutputs :: Scomb
getOutputs = getEnvField "getOutputs" "outputs"

getArgs :: Scomb
getArgs = getEnvField "getArgs" "args"

