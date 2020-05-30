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
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Code (Instr(..))
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

-- | Built-in language primitives
primitives :: [Scomb]
primitives =
  -- numeric operators
  [ intOp2 "+"
  , intOp2 "*"
  , intOp2 "-"
  , intOp2 "/"
  , op1 "negate" intT intT

  -- comparision
  , compareOp "=="
  , compareOp "/="
  , compareOp ">"
  , compareOp ">="
  , compareOp "<"
  , compareOp "<="

  -- conditionals
  , op3 "if" (boolT, varA, varA) varA

  -- booleans
  , constant "true"  (PrimBool True)
  , constant "false" (PrimBool False)
  , boolOp2 "&&"
  , boolOp2 "||"
  , boolOp2 "^^"
  , op1 "not" boolT boolT

  -- text
  , op1 "lengthText" textT intT
  , op2 "<>" (textT, textT) textT
  , op1 "hashBlake" textT textT
  , op1 "hashSha" textT textT

  -- sigma-expressions
  , sigmaOp2 "&"
  , sigmaOp2 "|"
  , op1 "pk" textT sigmaT
  , op1 "toSigma" boolT sigmaT

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
  , scomb'body = Typed (EPrim val) (primToType val)
  }

op1 :: Name -> Type -> Type -> Scomb
op1 name argT resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList $ [Typed "x" argT]
  , scomb'body = Typed (EAp (EVar name) (EVar "x")) resT
  }

intOp2 :: Name -> Scomb
intOp2 name = op2 name (intT, intT) intT

boolOp2 :: Name -> Scomb
boolOp2 name = op2 name (boolT, boolT) boolT

sigmaOp2 :: Name -> Scomb
sigmaOp2 name = op2 name (sigmaT, sigmaT) sigmaT

-- | TODO: do we need polymorphic comparison?
compareOp :: Name -> Scomb
compareOp name = op2 name (intT, intT) boolT

op2 :: Name -> (Type, Type) -> Type -> Scomb
op2 name (xT, yT) resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [Typed "x" xT, Typed "y" yT]
  , scomb'body = Typed (ap2 (EVar name) (EVar "x") (EVar "y")) resT
  }

op3 :: Name -> (Type, Type, Type) -> Type -> Scomb
op3 name (xT, yT, zT) resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [Typed "x" xT, Typed "y" yT, Typed "z" zT]
  , scomb'body = Typed (ap3 (EVar name) (EVar "x") (EVar "y") (EVar "z")) resT
  }

-- lists

nil :: Scomb
nil = Scomb
  { scomb'name = "Nil"
  , scomb'args = V.empty
  , scomb'body = Typed (EConstr (listT varA) 0 0) (listT varA)
  }

cons :: Scomb
cons = Scomb
  { scomb'name = "Cons"
  , scomb'args = V.fromList [Typed "a" varA, Typed "as" (listT varA)]
  , scomb'body = Typed (ap2 (EConstr consTy 1 2) (EVar "a") (EVar "as")) (listT varA)
  }
  where
   consTy = arrow2 varA (listT varA) (listT varA)

head' :: Scomb
head' = Scomb
  { scomb'name = "head"
  , scomb'args = V.fromList [Typed "xs" (listT varA)]
  , scomb'body = Typed (ECase (Typed "xs" (listT varA))
      [ CaseAlt 1 [Typed "a" varA, Typed "as" (listT varA)] "a"]) varA

  }

tail' :: Scomb
tail' = Scomb
  { scomb'name = "head"
  , scomb'args = V.fromList [Typed "xs" (listT varA)]
  , scomb'body = Typed (ECase (Typed "xs" (listT varA))
      [ CaseAlt 1 [Typed "a" varA, Typed "as" (listT varA)] "as"]) (listT varA)
  }

foldr' :: Scomb
foldr' = Scomb
  { scomb'name = "foldr"
  , scomb'args = V.fromList
                    [ Typed "cons" (arrow2 varA varB varB)
                    , Typed "nil"  varB
                    , Typed "xs"   (listT varA)
                    ]
  , scomb'body =
      Typed
          (ECase (Typed "xs" (listT varA))
          [ CaseAlt 0 []                                         "nil"
          , CaseAlt 1 [Typed "a" varA, Typed "as" (listT varA)]  (ap2 "cons" "a" (ap3 "foldr" "cons" "nil" "as"))
          ])
          varA
  }

map' :: Scomb
map' = Scomb
  { scomb'name = "map"
  , scomb'args = V.fromList
                    [ Typed "f"  (arrowT varA varB)
                    , Typed "xs" (listT varA)]
  , scomb'body =
      Typed
        (ECase (Typed "xs" (listT varA))
        [ CaseAlt 0 []           "nil"
        , CaseAlt 1 [Typed "a" varA, Typed "as" (listT varA)]  (ap2 "cons" (EAp "f" "a") (ap2 "map" "f" "as"))
        ])
        (listT varB)
  }

concat' :: Scomb
concat' = Scomb
  { scomb'name = "concat"
  , scomb'args = V.fromList
                    [ Typed "as" (listT varA)
                    , Typed "bs" (listT varA)
                    ]
  , scomb'body =
      Typed
        (ECase (Typed "as" (listT varA))
        [ CaseAlt 0 []           "bs"
        , CaseAlt 1 [Typed "x" varA, Typed "xs" (listT varA)]  (ap2 "Cons" "x" (ap2 "concat" "xs" "bs"))
        ])
      (listT varA)
  }


filter' :: Scomb
filter' = Scomb
  { scomb'name = "filter"
  , scomb'args = V.fromList
                     [ Typed "predicate" (arrowT varA boolT)
                     , Typed "xs"        (listT varA) ]
  , scomb'body =
      Typed
        (ECase (Typed "xs" (listT varA))
                    [ CaseAlt 1 [] "Nil"
                    , CaseAlt 2 [ Typed "p"  varA
                                , Typed "ps" (listT varA)
                                ] (ELet [(Typed "rest" (listT varA), (ap2 "filter" "predicate" "ps"))]
                                            (ap3 "if"
                                                  (EAp "predicate" "p")
                                                  (ap2 "Cons" "p" "rest")
                                                  "rest"
                                            ))
                    ])
        (listT varA)
  }

foldl' :: Scomb
foldl' = Scomb
  { scomb'name = "foldl"
  , scomb'args = V.fromList
                    [ Typed "accum" (arrow2 varA varB varA)
                    , Typed "z"     varA
                    , Typed "xs"    (listT varB)
                    ]
  , scomb'body =
      Typed
        (ECase (Typed "xs" (listT varB))
        [ CaseAlt 0 []  "z"
        , CaseAlt 1 [Typed "a" varB, Typed "as" (listT varB)]  (ap3 "foldl" "accum" (ap2 "accum" "z" "a") "as")
        ])
        varA
  }

listAt :: Scomb
listAt = Scomb
  { scomb'name = "listAt"
  , scomb'args = V.fromList [Typed "n" intT, Typed "xs" (listT varA)]
  , scomb'body =
      Typed
        (ECase (Typed "xs" (listT varA))
          [CaseAlt 1 [Typed "a" varA, Typed "as" (listT varA)]
              (ap3 "if" (ap2 "<=" "n" (int 0))
                  "a"
                  (ap2 "listAt" (ap2 "-" "n" (int 1)) "as")
              )
          ])
      varA
  }

-- functions

id' :: Scomb
id' = Scomb
  { scomb'name = "id"
  , scomb'args = V.fromList [Typed "x" varA]
  , scomb'body = Typed "x" varA
  }

const' :: Scomb
const' = Scomb
  { scomb'name = "const"
  , scomb'args = V.fromList [Typed "x" varA, Typed "y" varB]
  , scomb'body = Typed "x" varA
  }

compose :: Scomb
compose = Scomb
  { scomb'name = "compose"
  , scomb'args = V.fromList
                    [ Typed "f" (arrowT varB varC)
                    , Typed "g" (arrowT varA varB)
                    , Typed "x" varA
                    ]
  , scomb'body = Typed (EAp "f" (EAp "g" "x")) varC
  }

-- tuples

tupleCons :: Int -> Scomb
tupleCons arity = Scomb
  { scomb'name = mappend "Tuple" (showt arity)
  , scomb'args = V.fromList $ zipWith Typed vs vTs
  , scomb'body = Typed (L.foldl' (\f arg -> EAp f (EVar arg)) (EConstr consTy 0 arity) vs) (tupleT vTs)
  }
  where
    vs = fmap (\n -> mappend "v" (showt n)) [1 .. arity]
    vTs = fmap varT vs
    consTy = funT vTs (tupleT vTs)

maxTupleSize :: Int
maxTupleSize = 13

tuples :: [Scomb]
tuples = fst' : snd' : fmap tupleCons [2 .. maxTupleSize]

fst' :: Scomb
fst' = Scomb
  { scomb'name = "fst"
  , scomb'args = V.fromList [Typed "x" argT]
  , scomb'body = Typed
      (ECase (Typed "x" argT) [CaseAlt 0 [Typed "a" varA, Typed "b" varB] "a"])
      varA
  }
  where
    argT = tupleT [varA, varB]

snd' :: Scomb
snd' = Scomb
  { scomb'name = "snd"
  , scomb'args = V.fromList [Typed "x" argT]
  , scomb'body =
      Typed
        (ECase (Typed "x" argT) [CaseAlt 0 [Typed "a" varA, Typed "b" varB] "b"])
        varB
  }
  where
    argT = tupleT [varA, varB]

-- boxes

-- | Low level representation of Box is a tuple of four elements:
-- > (name, script, value, args)
boxCons :: Scomb
boxCons = Scomb
  { scomb'name = "Box"
  , scomb'args = V.fromList boxArgs
  , scomb'body = Typed (ap (EConstr consTy 0 4) $ fmap (EVar . typed'value) boxArgs) boxT
  }
  where
    consTy = funT (fmap typed'type boxArgs) boxT

getBoxField :: Name -> Name -> Type -> Scomb
getBoxField name field resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [Typed "box" boxT]
  , scomb'body =
      Typed
        (ECase (Typed "box" boxT) [CaseAlt 0 boxArgs (EVar field)])
        resT
  }
  where

boxArgs :: [Typed Name]
boxArgs =
  [ Typed "name"   textT
  , Typed "script" textT
  , Typed "value"  intT
  , Typed "args"   (arrowT textT intT)] -- TODO: think out type for argument it's not only int

getBoxName :: Scomb
getBoxName = getBoxField "getBoxName" "name" textT

getBoxScript :: Scomb
getBoxScript = getBoxField "getBoxScript" "script" textT

getBoxValue :: Scomb
getBoxValue = getBoxField "getBoxValue" "value" intT

getBoxArgs :: Scomb
getBoxArgs = getBoxField "getBoxArgs" "args" (arrowT textT intT) -- TODO: think on result type of getArg

-- environment

-- | Low-level representation of environment is a tuple of four elements
-- > (blockchainHeight, inputList, outputList, argList)
envCons :: Scomb
envCons = Scomb
  { scomb'name = "Env"
  , scomb'args = V.fromList envArgs
  , scomb'body = Typed (ap (EConstr consTy 0 4) (fmap (EVar . typed'value) envArgs)) envT
  }
  where
    consTy = funT (fmap typed'type envArgs) envT

envArgs :: [Typed Name]
envArgs =
  [ Typed "height"  intT
  , Typed "inputs"  (listT boxT)
  , Typed "outputs" (listT boxT)
  , Typed "args"    (listT intT) -- TODO: reconsider type for args it can be not only int as a result
  ]

getEnvField :: Name -> Name -> Type -> Scomb
getEnvField name field resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [Typed "env" envT]
  , scomb'body =
      Typed
        (ECase (Typed "env" envT) [CaseAlt 0 envArgs (EVar field)])
        resT
  }

getHeight :: Scomb
getHeight = getEnvField "getHeight" "height" intT

getInputs :: Scomb
getInputs = getEnvField "getInputs" "inputs" (listT boxT)

getOutputs :: Scomb
getOutputs = getEnvField "getOutputs" "outputs" (listT boxT)

getArgs :: Scomb
getArgs = getEnvField "getArgs" "args" (listT intT)

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

arrow2 :: Type -> Type -> Type -> Type
arrow2 a b c = arrowT a (arrowT b c)

varA :: Type
varA = varT "a"

varB :: Type
varB = varT "b"

varC :: Type
varC = varT "c"

funT :: [Type] -> Type -> Type
funT args resT = L.foldl' (\res arg -> arrowT arg res) resT args

