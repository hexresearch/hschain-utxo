{-# OPTIONS_GHC -Wno-orphans #-}
-- | Built-in language primitives
module Hschain.Utxo.Lang.Core.Compile.Primitives(
    primitives
  , builtInUnary
  , builtInDiadic
  , preludeTypeContext
) where

import Data.Map.Strict (Map)
import Data.String

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Code (Instr(..))
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

preludeTypeContext :: TypeContext
preludeTypeContext = TypeContext $ M.fromList $
  fmap (\sc -> (scomb'name sc, getScombType sc)) primitives

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
  ]

ap :: Expr -> [Expr] -> Expr
ap f args = L.foldl' (\op a -> EAp op a) f args

-- | Application of function to two arguments
ap2 :: Expr -> Expr -> Expr -> Expr
ap2 f a b = EAp (EAp f a) b

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

