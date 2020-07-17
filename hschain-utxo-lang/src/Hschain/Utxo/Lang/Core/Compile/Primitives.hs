-- | Built-in language primitives
module Hschain.Utxo.Lang.Core.Compile.Primitives(
    preludeLib
  , primitives
  , builtInUnary
  , builtInDiadic
  , preludeTypeContext
  , toCompareName
  , environmentFunctions
) where

import Data.Fix
import Data.Int
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Expr (Box(..), BoxId(..), Script(..), Args(..), ArgType(..), argTypeName)
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Code (Instr(..))
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Types (TxEnv(..))

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Language.HM as H
import qualified Hschain.Utxo.Lang.Const as Const

preludeLib :: TxEnv -> CoreProg
preludeLib env = CoreProg $ environmentFunctions env ++ primitives

preludeTypeContext :: TypeContext
preludeTypeContext = primitivesCtx <> environmentTypes
  where
    primitivesCtx = TypeContext $ M.fromList $
      fmap (\sc -> (scomb'name sc, getScombType sc)) primitives

-- | Built-in functions that read environment.
-- We create set of global constants in the script
-- os that user can rely on them in the code.
--
-- So we create library functions that contain concrete
-- constants for current state of our blockchain.
environmentFunctions :: TxEnv -> [Scomb]
environmentFunctions TxEnv{..} =
  [ getHeight txEnv'height
  , getInputs txEnv'inputs
  , getOutputs txEnv'outputs
  ] ++ getArgs txEnv'args

environmentTypes :: TypeContext
environmentTypes = TypeContext $ M.fromList $
  [ (Const.getHeight,  intT)
  , (Const.getInputs,  listT boxT)
  , (Const.getOutputs, listT boxT)
  ] ++ getArgsTypes
  where
    getArgsTypes = fmap toArgType [ (IntArg, intT), (TextArg, textT), (BoolArg, boolT)]

    toArgType (typeTag, ty) = (Const.getArgs $ argTypeName typeTag, listT ty)

-- | Built-in language primitives
primitives :: [Scomb]
primitives =
  -- numeric operators
  [ intOp2 "+"
  , intOp2 "*"
  , intOp2 "-"
  , intOp2 "/"
  , op1 "negate" intT intT


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
  ]
  ++ (comparePack =<< [intT, boolT, textT])
  ++ getBoxArgs

------------------------------------------------------------
-- generic utilities

-- | comparision operators per type
comparePack :: TypeCore -> [Scomb]
comparePack ty =
  [ compareOp ty (toCompareName ty "equals")
  , compareOp ty (toCompareName ty "notEquals")
  , compareOp ty (toCompareName ty "greaterThan")
  , compareOp ty (toCompareName ty "greaterThanEquals")
  , compareOp ty (toCompareName ty "lessThan")
  , compareOp ty (toCompareName ty "lessThanEquals")
  ]

ap :: ExprCore -> [ExprCore] -> ExprCore
ap f args = L.foldl' (\op a -> EAp op a) f args

-- | Application of function to two arguments
ap2 :: ExprCore -> ExprCore -> ExprCore -> ExprCore
ap2 f a b = EAp (EAp f a) b

constant :: Name -> Prim -> Scomb
constant name val = Scomb
  { scomb'name = name
  , scomb'args = V.empty
  , scomb'body = Typed (EPrim val) (primToType val)
  }

op1 :: Name -> TypeCore -> TypeCore -> Scomb
op1 name argT resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList $ [Typed "x" argT]
  , scomb'body = Typed (EAp (EVar $ Typed name (arrowT argT resT)) (EVar $ Typed "x" argT)) resT
  }

intOp2 :: Name -> Scomb
intOp2 name = op2 name (intT, intT) intT

boolOp2 :: Name -> Scomb
boolOp2 name = op2 name (boolT, boolT) boolT

sigmaOp2 :: Name -> Scomb
sigmaOp2 name = op2 name (sigmaT, sigmaT) sigmaT

-- | TODO: do we need polymorphic comparison?
compareOp :: TypeCore -> Name -> Scomb
compareOp ty name = op2 name (ty, ty) boolT

op2 :: Name -> (TypeCore, TypeCore) -> TypeCore -> Scomb
op2 name (xT, yT) resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [Typed "x" xT, Typed "y" yT]
  , scomb'body = Typed (ap2 (EVar $ Typed name (funT [xT, yT] resT)) (EVar $ Typed "x" xT) (EVar $ Typed "y" yT)) resT
  }

------------------------------------------------------------
-- boxes

toCompareName :: TypeCore -> Name -> Name
toCompareName ty name = mconcat [primName ty, ".", name]
  where
    primName (H.Type (Fix x)) = case x of
      H.ConT _ prim _ -> prim
      _               -> error "Non-primitive type"

-- | Low level representation of Box is a tuple of four elements:
-- > (name, script, value, args)
boxCons :: Scomb
boxCons = Scomb
  { scomb'name = "Box"
  , scomb'args = V.fromList boxArgs
  , scomb'body = Typed (ap (EConstr consTy 0 4) $ fmap EVar boxArgs) boxT
  }
  where
    consTy = funT (fmap typed'type boxArgs) boxT

getBoxField :: Name -> Typed Name -> TypeCore -> Scomb
getBoxField name field resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [Typed "box" boxT]
  , scomb'body =
      Typed
        (ECase (Typed (EVar $ Typed "box" boxT) boxT) [CaseAlt 0 boxArgs (EVar field)])
        resT
  }
  where

boxArgs :: [Typed Name]
boxArgs =
  [ Typed "name"   textT
  , Typed "script" textT
  , Typed "value"  intT
  , Typed "args"   (tupleT argsTypes)
  ]

getBoxName :: Scomb
getBoxName = getBoxField Const.getBoxName (Typed "name" textT) textT

getBoxScript :: Scomb
getBoxScript = getBoxField Const.getBoxScript (Typed "script" textT) textT

getBoxValue :: Scomb
getBoxValue = getBoxField Const.getBoxValue (Typed "value" intT) intT

getBoxArgs :: [Scomb]
getBoxArgs = [ getBoxIntArgs, getBoxTextArgs, getBoxBoolArgs ]
  where
    getBoxIntArgs  = getBoxArgsBy IntArg  intT  "ints"
    getBoxTextArgs = getBoxArgsBy TextArg textT "texts"
    getBoxBoolArgs = getBoxArgsBy BoolArg boolT "bools"

    getBoxArgsBy typeTag resType resVar = Scomb
      { scomb'name = Const.getBoxArgs $ argTypeName typeTag
      , scomb'args = V.fromList [Typed "x" argT]
      , scomb'body = Typed
          (ECase (Typed (EVar $ Typed "x" argT) argT)
            [CaseAlt 0 [Typed "ints" (listT intT), Typed "texts" (listT textT), Typed "bools" (listT boolT)] (EVar $ Typed resVar resType)])
          (listT resType)
      }

    argT = tupleT argsTypes

boxConstr :: ExprCore -> ExprCore -> ExprCore -> ExprCore -> ExprCore
boxConstr name script value args = ap (EConstr consTy 0 4) [name, script, value, args]
  where
    consTy = funT (fmap typed'type boxArgs) boxT


toBox :: Box -> ExprCore
toBox Box{..} = boxConstr name script value args
  where
    name   = EPrim $ PrimText $ unBoxId box'id
    script = EPrim $ PrimText $ unScript box'script
    value  = EPrim $ PrimInt  $ box'value
    args   = toArgs box'args

toArgs :: Args -> ExprCore
toArgs Args{..} = ap (EConstr consTy 0 3) [ints, texts, bools]
  where
    consTy = funT argsTypes (tupleT argsTypes)
    ints   = toVec intT  $ fmap (EPrim . PrimInt)  args'ints
    texts  = toVec textT $ fmap (EPrim . PrimText) args'texts
    bools  = toVec boolT $ fmap (EPrim . PrimBool) args'bools

argsTypes :: [TypeCore]
argsTypes = [listT intT, listT textT, listT boolT]


------------------------------------------------------------
-- environment

getHeight :: Int64 -> Scomb
getHeight height = constant Const.getHeight (PrimInt height)

getInputs :: Vector Box -> Scomb
getInputs = getBoxes Const.getInputs

getOutputs :: Vector Box -> Scomb
getOutputs = getBoxes Const.getOutputs

getBoxes :: Text -> Vector Box -> Scomb
getBoxes name boxes = Scomb
  { scomb'name = name
  , scomb'args = V.empty
  , scomb'body = Typed
      (toVec boxT $ fmap toBox boxes)
      (listT boxT)
  }

toVec :: TypeCore -> Vector ExprCore -> ExprCore
toVec t vs = V.foldr cons nil vs
  where
    nil      = EConstr (listT t) 0 0
    cons a b = ap (EConstr consTy 1 2) [a, b]

    consTy = funT [t, listT t] (listT t)

getArgs :: Args -> [Scomb]
getArgs Args{..} =
  [ argComb PrimInt  intT  IntArg  args'ints
  , argComb PrimText textT TextArg args'texts
  , argComb PrimBool boolT BoolArg args'bools
  ]
  where
    argComb cons ty tyTag vals = Scomb
      { scomb'name = Const.getArgs $ argTypeName tyTag
      , scomb'args = V.empty
      , scomb'body = Typed
          (toVec ty $ fmap (EPrim . cons) vals)
          (listT ty)
      }

------------------------------------------------------------
-- prim ops

builtInDiadic :: Map Name Instr
builtInDiadic = M.fromList $
  [ ("+", Add)
  , ("*", Mul)
  , ("-", Sub)
  , ("/", Div)
  , ("&&", And)
  , ("||", Or)
  , ("^^", Xor)
  , ("&", SigAnd)
  , ("|", SigOr)
  , ("<>", TextAppend)
  ] ++ (compareNames =<< [intT, boolT, textT])
  where
    compareNames ty =
      [ (toCompareName ty "equals", Eq)
      , (toCompareName ty "notEquals", Ne)
      , (toCompareName ty "lessThan", Lt)
      , (toCompareName ty "lessThanEquals", Le)
      , (toCompareName ty "greaterThan", Gt)
      , (toCompareName ty "greaterThanEquals", Ge)
      ]

builtInUnary :: Map Name Instr
builtInUnary = M.fromList
  [ ("negate", Neg)
  , ("not", Not)
  , ("pk", SigPk)
  , ("toSigma", SigBool)
  , ("lengthText", TextLength)
  , ("hashBlake", HashBlake)
  , ("hashSha", HashSha)
  ]

