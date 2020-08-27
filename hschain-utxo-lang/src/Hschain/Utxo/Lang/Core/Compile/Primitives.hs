{-# Language OverloadedLists #-}
-- | Built-in language primitives
module Hschain.Utxo.Lang.Core.Compile.Primitives(
    preludeLib
  , primitives
  , preludeTypeContext
  , environmentFunctions
) where

import Data.Text (Text)
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Expr (Box(..), BoxId(..), Script(..), Args(..), ArgType(..), argTypeName, argTypes)
import Hschain.Utxo.Lang.Core.Compile.Build
  hiding (getBoxId, getBoxScript, getBoxValue, getSelf, getInputs, getOutputs)
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Types (InputEnv(..))

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Language.HM (monoT)

import qualified Hschain.Utxo.Lang.Const as Const

fromArgType :: ArgType -> TypeCore
fromArgType = \case
  IntArg   -> intT
  BoolArg  -> boolT
  TextArg  -> textT
  BytesArg -> bytesT

preludeLib :: InputEnv -> CoreProg
preludeLib env = CoreProg $ environmentFunctions env ++ primitives

preludeTypeContext :: TypeContext
preludeTypeContext = primitivesCtx <> environmentTypes
  where
    primitivesCtx = TypeContext $ M.fromList $
      fmap (\sc -> (scomb'name sc, getScombSignature sc)) primitives

-- | Built-in functions that read environment.
-- We create set of global constants in the script
-- os that user can rely on them in the code.
--
-- So we create library functions that contain concrete
-- constants for current state of our blockchain.
environmentFunctions :: InputEnv -> [Scomb]
environmentFunctions InputEnv{..} =
  [ getSelf inputEnv'self
  , getInputs inputEnv'inputs
  , getOutputs inputEnv'outputs
  ] ++ getArgs inputEnv'args

environmentTypes :: TypeContext
environmentTypes = TypeContext $ M.fromList $
  [ (Const.getSelf,    monoT boxT)
  , (Const.getInputs,  monoT $ listT boxT)
  , (Const.getOutputs, monoT $ listT boxT)
  ] ++ getArgsTypes
  where
    getArgsTypes = fmap toArgType argTypes

    toArgType typeTag = (Const.getArgs $ argTypeName typeTag, monoT $ listT $ fromArgType typeTag)

-- | Built-in language primitives
primitives :: [Scomb]
primitives =
  [ -- lists
    nilComb
  , consComb
  , sumComb
  , andComb
  , orComb
  , allComb
  , anyComb
  -- boxes
  , boxCons
  , getBoxId
  , getBoxScript
  , getBoxValue
  ]
  ++ getBoxArgs


------------------------------------------------------------
-- boxes

-- | Low level representation of Box is a tuple of four elements:
-- > (name, script, value, args)
boxCons :: Scomb
boxCons = Scomb
  { scomb'name   = "Box"
  , scomb'forall = []
  , scomb'args   = V.fromList boxArgs
  , scomb'body   = Typed (ap (EConstr consTy 0 4) $ fmap (EVar . typed'value) boxArgs) boxT
  }
  where
    consTy = funT (fmap typed'type boxArgs) boxT

getBoxField :: Name -> Name -> TypeCore -> Scomb
getBoxField name field resT = Scomb
  { scomb'name   = name
  , scomb'forall = []
  , scomb'args   = [Typed "box" boxT]
  , scomb'body   =
      Typed
        (ECase "box" [CaseAlt 0 boxArgs (EVar field)])
        resT
  }
  where

boxArgs :: [Typed Name]
boxArgs =
  [ Typed "name"   bytesT
  , Typed "script" bytesT
  , Typed "value"  intT
  , Typed "args"   argsT
  ]

getBoxId :: Scomb
getBoxId = getBoxField Const.getBoxId "name" bytesT

getBoxScript :: Scomb
getBoxScript = getBoxField Const.getBoxScript "script" bytesT

getBoxValue :: Scomb
getBoxValue = getBoxField Const.getBoxValue "value" intT

getBoxArgs :: [Scomb]
getBoxArgs =
  [ getBoxArgsBy IntArg   "ints"
  , getBoxArgsBy TextArg  "texts"
  , getBoxArgsBy BoolArg  "bools"
  , getBoxArgsBy BytesArg "bytes"
  ]
  where
    getBoxArgsBy typeTag resVar = Scomb
      { scomb'name   = Const.getBoxArgs $ argTypeName typeTag
      , scomb'forall = []
      , scomb'args   = [Typed "x" boxT]
      , scomb'body   = Typed
          (onBox $ onArgs $ EVar resVar)
          (listT resType)
      }
      where
        resType = fromArgType typeTag
        onBox e  = ECase "x" [CaseAlt 0 boxArgs e]
        onArgs e = ECase "args" [CaseAlt 0 [Typed "ints" (listT intT), Typed "texts" (listT textT), Typed "bools" (listT boolT)] e]

boxConstr :: ExprCore -> ExprCore -> ExprCore -> ExprCore -> ExprCore
boxConstr name script value args = ap (EConstr consTy 0 4) [name, script, value, args]
  where
    consTy = funT (fmap typed'type boxArgs) boxT


toBox :: Box -> ExprCore
toBox Box{..} = boxConstr name script value args
  where
    name   = EPrim $ PrimBytes $ unBoxId box'id
    script = EPrim $ PrimBytes $ unScript box'script
    value  = EPrim $ PrimInt   $ box'value
    args   = toArgs box'args

toArgs :: Args -> ExprCore
toArgs Args{..} = ap (EConstr consTy 0 3) [ints, texts, bools]
  where
    consTy = funT argsTypes (tupleT argsTypes)
    ints   = toVec intT  $ fmap (EPrim . PrimInt)  args'ints
    texts  = toVec textT $ fmap (EPrim . PrimText) args'texts
    bools  = toVec boolT $ fmap (EPrim . PrimBool) args'bools


------------------------------------------------------------
-- environment

getSelf :: Box -> Scomb
getSelf b = constantComb "getSelf" boxT $ toBox b

getInputs :: Vector Box -> Scomb
getInputs = getBoxes Const.getInputs

getOutputs :: Vector Box -> Scomb
getOutputs = getBoxes Const.getOutputs

getBoxes :: Text -> Vector Box -> Scomb
getBoxes name boxes = constantComb name (listT boxT) (toVec boxT $ fmap toBox boxes)

toVec :: TypeCore -> Vector ExprCore -> ExprCore
toVec t vs = V.foldr cons nil vs
  where
    nil      = EConstr (listT t) 0 0
    cons a b = ap (EConstr consTy 1 2) [a, b]

    consTy = funT [t, listT t] (listT t)

getArgs :: Args -> [Scomb]
getArgs Args{..} =
  [ argComb PrimInt   IntArg   args'ints
  , argComb PrimText  TextArg  args'texts
  , argComb PrimBool  BoolArg  args'bools
  , argComb PrimBytes BytesArg args'bytes
  ]
  where
    argComb cons tyTag vals = constantComb (Const.getArgs $ argTypeName tyTag) (listT ty) (toVec ty $ fmap (EPrim . cons) vals)
      where
        ty = fromArgType tyTag

------------------------------------------------------------
-- lists

nilComb :: Scomb
nilComb = constantComb "nil" nilTy (EConstr nilTy 0 0)
  where
    nilTy = listT (varT "a")

consComb :: Scomb
consComb = Scomb
  { scomb'name   = "cons"
  , scomb'forall = ["a"]
  , scomb'args   = [x, xs]
  , scomb'body   = Typed
      (ap (EConstr consT 1 2) ["x", "xs"])
      asT
  }
  where
    x  = Typed "x" aT
    xs = Typed "xs" asT
    consT = funT [aT, asT] asT

    aT  = varT "a"
    asT = listT aT


genFoldrComb :: TypeCore -> TypeCore -> ExprCore -> ExprCore -> Name -> Scomb
genFoldrComb aT bT f z name = Scomb
  { scomb'name   = name
  , scomb'forall = []
  , scomb'args   = [as]
  , scomb'body   = Typed
      (ap (EPrimOp (OpListFoldr aT bT)) [f, z, "as"])
      bT
  }
  where
    as = Typed "as" (listT aT)


sumComb :: Scomb
sumComb = genFoldrComb intT intT (EPrimOp OpAdd) zero "sum"

orComb :: Scomb
orComb = genFoldrComb boolT boolT (EPrimOp OpBoolOr) (bool False) "or"

andComb :: Scomb
andComb = genFoldrComb boolT boolT (EPrimOp OpBoolAnd) (bool True) "and"

genFoldrMapComb :: TypeCore -> ExprCore -> ExprCore -> Name -> Scomb
genFoldrMapComb bT append z name = Scomb
  { scomb'name   = name
  , scomb'forall = ["a"]
  , scomb'args   = [f, as]
  , scomb'body   = Typed
      (ECase "as"
        [ CaseAlt 0 [] z
        , CaseAlt 1 [x, xs] (ap append [EAp "f" "x", ap nameV ["f", "xs"]])
        ])
      bT
  }
  where
    f = Typed "f" fT
    as = Typed "as" asT
    x = Typed "x" aT
    xs = Typed "xs" asT

    nameV  = EVar name

    fT = aT `arrowT` bT
    aT = varT "a"
    asT = listT aT

allComb :: Scomb
allComb = genFoldrMapComb boolT (EPrimOp OpBoolAnd) (bool True) "all"

anyComb :: Scomb
anyComb = genFoldrMapComb boolT (EPrimOp OpBoolOr) (bool False) "any"

zero :: ExprCore
zero = EPrim $ PrimInt 0
