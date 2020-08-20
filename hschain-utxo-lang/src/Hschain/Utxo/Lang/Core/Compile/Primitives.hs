{-# Language OverloadedLists #-}
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

import Data.Int
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Expr (Box(..), BoxId(..), Script(..), Args(..), ArgType(..), argTypeName, argTypes)
import Hschain.Utxo.Lang.Core.Compile.Build
  hiding (getBoxId, getBoxScript, getBoxValue, getHeight, getSelf, getInputs, getOutputs)
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Code (Instr(..))
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
  [ getHeight inputEnv'height
  , getSelf inputEnv'self
  , getInputs inputEnv'inputs
  , getOutputs inputEnv'outputs
  ] ++ getArgs inputEnv'args

environmentTypes :: TypeContext
environmentTypes = TypeContext $ M.fromList $
  [ (Const.getHeight,  monoT intT)
  , (Const.getSelf,    monoT boxT)
  , (Const.getInputs,  monoT $ listT boxT)
  , (Const.getOutputs, monoT $ listT boxT)
  ] ++ getArgsTypes
  where
    getArgsTypes = fmap toArgType argTypes

    toArgType typeTag = (Const.getArgs $ argTypeName typeTag, monoT $ listT $ fromArgType typeTag)

-- | Built-in language primitives
primitives :: [Scomb]
primitives =
  -- numeric operators
  [ intOp2 "*"
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
  , op1 "hashBlake" bytesT bytesT
  , op1 "hashSha"   bytesT bytesT

  -- lists
  , nilComb
  , consComb
  , foldrComb
  , mapComb
  , listAppendComb
  , listAtComb
  , filterComb
  , foldlComb
  , lengthComb
  , sumComb
  , productComb
  , andComb
  , orComb
  , sigmaAndComb
  , sigmaOrComb
  , allComb
  , anyComb
  , sigmaAllComb
  , sigmaAnyComb

  -- sigma-expressions
  , sigmaOp2 "&&&"
  , sigmaOp2 "|||"
  , op1 "pk" textT sigmaT
  , op1 "toSigma" boolT sigmaT

  -- boxes
  , boxCons
  , getBoxId
  , getBoxScript
  , getBoxValue
  ]
  ++ (comparePack =<< argTypes)
  ++ getBoxArgs
  ++ byteCombs


------------------------------------------------------------
-- generic utilities

-- | comparision operators per type
comparePack :: ArgType -> [Scomb]
comparePack tyTag =
  [ compareOp ty (toCompareName ty "equals")
  , compareOp ty (toCompareName ty "notEquals")
  , compareOp ty (toCompareName ty "greaterThan")
  , compareOp ty (toCompareName ty "greaterThanEquals")
  , compareOp ty (toCompareName ty "lessThan")
  , compareOp ty (toCompareName ty "lessThanEquals")
  ]
  where
    ty = fromArgType tyTag

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

getHeight :: Int64 -> Scomb
getHeight height = constant Const.getHeight (PrimInt height)

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
-- bytes

byteCombs :: [Scomb]
byteCombs = appendByteComb : sha256 : (fmap toBytes argTypes ++ fmap fromBytes argTypes)

appendByteComb :: Scomb
appendByteComb = op2 Const.appendBytes (bytesT, bytesT) bytesT

sha256 :: Scomb
sha256 = op1 Const.sha256 bytesT bytesT

toBytes :: ArgType -> Scomb
toBytes tag = op1 (Const.serialiseBytes $ argTypeName tag) (fromArgType tag) bytesT

fromBytes :: ArgType -> Scomb
fromBytes tag = op1 (Const.deserialiseBytes $ argTypeName tag) bytesT (fromArgType tag)

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

foldrComb :: Scomb
foldrComb = Scomb
  { scomb'name   = "foldr"
  , scomb'forall = ["a", "b"]
  , scomb'args   = [f, z, as]
  , scomb'body   = Typed
      (ECase "as"
        [ CaseAlt 0 [] "z"
        , CaseAlt 1 [x, xs] (ap "f" ["x", ap "foldr" ["f", "z", "xs"]])
        ])
      bT
  }
  where
    f = Typed "f" fT
    z = Typed "z" zT
    as = Typed "as" asT
    x = Typed "x" aT
    xs = Typed "xs" asT

    fT = aT `arrowT` (bT `arrowT` bT)
    asT = listT aT
    zT = bT
    aT = varT "a"
    bT = varT "b"

lengthComb :: Scomb
lengthComb = Scomb
  { scomb'name   = Const.length
  , scomb'forall = ["a"]
  , scomb'args   = [Typed "as" (listT aT)]
  , scomb'body   = Typed
      (ECase "as"
        [ CaseAlt 0 [] (EPrim $ PrimInt 0)
        , CaseAlt 1 [x, xs] (add one (EAp (EVar Const.length) "xs"))
        ])
      intT
  }
  where
    aT  = varT "a"

    x      = Typed "x" aT
    xs     = Typed "xs" (listT aT)

mapComb :: Scomb
mapComb = Scomb
  { scomb'name   = Const.map
  , scomb'forall = ["a", "b"]
  , scomb'args   = [f, as]
  , scomb'body   = Typed
      (ECase "as"
        [ CaseAlt 0 [] (EConstr nilT 0 0)
        , CaseAlt 1 [x, xs] (ap (EConstr consT 1 2) [EAp "f" "x", ap "map" ["f", "xs"]])
        ])
      bsT
  }
  where
    f      = Typed "f" fT
    as     = Typed "as" asT
    x      = Typed "x" aT
    xs     = Typed "xs" asT

    nilT = listT aT
    consT = bT `arrowT` (listT bT `arrowT` listT bT)
    fT = aT `arrowT` bT
    asT = listT aT
    bsT = listT bT
    aT = varT "a"
    bT = varT "b"

listAtComb :: Scomb
listAtComb = Scomb
  { scomb'name   = Const.listAt
  , scomb'forall = ["a"]
  , scomb'args   = [as, n]
  , scomb'body   = Typed

      (ECase "as"
        [ CaseAlt 0 [] EBottom
        , CaseAlt 1 [x, xs] (EIf (lessThanEquals intT "n" zero) "x" (ap (EVar Const.listAt) ["xs", sub "n" one]))
        ])
      aT
  }
  where
    n      = Typed "n" intT
    as     = Typed "as" asT
    x      = Typed "x" aT
    xs     = Typed "xs" asT

    asT = listT aT
    aT = varT "a"

listAppendComb :: Scomb
listAppendComb = Scomb
  { scomb'name   = Const.appendList
  , scomb'forall = ["a"]
  , scomb'args   = [as, bs]
  , scomb'body   = Typed
      (ECase "as"
        [ CaseAlt 0 [] "bs"
        , CaseAlt 1 [x, xs] (ap (EConstr consT 1 2) ["x", ap (EVar Const.appendList) ["xs", "bs"]])
        ])
      asT
  }
  where
    as     = Typed "as" asT
    bs     = Typed "bs" asT
    x      = Typed "x" aT
    xs     = Typed "xs" asT

    consT = aT `arrowT` (listT aT `arrowT` listT aT)
    asT = listT aT
    aT = varT "a"


filterComb :: Scomb
filterComb = Scomb
  { scomb'name   = Const.filter
  , scomb'forall = ["a"]
  , scomb'args   = [f, as]
  , scomb'body   = Typed
      (ECase "as"
        [ CaseAlt 0 [] (EConstr nilT 0 0)
        , CaseAlt 1 [x, xs]
            (ELet "ys" (ap "filter" ["f", "xs"])
                  (EIf (EAp "f" "x")
                       (ap (EConstr consT 1 2) ["x", "ys"])
                       "ys"
                  ))
        ])
      asT
  }
  where
    f      = Typed "f" fT
    as     = Typed "as" asT
    x      = Typed "x" aT
    xs     = Typed "xs" asT

    nilT = listT aT
    consT = aT `arrowT` (listT aT `arrowT` listT aT)
    fT = aT `arrowT` boolT
    asT = listT aT
    aT = varT "a"

foldlComb :: Scomb
foldlComb = Scomb
  { scomb'name   = Const.foldl
  , scomb'forall = ["a", "b"]
  , scomb'args   = [f, z, as]
  , scomb'body   = Typed
      (ECase "as"
        [ CaseAlt 0 [] "z"
        , CaseAlt 1 [x, xs] (ap foldlv ["f", ap "f" ["z", "x"], "xs"])
        ])
      bT
  }
  where
    f  = Typed "f" fT
    z  = Typed "z" zT
    as = Typed "as" asT
    x  = Typed "x"  aT
    xs = Typed "xs" asT

    foldlv = EVar Const.foldl

    asT = listT asT
    zT  = bT
    fT  = bT `arrowT` (aT `arrowT` bT)

    aT = varT "a"
    bT = varT "b"


genFoldrComb :: TypeCore -> TypeCore -> ExprCore -> ExprCore -> Name -> Scomb
genFoldrComb aT bT f z name = Scomb
  { scomb'name   = name
  , scomb'forall = []
  , scomb'args   = [as]
  , scomb'body   = Typed
      (ap foldrV [f, z, "as"])
      bT
  }
  where
    as = Typed "as" (listT aT)
    foldrV = EVar Const.foldr

sumComb :: Scomb
sumComb = genFoldrComb intT intT "+" zero "sum"

productComb :: Scomb
productComb = genFoldrComb intT intT "*" one "product"

orComb :: Scomb
orComb = genFoldrComb boolT boolT "||" (bool False) "or"

andComb :: Scomb
andComb = genFoldrComb boolT boolT "&&" (bool True) "and"

sigmaOrComb :: Scomb
sigmaOrComb = genFoldrComb sigmaT sigmaT sigmaOrV (sigmaBool False) "sigmaOr"

sigmaOrV :: ExprCore
sigmaOrV = "|||"

sigmaAndComb :: Scomb
sigmaAndComb = genFoldrComb sigmaT sigmaT sigmaAndV (sigmaBool False) "sigmaAnd"

sigmaAndV :: ExprCore
sigmaAndV = "&&&"

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
allComb = genFoldrMapComb boolT "&&" (bool True) "all"

anyComb :: Scomb
anyComb = genFoldrMapComb boolT "||" (bool False) "any"

sigmaAllComb :: Scomb
sigmaAllComb = genFoldrMapComb sigmaT sigmaAndV (sigmaBool True) "sigmaAll"

sigmaAnyComb :: Scomb
sigmaAnyComb = genFoldrMapComb sigmaT sigmaOrV (sigmaBool False) "sigmaAny"

one :: ExprCore
one = EPrim $ PrimInt 1

zero :: ExprCore
zero = EPrim $ PrimInt 0

add :: ExprCore -> ExprCore -> ExprCore
add a b = ap (EPrimOp OpAdd) [a, b]

sub :: ExprCore -> ExprCore -> ExprCore
sub a b = ap "-" [a, b]

lessThanEquals :: TypeCore -> ExprCore -> ExprCore -> ExprCore
lessThanEquals ty a b = ap lteV [a, b]
  where
    lteV = EVar (toCompareName ty "lessThanEquals")

------------------------------------------------------------
-- prim ops

builtInDiadic :: Map Name Instr
builtInDiadic = M.fromList $
  [ ("&&", And)
  , ("||", Or)
  , ("^^", Xor)
  , ("&&&", SigAnd)
  , ("|||", SigOr)
  , ("<>", TextAppend)
  , (Const.appendBytes, BytesAppend)
  ] ++ (compareNames =<< [intT, boolT, textT, bytesT])
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
builtInUnary = M.fromList $
  [ ("not", Not)
  , ("pk", SigPk)
  , ("toSigma", SigBool)
  , ("lengthText", TextLength)
  , ("hashBlake", HashBlake)
  , ("hashSha", HashSha)
  , (Const.sha256, Sha256)]
  ++ (fmap (\tag -> (Const.serialiseBytes $ argTypeName tag, ToBytes tag)) argTypes)
  ++ (fmap (\tag -> (Const.deserialiseBytes $ argTypeName tag, FromBytes tag)) argTypes)

