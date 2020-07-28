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
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Code (Instr(..))
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Types (TxEnv(..))

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Const as Const

fromArgType :: ArgType -> TypeCore
fromArgType = \case
  IntArg   -> intT
  BoolArg  -> boolT
  TextArg  -> textT
  BytesArg -> bytesT

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
  , getSelf txEnv'self
  , getInputs txEnv'inputs
  , getOutputs txEnv'outputs
  ] ++ getArgs txEnv'args

environmentTypes :: TypeContext
environmentTypes = TypeContext $ M.fromList $
  [ (Const.getHeight,  intT)
  , (Const.getSelf,    boxT)
  , (Const.getInputs,  listT boxT)
  , (Const.getOutputs, listT boxT)
  ] ++ getArgsTypes
  where
    getArgsTypes = fmap toArgType argTypes

    toArgType typeTag = (Const.getArgs $ argTypeName typeTag, listT $ fromArgType typeTag)

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
  { scomb'name = "Box"
  , scomb'args = V.fromList boxArgs
  , scomb'body = Typed (ap (EConstr consTy 0 4) $ fmap (EVar . typed'value) boxArgs) boxT
  }
  where
    consTy = funT (fmap typed'type boxArgs) boxT

getBoxField :: Name -> Name -> TypeCore -> Scomb
getBoxField name field resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [Typed "box" boxT]
  , scomb'body =
      Typed
        (ECase (Typed (EVar "box") boxT) [CaseAlt 0 boxArgs (EVar field)])
        resT
  }
  where

boxArgs :: [Typed Name]
boxArgs =
  [ Typed "name"   textT
  , Typed "script" bytesT
  , Typed "value"  intT
  , Typed "args"   argsT
  ]

getBoxId :: Scomb
getBoxId = getBoxField Const.getBoxId "name" textT

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
      { scomb'name = Const.getBoxArgs $ argTypeName typeTag
      , scomb'args = V.fromList [Typed "x" boxT]
      , scomb'body = Typed
          (onBox $ onArgs $ EVar resVar)
          (listT resType)
      }
      where
        resType = fromArgType typeTag
        onBox e  = ECase (Typed (EVar "x") boxT) [CaseAlt 0 boxArgs e]
        onArgs e = ECase (Typed (EVar "args") argsT) [CaseAlt 0 [Typed "ints" (listT intT), Typed "texts" (listT textT), Typed "bools" (listT boolT)] e]

boxConstr :: ExprCore -> ExprCore -> ExprCore -> ExprCore -> ExprCore
boxConstr name script value args = ap (EConstr consTy 0 4) [name, script, value, args]
  where
    consTy = funT (fmap typed'type boxArgs) boxT


toBox :: Box -> ExprCore
toBox Box{..} = boxConstr name script value args
  where
    name   = EPrim $ PrimText  $ unBoxId box'id
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
  { scomb'name = "cons"
  , scomb'args = V.fromList [x, xs]
  , scomb'body = Typed
      (ap (EConstr consT 1 2) [EVar "x", EVar "xs"])
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
  { scomb'name = "foldr"
  , scomb'args = V.fromList [f, z, as]
  , scomb'body = Typed
      (ECase (Typed (EVar "as") asT)
        [ CaseAlt 0 [] (EVar "z")
        , CaseAlt 1 [x, xs] (ap (EVar "f") [EVar "x", ap (EVar "foldr") [EVar "f", EVar "z", EVar "xs"]])
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
  { scomb'name = Const.length
  , scomb'args = V.fromList [Typed "as" (listT aT)]
  , scomb'body = Typed
      (ECase (Typed (EVar "as") asT)
        [ CaseAlt 0 [] (EPrim $ PrimInt 0)
        , CaseAlt 1 [x, xs] (add one (EAp (EVar Const.length) (EVar "xs")))
        ])
      intT
  }
  where
    asT = listT aT
    aT  = varT "a"

    x      = Typed "x" aT
    xs     = Typed "xs" (listT aT)

mapComb :: Scomb
mapComb = Scomb
  { scomb'name = Const.map
  , scomb'args = V.fromList [f, as]
  , scomb'body = Typed
      (ECase (Typed (EVar "as") asT)
        [ CaseAlt 0 [] (EConstr nilT 0 0)
        , CaseAlt 1 [x, xs] (ap (EConstr consT 1 2) [EAp (EVar "f") (EVar "x"), ap (EVar "map") [EVar "f", EVar "xs"]])
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
  { scomb'name = Const.listAt
  , scomb'args = V.fromList [as, n]
  , scomb'body = Typed

      (ECase (Typed (EVar "as") asT)
        [ CaseAlt 0 [] EBottom
        , CaseAlt 1 [x, xs] (EIf (lessThanEquals intT (EVar "n") zero) (EVar "x") (ap (EVar Const.listAt) [EVar "xs", sub (EVar "n") one]))
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
  { scomb'name = Const.appendList
  , scomb'args = V.fromList [as, bs]
  , scomb'body = Typed
      (ECase (Typed (EVar "as") asT)
        [ CaseAlt 0 [] (EVar "bs")
        , CaseAlt 1 [x, xs] (ap (EConstr consT 1 2) [EVar "x", ap (EVar Const.appendList) [EVar "xs", EVar "bs"]])
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
  { scomb'name = Const.filter
  , scomb'args = V.fromList [f, as]
  , scomb'body = Typed
      (ECase (Typed (EVar "as") asT)
        [ CaseAlt 0 [] (EConstr nilT 0 0)
        , CaseAlt 1 [x, xs]
            (ELet [(ys, ap (EVar "filter") [EVar "f", EVar "xs"])]
                  (EIf (EAp (EVar "f") (EVar "x"))
                       (ap (EConstr consT 1 2) [EVar "x", EVar "ys"])
                       (EVar "ys")
                  ))
        ])
      asT
  }
  where
    f      = Typed "f" fT
    as     = Typed "as" asT
    x      = Typed "x" aT
    xs     = Typed "xs" asT
    ys     = Typed "ys" asT

    nilT = listT aT
    consT = aT `arrowT` (listT aT `arrowT` listT aT)
    fT = aT `arrowT` boolT
    asT = listT aT
    aT = varT "a"

foldlComb :: Scomb
foldlComb = Scomb
  { scomb'name = Const.foldl
  , scomb'args = V.fromList [f, z, as]
  , scomb'body = Typed
      (ECase (Typed asv asT)
        [ CaseAlt 0 [] zv
        , CaseAlt 1 [x, xs] (ap foldlv [fv, ap fv [zv, xv], xsv])
        ])
      bT
  }
  where
    f  = Typed "f" fT
    z  = Typed "z" zT
    as = Typed "as" asT
    x  = Typed "x"  aT
    xs = Typed "xs" asT

    fv  = EVar "f"
    zv  = EVar "z"
    asv = EVar "as"
    xv  = EVar "x"
    xsv = EVar "xs"
    foldlv = EVar Const.foldl

    asT = listT asT
    zT  = bT
    fT  = bT `arrowT` (aT `arrowT` bT)

    aT = varT "a"
    bT = varT "b"


genFoldrComb :: TypeCore -> TypeCore -> ExprCore -> ExprCore -> Name -> Scomb
genFoldrComb aT bT f z name = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [as]
  , scomb'body = Typed
      (ap foldrV [f, z, asV])
      bT
  }
  where
    as = Typed "as" (listT aT)
    asV = EVar "as"
    foldrV = EVar Const.foldr

sumComb :: Scomb
sumComb = genFoldrComb intT intT (EVar "+") zero "sum"

productComb :: Scomb
productComb = genFoldrComb intT intT (EVar "*") one "product"

orComb :: Scomb
orComb = genFoldrComb boolT boolT (EVar "||") (bool False) "or"

andComb :: Scomb
andComb = genFoldrComb boolT boolT (EVar "&&") (bool True) "and"

sigmaOrComb :: Scomb
sigmaOrComb = genFoldrComb sigmaT sigmaT sigmaOrV (sigmaBool False) "sigmaOr"

sigmaOrV :: ExprCore
sigmaOrV = EVar "|||"

sigmaAndComb :: Scomb
sigmaAndComb = genFoldrComb sigmaT sigmaT sigmaAndV (sigmaBool False) "sigmaAnd"

sigmaAndV :: ExprCore
sigmaAndV = EVar "&&&"

genFoldrMapComb :: TypeCore -> TypeCore -> ExprCore -> ExprCore -> Name -> Scomb
genFoldrMapComb aT bT append z name = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [f, as]
  , scomb'body = Typed
      (ECase (Typed asv asT)
        [ CaseAlt 0 [] z
        , CaseAlt 1 [x, xs] (ap append [EAp fv xv, ap nameV [fv, xsv]])
        ])
      bT
  }
  where
    f = Typed "f" fT
    as = Typed "as" asT
    x = Typed "x" aT
    xs = Typed "xs" asT

    fv     = EVar "f"
    asv    = EVar "as"
    xv     = EVar "x"
    xsv    = EVar "xs"

    nameV  = EVar name

    fT = aT `arrowT` bT
    asT = listT aT

allComb :: Scomb
allComb = genFoldrMapComb (varT "a") boolT (EVar "&&") (bool True) "all"

anyComb :: Scomb
anyComb = genFoldrMapComb (varT "a") boolT (EVar "||") (bool False) "any"

sigmaAllComb :: Scomb
sigmaAllComb = genFoldrMapComb (varT "a") sigmaT sigmaAndV (sigmaBool True) "sigmaAll"

sigmaAnyComb :: Scomb
sigmaAnyComb = genFoldrMapComb (varT "a") sigmaT sigmaOrV (sigmaBool False) "sigmaAny"

one :: ExprCore
one = EPrim $ PrimInt 1

zero :: ExprCore
zero = EPrim $ PrimInt 0

add :: ExprCore -> ExprCore -> ExprCore
add a b = ap (EVar "+") [a, b]

sub :: ExprCore -> ExprCore -> ExprCore
sub a b = ap (EVar "-") [a, b]

lessThanEquals :: TypeCore -> ExprCore -> ExprCore -> ExprCore
lessThanEquals ty a b = ap lteV [a, b]
  where
    lteV = EVar (toCompareName ty "lessThanEquals")

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
  [ ("negate", Neg)
  , ("not", Not)
  , ("pk", SigPk)
  , ("toSigma", SigBool)
  , ("lengthText", TextLength)
  , ("hashBlake", HashBlake)
  , ("hashSha", HashSha)
  , (Const.sha256, Sha256)]
  ++ (fmap (\tag -> (Const.serialiseBytes $ argTypeName tag, ToBytes tag)) argTypes)
  ++ (fmap (\tag -> (Const.deserialiseBytes $ argTypeName tag, FromBytes tag)) argTypes)

