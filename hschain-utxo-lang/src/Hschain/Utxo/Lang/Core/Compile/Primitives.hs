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
import Hschain.Utxo.Lang.Core.Compile.Build
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Code (Instr(..))
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Types (TxEnv(..))

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
    getArgsTypes = fmap toArgType [ IntArg, TextArg, BoolArg ]

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
  , op1 "hashBlake" textT textT
  , op1 "hashSha" textT textT

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
  , Typed "args"   argsT
  ]

getBoxId :: Scomb
getBoxId = getBoxField Const.getBoxId (Typed "name" textT) textT

getBoxScript :: Scomb
getBoxScript = getBoxField Const.getBoxScript (Typed "script" textT) textT

getBoxValue :: Scomb
getBoxValue = getBoxField Const.getBoxValue (Typed "value" intT) intT

getBoxArgs :: [Scomb]
getBoxArgs = [ getBoxIntArgs, getBoxTextArgs, getBoxBoolArgs ]
  where
    getBoxIntArgs  = getBoxArgsBy IntArg  "ints"
    getBoxTextArgs = getBoxArgsBy TextArg "texts"
    getBoxBoolArgs = getBoxArgsBy BoolArg "bools"

    getBoxArgsBy typeTag resVar = Scomb
      { scomb'name = Const.getBoxArgs $ argTypeName typeTag
      , scomb'args = V.fromList [x]
      , scomb'body = Typed
          (onBox $ onArgs $ EVar $ Typed resVar resType)
          (listT resType)
      }
      where
        resType = fromArgType typeTag
        onBox e  = ECase (Typed xv boxT) [CaseAlt 0 boxArgs e]
        onArgs e = ECase (Typed argFieldV argsT) [CaseAlt 0 [Typed "ints" (listT intT), Typed "texts" (listT textT), Typed "bools" (listT boolT)] e]
        argField = Typed "args" argsT
        argFieldV = EVar argField

        x = Typed "x" boxT
        xv = EVar x


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
  [ argComb PrimInt  intT  IntArg  args'ints
  , argComb PrimText textT TextArg args'texts
  , argComb PrimBool boolT BoolArg args'bools
  ]
  where
    argComb cons ty tyTag vals = constantComb (Const.getArgs $ argTypeName tyTag) (listT ty) (toVec ty $ fmap (EPrim . cons) vals)

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
      (ap (EConstr consT 1 2) [xv, xsv])
      asT
  }
  where
    x  = Typed "x" aT
    xs = Typed "xs" asT
    consT = funT [aT, asT] asT

    xv = EVar x
    xsv = EVar xs

    aT  = varT "a"
    asT = listT aT

foldrComb :: Scomb
foldrComb = Scomb
  { scomb'name = "foldr"
  , scomb'args = V.fromList [f, z, as]
  , scomb'body = Typed
      (ECase (Typed asv asT)
        [ CaseAlt 0 [] zv
        , CaseAlt 1 [x, xs] (ap fv [xv, ap foldrv [fv, zv, xsv]])
        ])
      bT
  }
  where
    f = Typed "f" fT
    z = Typed "z" zT
    foldr' = Typed "foldr" foldrT
    as = Typed "as" asT
    x = Typed "x" aT
    xs = Typed "xs" asT

    fv     = EVar f
    zv     = EVar z
    foldrv = EVar foldr'
    asv    = EVar as
    xv     = EVar x
    xsv    = EVar xs

    foldrT = funT [fT, zT, asT] bT

    fT = aT `arrowT` (bT `arrowT` bT)
    asT = listT aT
    zT = bT
    aT = varT "a"
    bT = varT "b"

lengthComb :: Scomb
lengthComb = Scomb
  { scomb'name = "length"
  , scomb'args = V.fromList [Typed "as" (listT aT)]
  , scomb'body = Typed
      (ECase (Typed asv asT)
        [ CaseAlt 0 [] (EPrim $ PrimInt 0)
        , CaseAlt 1 [x, xs] (add one (length' xsv))
        ])
      intT
  }
  where
    lengthT = asT `arrowT` intT
    asT = listT aT
    aT  = varT "a"

    as     = Typed "as" asT
    x      = Typed "x" aT
    xs     = Typed "xs" (listT aT)
    asv    = EVar as
    xsv    = EVar xs

    lengthv = EVar $ Typed "length" lengthT
    length' = EAp lengthv

mapComb :: Scomb
mapComb = Scomb
  { scomb'name = "map"
  , scomb'args = V.fromList [f, as]
  , scomb'body = Typed
      (ECase (Typed asv asT)
        [ CaseAlt 0 [] (EConstr nilT 0 0)
        , CaseAlt 1 [x, xs] (ap (EConstr consT 1 2) [EAp fv xv, ap mapv [fv, xsv]])
        ])
      bsT
  }
  where
    f      = Typed "f" fT
    as     = Typed "as" asT
    x      = Typed "x" aT
    xs     = Typed "xs" asT

    fv     = EVar f
    asv    = EVar as

    mapv   = EVar $ Typed "map" mapT
    xv     = EVar x
    xsv    = EVar xs

    mapT = funT [fT, asT] bsT

    nilT = listT aT
    consT = bT `arrowT` (listT bT `arrowT` listT bT)
    fT = aT `arrowT` bT
    asT = listT aT
    bsT = listT bT
    aT = varT "a"
    bT = varT "b"

listAtComb :: Scomb
listAtComb = Scomb
  { scomb'name = "listAt"
  , scomb'args = V.fromList [n, as]
  , scomb'body = Typed

      (ECase (Typed asv asT)
        [ CaseAlt 0 [] (EBottom )
        , CaseAlt 1 [x, xs] (EIf (lessThanEquals intT nv zero) xv (ap listAtv [sub nv one, xsv]))
        ])
      aT
  }
  where
    n      = Typed "n" intT
    as     = Typed "as" asT
    x      = Typed "x" aT
    xs     = Typed "xs" asT

    nv     = EVar n
    asv    = EVar as

    listAtv = EVar $ Typed "listAt" listAtT
    xv     = EVar x
    xsv    = EVar xs

    listAtT = funT [intT, asT] aT

    asT = listT aT
    aT = varT "a"

listAppendComb :: Scomb
listAppendComb = Scomb
  { scomb'name = "++"
  , scomb'args = V.fromList [as, bs]
  , scomb'body = Typed
      (ECase (Typed asv asT)
        [ CaseAlt 0 [] bsv
        , CaseAlt 1 [x, xs] (ap (EConstr consT 1 2) [xv, ap listAppendv [xsv, bsv]])
        ])
      asT
  }
  where
    as     = Typed "as" asT
    bs     = Typed "bs" asT
    x      = Typed "x" aT
    xs     = Typed "xs" asT

    asv    = EVar as
    bsv    = EVar bs

    listAppendv   = EVar $ Typed "++" listAppendT
    xv     = EVar x
    xsv    = EVar xs

    listAppendT = funT [asT, asT] asT

    consT = aT `arrowT` (listT aT `arrowT` listT aT)
    asT = listT aT
    aT = varT "a"


filterComb :: Scomb
filterComb = Scomb
  { scomb'name = "filter"
  , scomb'args = V.fromList [f, as]
  , scomb'body = Typed
      (ECase (Typed asv asT)
        [ CaseAlt 0 [] (EConstr nilT 0 0)
        , CaseAlt 1 [x, xs]
            (ELet [(ys, ap filterv [fv, xsv])]
                  (EIf (EAp fv xv)
                       (ap (EConstr consT 1 2) [xv, ysv])
                       ysv
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

    fv     = EVar f
    asv    = EVar as

    filterv   = EVar $ Typed "filter" filterT
    xv     = EVar x
    xsv    = EVar xs
    ysv    = EVar ys

    filterT = funT [fT, asT] asT

    nilT = listT aT
    consT = aT `arrowT` (listT aT `arrowT` listT aT)
    fT = aT `arrowT` boolT
    asT = listT aT
    aT = varT "a"

foldlComb :: Scomb
foldlComb = Scomb
  { scomb'name = "foldl"
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
    foldlName = Typed "foldl" foldlT

    fv  = EVar f
    zv  = EVar z
    asv = EVar as
    xv  = EVar x
    xsv = EVar xs
    foldlv = EVar foldlName

    foldlT = funT [fT, zT, asT] bT
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
    asV = EVar as
    fT = funT [aT, bT] bT
    foldrV = EVar $ Typed "foldr" foldrT
    foldrT = funT [fT, bT, listT aT] bT

sumComb :: Scomb
sumComb = genFoldrComb intT intT addV zero "sum"
  where
    addV = EVar $ Typed "+" addT
    addT = funT [intT, intT] intT

productComb :: Scomb
productComb = genFoldrComb intT intT mulV one "product"
  where
    mulV = EVar $ Typed "*" mulT
    mulT = funT [intT, intT] intT

orComb :: Scomb
orComb = genFoldrComb boolT boolT orV (bool False) "or"

orV :: ExprCore
orV = EVar $ Typed "||" orT
  where
    orT = funT [boolT, boolT] boolT

andComb :: Scomb
andComb = genFoldrComb boolT boolT andV (bool True) "and"

andV :: ExprCore
andV = EVar $ Typed "&&" andT
  where
    andT = funT [boolT, boolT] boolT

sigmaOrComb :: Scomb
sigmaOrComb = genFoldrComb sigmaT sigmaT sigmaOrV (sigmaBool False) "sigmaOr"

sigmaOrV :: ExprCore
sigmaOrV = EVar $ Typed "|||" sigmaOrT
  where
    sigmaOrT = funT [sigmaT, sigmaT] sigmaT

sigmaAndComb :: Scomb
sigmaAndComb = genFoldrComb sigmaT sigmaT sigmaAndV (sigmaBool False) "sigmaAnd"

sigmaAndV :: ExprCore
sigmaAndV = EVar $ Typed "&&&" sigmaAndT
  where
    sigmaAndT = funT [sigmaT, sigmaT] sigmaT

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

    fv     = EVar f
    asv    = EVar as
    xv     = EVar x
    xsv    = EVar xs

    nameV  = EVar $ Typed name nameT
    nameT  = funT [fT, asT] bT

    fT = aT `arrowT` bT
    asT = listT aT

allComb :: Scomb
allComb = genFoldrMapComb (varT "a") boolT andV (bool True) "all"

anyComb :: Scomb
anyComb = genFoldrMapComb (varT "a") boolT orV (bool False) "any"

sigmaAllComb :: Scomb
sigmaAllComb = genFoldrMapComb (varT "a") sigmaT sigmaAndV (sigmaBool True) "sigmaAll"

sigmaAnyComb :: Scomb
sigmaAnyComb = genFoldrMapComb (varT "a") sigmaT sigmaOrV (sigmaBool False) "sigmaAny"

one :: ExprCore
one = EPrim $ PrimInt 1

zero :: ExprCore
zero = EPrim $ PrimInt 0

add :: ExprCore -> ExprCore -> ExprCore
add a b = ap addV [a, b]
  where
    addV = EVar $ Typed "+" addT
    addT = funT [intT, intT] intT

sub :: ExprCore -> ExprCore -> ExprCore
sub a b = ap subV [a, b]
  where
    subV = EVar $ Typed "-" subT
    subT = funT [intT, intT] intT

lessThanEquals :: TypeCore -> ExprCore -> ExprCore -> ExprCore
lessThanEquals ty a b = ap lteV [a, b]
  where
    lteV = EVar $ Typed (toCompareName ty "lessThanEquals") (funT [ty, ty] boolT)

fromArgType :: ArgType -> TypeCore
fromArgType = \case
  IntArg  -> intT
  BoolArg -> boolT
  TextArg -> textT

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

