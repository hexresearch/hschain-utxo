module Hschain.Utxo.Lang.Infer where

import Hex.Common.Control
import Hex.Common.Text

import Control.Monad.Except
import Control.Monad.Trans
import Data.Fix hiding ((~>))
import Data.Vector (Vector)

import Data.Either
import Data.Function (on)
import Data.Set (Set)

import Language.HM (appE, varE, absE, letE, varT, appT, conT, monoT, forAllT, arrowT)

import Safe

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Vector as V

import Debug.Trace

import qualified Language.HM as H

checkMainModule :: H.Context Loc -> Module -> Maybe TypeError
checkMainModule ctx m = either Just (const Nothing) $ inferExpr ctx $ either modErr id $ moduleToMainExpr m
  where
    modErr = error . mappend "Failed to load module with: "

inferExpr :: H.Context Loc -> Lang -> Either TypeError Type
inferExpr ctx = H.inferW ctx . reduceExpr

reduceExpr :: Lang -> H.Term Loc
reduceExpr (Fix expr) = case expr of
  Var loc var               -> fromVarName var
  Apply loc a b             -> appE loc (rec a) (rec b)
  InfixApply loc a name b   -> fromInfixApply loc (rec a) name (rec b)
  Lam loc var a             -> absE loc (varName'name var) (rec a)
  LamList loc vs a          -> rec $ unfoldLamList loc vs a
  Let loc binds a           -> fromLet loc binds (rec a)
  LetRec loc var a b        -> undefined
  Ascr loc a ty             -> fromAscr loc (rec a) ty
  PrimE loc prim            -> fromPrim loc prim
  If loc cond t e           -> fromIf loc (rec cond) (rec t) (rec e)
  Pk loc a                  -> fromPk loc (rec a)
  -- operations
  UnOpE loc unOp a          -> fromUnOp loc unOp (rec a)
  BinOpE loc binOp a b      -> fromBinOp loc binOp (rec a) (rec b)
  Tuple loc vs              -> fromTuple loc $ fmap rec vs
  -- vectors
  VecE loc v                -> fromVec loc $ fmap rec v
  -- text
  TextE loc txt             -> fromText loc $ fmap rec txt
  -- boxes
  BoxE loc box              -> fromBox loc $ fmap rec box
  -- undefined
  Undef loc                 -> varE loc undefVar
  -- debug
  Trace loc a b             -> fromTrace loc (rec a) (rec b)
  -- environment
  GetEnv loc envId          -> fromGetEnv loc $ fmap rec envId
  where
    rec = reduceExpr

    fromInfixApply loc a name b =
      appE loc (appE (H.getLoc name) (fromVarName name) a) b

    fromVarName VarName{..}  = varE varName'loc varName'name

    fromLet _ binds expr = foldr bindToLet expr (sortBindGroups binds)
      where
        bindToLet Bind{..} body =
          letE (varName'loc bind'name) (varName'name bind'name)
               (reduceExpr $ altToExpr bind'alt)
               body

    fromAscr loc a ty = H.assertTypeE loc a ty

    fromPrim loc prim = ($ loc) $ case prim of
      PrimInt _    -> intE
      PrimString _ -> textE
      PrimBool _   -> boolE
      PrimSigma _  -> boolE

    fromIf loc cond t e = app3 loc ifVar cond t e

    fromPk loc a = appE loc (varE loc pkVar) a

    fromUnOp loc op a = case op of
      Not              -> not' a
      Neg              -> negate' a
      TupleAt size idx -> tupleAt size idx a
      where
        not' = appE loc (varE loc notVar)
        negate' = appE loc (varE loc negateVar)
        tupleAt size n = appE loc (varE loc $ tupleAtVar size n)

    fromBinOp loc op = op2 $ case op of
      And                 -> andVar
      Or                  -> orVar
      Plus                -> plusVar
      Minus               -> minusVar
      Times               -> timesVar
      Div                 -> divVar
      Equals              -> equalsVar
      NotEquals           -> notEqualsVar
      LessThan            -> lessThanVar
      GreaterThan         -> greaterThanVar
      LessThanEquals      -> lessThanEqualsVar
      GreaterThanEquals   -> greaterThanEqualsVar
      where
        op2 = app2 loc

    fromTuple loc vs = appNs loc (tupleConVar size) $ V.toList vs
      where
        size = V.length vs

    fromVec _ expr = case expr of
      NewVec loc vs      -> V.foldr (consVec loc) (nilVec loc) vs
      VecAppend loc a b  -> app2 loc appendVecVar a b
      VecAt loc a n      -> app2 loc vecAtVar a n
      VecLength loc      -> varE loc lengthVecVar
      VecMap loc         -> varE loc mapVecVar
      VecFold loc        -> varE loc foldVecVar

    fromText _ expr = case expr of
      TextAppend loc a b            -> app2 loc appendTextVar a b
      ConvertToText textTypeTag loc -> varE loc (convertToTextVar textTypeTag)
      TextLength loc                -> varE loc lengthTextVar
      TextHash loc hashAlgo         -> varE loc (textHashVar hashAlgo)

    fromBox _ expr = case expr of
      PrimBox loc _     -> varE loc boxVar
      BoxAt loc a field -> fromBoxField loc a field

    fromBoxField loc a field = case field of
      BoxFieldId       -> app1 loc getBoxIdVar a
      BoxFieldValue    -> app1 loc getBoxValueVar a
      BoxFieldScript   -> app1 loc getBoxScriptVar a
      BoxFieldArg arg  -> app2 loc getBoxArgVar a arg

    fromTrace loc msg a = app2 loc traceVar msg a

    fromGetEnv _ expr = case expr of
      Height loc    -> varE loc heightVar
      Input loc  a  -> app1 loc inputVar a
      Output loc a  -> app1 loc outputVar a
      Self loc      -> varE loc selfVar
      Inputs loc    -> varE loc inputsVar
      Outputs loc   -> varE loc outputsVar
      GetVar loc a  -> app1 loc getVarVar a

    app1 loc var a = appE loc (varE loc var) a
    app2 loc var a b = appE loc (appE loc (varE loc var) a) b
    app3 loc var a b c = appE loc (app2 loc var a b) c
    appNs loc var as = foldl (\con a -> appE loc con a) (varE loc var) as

    nilVec loc = varE loc nilVecVar
    consVec loc = app2 loc consVecVar


defaultContext :: H.Context Loc
defaultContext = H.Context $ M.fromList $
  -- primitives
  [ (intVar,    monoT intT)
  , (textVar,   monoT textT)
  , (boolVar,   monoT boolT)
  -- if
  , (ifVar,     forA $ monoT $ boolT `arr` (a `arr` (a `arr` a)))
  -- pk
  , (pkVar,     monoT $ textT `arr` boolT)
  -- operations
  --  unary
  , (notVar,    monoT $ boolT `arr` boolT)
  , (negateVar, monoT $ intT `arr` intT)
  -- binary
  , (andVar,    boolOp2)
  , (orVar,     boolOp2)
  , (plusVar,   intOp2)
  , (minusVar,  intOp2)
  , (timesVar,  intOp2)
  , (divVar,    intOp2)
  , (equalsVar, cmpOp2 )
  , (notEqualsVar, cmpOp2)
  , (lessThanVar, cmpOp2)
  , (greaterThanVar, cmpOp2)
  , (lessThanEqualsVar, cmpOp2)
  , (greaterThanEqualsVar, cmpOp2)
  -- vec expressions
  , (nilVecVar, forA $ monoT $ vectorT a)
  , (consVecVar, forA $ monoT $ a `arr` (vectorT a `arr` vectorT a))
  , (appendVecVar, forA $ monoT $ vectorT a `arr` (vectorT a `arr` vectorT a))
  , (vecAtVar, forA $ monoT $ vectorT a `arr` (intT `arr` a))
  , (lengthVecVar, forA $ monoT $ vectorT a `arr` intT)
  , (mapVecVar, forAB $ monoT $ (a `arr` b) `arr` (vectorT a `arr` vectorT b))
  , (foldVecVar, forAB $ monoT $ (b `arr` (a `arr` b)) `arr` (b `arr` (vectorT a `arr` b)))
  , (getBoxIdVar, monoT $ boxT `arr` textT)
  , (getBoxValueVar, monoT $ boxT `arr` intT)
  , (getBoxScriptVar, monoT $ boxT `arr` scriptT)
  , (getBoxArgVar, forA $ monoT $ boxT `arr` (textT `arr` a))
  , (undefVar, forA $ monoT a)
  , (traceVar, forA $ monoT $ textT `arr` (a `arr` a))
  , (heightVar, monoT intT)
  , (inputVar, monoT $ intT `arr` boxT)
  , (outputVar, monoT $ intT `arr` boxT)
  , (selfVar, monoT boxT)
  , (inputsVar, monoT $ vectorT boxT)
  , (outputsVar, monoT $ vectorT boxT)
  , (getVarVar, forA $ monoT $ intT `arr` a)
  ] ++ tupleVars ++ textExprVars
  where
    forA = forAllT noLoc "a"
    forAB = forA . forAllT noLoc "b"
    a = varT noLoc "a"
    b = varT noLoc "b"
    arr = arrowT noLoc

    opT2 x = monoT $ x `arr` (x `arr` x)
    boolOp2 = opT2 boolT
    intOp2 = opT2 intT
    cmpOp2 = forA $ monoT $ a `arr` (a `arr` boolT)

    tupleVars = [ toTuple size idx | size <- [2..6], idx <- [1 .. size]]
      where
        toTuple :: Int -> Int -> (H.Var, H.Signature Loc)
        toTuple size idx = (tupleAtVar size idx, tupleAtType size idx)

        tupleAtType :: Int -> Int -> H.Signature Loc
        tupleAtType size idx = pred $ monoT $ ty `arr` (varT noLoc $ v idx)
          where
            pred = foldr (.) id $ fmap (\n -> forAllT noLoc (v n)) [1 .. size]
            ty = foldl (\z n -> appT noLoc z (varT noLoc $ v n)) (conT noLoc (mappend "Tuple" (showt size))) [1 .. size]

            v n = mappend "a" (showt n)

    textExprVars =
      [ (appendTextVar, monoT $ textT `arr` (textT `arr` textT))
      , (lengthTextVar, monoT $ textT `arr` intT)
      , convertExpr IntToText intT
      , convertExpr BoolToText boolT
      , convertExpr ScriptToText scriptT
      ] ++ (fmap (\alg -> (textHashVar alg, monoT $ textT `arr` textT)) [Sha256, Blake2b256])
      where
        convertExpr tag ty = (convertToTextVar tag, monoT $ ty `arr` textT)


intE, textE, boolE :: Loc -> H.Term Loc

intE loc = varE loc intVar
textE loc = varE loc textVar
boolE loc = varE loc boolVar

intVar, textVar, boolVar, notVar, negateVar, boxVar, scriptVar :: H.Var

intVar = "Int"
textVar = "Text"
boolVar = "Bool"
boxVar = "Box"
scriptVar = secretVar "Script"
notVar = secretVar "not"
negateVar = secretVar "negate"

tupleAtVar :: Int -> Int -> H.Var
tupleAtVar size n = secretVar $ mconcat ["tupleAt-", showt size, "-", showt n]

tupleConVar :: Int -> H.Var
tupleConVar size = secretVar $ mappend "tuple" (showt size)

ifVar, pkVar :: H.Var

ifVar = secretVar "if"
pkVar = secretVar "pk"

intT :: H.Type Loc

intT = conT noLoc intVar


andVar, orVar, plusVar, minusVar, timesVar, divVar,
  equalsVar, notEqualsVar, lessThanVar,
  greaterThanVar, lessThanEqualsVar, greaterThanEqualsVar :: H.Var

andVar  = secretVar "and"
orVar   = secretVar "or"
plusVar = secretVar "plus"
minusVar = secretVar "minus"
timesVar = secretVar "times"
divVar   = secretVar "div"
equalsVar = secretVar "equals"
notEqualsVar = secretVar "notEquals"
lessThanVar  = secretVar "lessThan"
greaterThanVar = secretVar "greaterThan"
lessThanEqualsVar = secretVar "lessThanEquals"
greaterThanEqualsVar = secretVar "greaterThanEquals"

nilVecVar, consVecVar, appendVecVar, vecAtVar, lengthVecVar, mapVecVar, foldVecVar :: H.Var

nilVecVar = secretVar "nilVec"
consVecVar = secretVar "consVec"
appendVecVar = secretVar "appendVec"
vecAtVar = secretVar "vecAt"
lengthVecVar = secretVar "lengthVec"
mapVecVar = secretVar "mapVec"
foldVecVar = secretVar "foldVec"

appendTextVar, lengthTextVar :: H.Var

appendTextVar = secretVar "appendText"
lengthTextVar = secretVar "lengthText"

convertToTextVar :: TextTypeTag -> H.Var
convertToTextVar tag = secretVar $ mappend "convertToText" (showt tag)

textHashVar :: HashAlgo -> H.Var
textHashVar hashAlgo = secretVar $ mappend "textHash" (showt hashAlgo)


getBoxIdVar, getBoxValueVar, getBoxScriptVar, getBoxArgVar :: H.Var

getBoxIdVar = secretVar "getBoxId"
getBoxValueVar = secretVar "getBoxValue"
getBoxScriptVar = secretVar "getBoxScript"
getBoxArgVar = secretVar "getBoxArg"

undefVar :: H.Var
undefVar = secretVar "undefined"

traceVar :: H.Var
traceVar = secretVar "trace"

heightVar, inputVar, outputVar, selfVar, inputsVar, outputsVar, getVarVar :: H.Var

heightVar = secretVar "height"
inputVar = secretVar "input"
outputVar = secretVar "output"
selfVar = secretVar "self"
inputsVar = secretVar "inputs"
outputsVar = secretVar "outputs"
getVarVar = secretVar "getVar"

secretVar :: H.Var -> H.Var
secretVar = mappend ":# "



