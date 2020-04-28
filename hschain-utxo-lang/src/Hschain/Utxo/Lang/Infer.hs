module Hschain.Utxo.Lang.Infer(
    InferM(..)
  , runInferM
  , InferCtx(..)
  , inferExpr
  , reduceExpr
  , maxTupleSize
  , intT
  , userTypesToTypeContext
) where

import Hex.Common.Control
import Hex.Common.Text

import Control.Arrow (first)
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans

import Data.Fix hiding ((~>))
import Data.Foldable
import Data.Vector (Vector)

import Data.Either
import Data.Function (on)
import Data.Set (Set)
import Data.String

import Language.HM (appE, varE, lamE, letE, varT, conT, monoT, forAllT, arrowT, stripSignature)

import Safe

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Vector as V

import qualified Language.HM as H

newtype InferM a = InferM (FreshVar (Either Error) a)
  deriving newtype (Functor, Applicative, Monad, MonadFreshVar, MonadError Error)

instance MonadLang InferM where

runInferM :: InferM a -> Either Error a
runInferM (InferM m) = runFreshVar m

maxTupleSize :: Int
maxTupleSize = 6

inferExpr :: InferCtx -> Lang -> InferM Type
inferExpr (InferCtx typeCtx userTypes) =
  (InferM . lift . eitherTypeError . H.inferType (defaultContext <> typeCtx)) <=< reduceExpr userTypes

setLoc :: Loc -> VarName -> VarName
setLoc loc v = v { varName'loc = loc }

reduceExpr :: UserTypeCtx -> Lang -> InferM (H.Term VarName)
reduceExpr ctx@UserTypeCtx{..} (Fix expr) = case expr of
  Var loc var               -> pure $ fromVarName var
  Apply _ a b               -> liftA2 appE (rec a) (rec b)
  InfixApply _ a name b     -> liftA2 (\fa fb -> fromInfixApply fa name fb) (rec a) (rec b)
  Cons loc name args        -> fmap (fromCons loc name) (mapM rec $ V.toList args)
  Lam loc pat a             -> case pat of
                                  PVar _ var -> fmap (lamE var) (rec a)
                                  _          -> do
                                    v <- getFreshVar loc
                                    rec $ Fix $ Lam loc (PVar loc v) (Fix $ CaseOf loc (Fix $ Var loc v) [CaseExpr pat a])
  LamList loc vs a          -> rec $ unfoldLamList loc vs a
  Let loc binds a           -> fromLet loc binds =<< (rec a)
  LetRec loc var a b        -> pure $ undefined
  -- cases
  CaseOf loc expr alts      -> rec =<< caseToLet selectorNameVar loc expr alts
  Ascr _ a ty               -> fmap (\term -> fromAscr term $ stripSignature ty) (rec a)
  PrimE loc prim            -> pure $ fromPrim loc prim
  If loc cond t e           -> liftA3 (fromIf loc) (rec cond) (rec t) (rec e)
  Pk loc a                  -> fmap (fromPk loc) (rec a)
  -- operations
  UnOpE loc unOp a          -> fmap (fromUnOp loc unOp) (rec a)
  BinOpE loc binOp a b      -> liftA2 (fromBinOp loc binOp) (rec a) (rec b)
  Tuple loc vs              -> fmap (fromTuple loc) $ mapM rec vs
  -- vectors
  VecE loc v                -> fmap (fromVec loc) $ mapM rec v
  -- text
  TextE loc txt             -> fmap (fromText loc) $ mapM rec txt
  -- boxes
  BoxE loc box              -> fmap (fromBox loc) $ mapM rec box
  -- undefined
  Undef loc                 -> pure $ varE $ setLoc loc undefVar
  -- debug
  Trace loc a b             -> liftA2 (fromTrace loc) (rec a) (rec b)
  -- environment
  GetEnv loc envId          -> fmap (fromGetEnv loc) $ mapM rec envId
  AltE loc a b              -> liftA2 (app2 loc altVar) (rec a) (rec b)
  FailCase loc              -> return $ varE $ setLoc loc failCaseVar
  -- records
  RecConstr loc cons fields -> fromRecCons loc cons fields
  RecUpdate loc a upds      -> liftA2 (fromRecUpdate loc) (rec a) (mapM (\(field, x) -> fmap (field, ) $ rec x) upds)
  where
    rec = reduceExpr ctx

    fromInfixApply a name b =
      appE (appE (fromVarName name) a) b

    fromVarName v  = varE v

    fromCons loc cons args = foldl (\f arg -> appE f arg) (varE $ VarName loc (consName'name cons)) args

    fromRecCons loc cons fields = do
      args <- orderRecordFieldsFromContext ctx cons fields
      fmap (fromCons loc cons) $ mapM rec args

    fromLet _ binds expr = foldrM bindToLet expr (sortBindGroups binds)
      where
        bindToLet Bind{..} body = fmap (\alt ->
          letE [(bind'name, alt)]
               body)
               (rec =<< altGroupToExpr bind'alts)

    fromAscr a ty = H.assertTypeE a ty

    fromPrim loc prim = ($ loc) $ case prim of
      PrimInt _    -> intE
      PrimString _ -> textE
      PrimBool _   -> boolE
      PrimSigma _  -> boolE

    fromIf loc cond t e = app3 loc ifVar cond t e

    fromPk loc a = appE (varE $ setLoc loc pkVar) a

    fromUnOp loc op a = case op of
      Not              -> not' a
      Neg              -> negate' a
      TupleAt size idx -> tupleAt size idx a
      where
        not' = appE (varE $ setLoc loc notVar)
        negate' = appE (varE $ setLoc loc negateVar)
        tupleAt size n = appE (varE $ setLoc loc $ tupleAtVar size n)

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

    fromTuple loc vs = appNs loc (setLoc loc $ tupleConVar size) $ V.toList vs
      where
        size = V.length vs

    fromVec _ expr = case expr of
      NewVec loc vs      -> V.foldr (consVec loc) (nilVec loc) vs
      VecAppend loc a b  -> app2 loc appendVecVar a b
      VecAt loc a n      -> app2 loc vecAtVar a n
      VecLength loc      -> varE $ setLoc loc lengthVecVar
      VecMap loc         -> varE $ setLoc loc mapVecVar
      VecFold loc        -> varE $ setLoc loc foldVecVar

    fromText _ expr = case expr of
      TextAppend loc a b            -> app2 loc appendTextVar a b
      ConvertToText textTypeTag loc -> varE $ setLoc loc (convertToTextVar textTypeTag)
      TextLength loc                -> varE $ setLoc loc lengthTextVar
      TextHash loc hashAlgo         -> varE $ setLoc loc (textHashVar hashAlgo)

    fromBox _ expr = case expr of
      PrimBox loc _     -> varE $ setLoc loc boxVar
      BoxAt loc a field -> fromBoxField loc a field

    fromBoxField loc a field = case field of
      BoxFieldId       -> app1 loc getBoxIdVar a
      BoxFieldValue    -> app1 loc getBoxValueVar a
      BoxFieldScript   -> app1 loc getBoxScriptVar a
      BoxFieldArg arg  -> app2 loc getBoxArgVar a arg

    fromTrace loc msg a = app2 loc traceVar msg a

    fromGetEnv _ expr = case expr of
      Height loc    -> varE $ setLoc loc heightVar
      Input loc  a  -> app1 loc inputVar a
      Output loc a  -> app1 loc outputVar a
      Self loc      -> varE $ setLoc loc selfVar
      Inputs loc    -> varE $ setLoc loc inputsVar
      Outputs loc   -> varE $ setLoc loc outputsVar
      GetVar loc a  -> app1 loc getVarVar a

    app1 loc var a = appE (varE (setLoc loc var)) a
    app2 loc var a b = appE (appE (varE (setLoc loc var)) a) b
    app3 loc var a b c = appE (app2 loc var a b) c
    appNs loc var as = foldl (\con a -> appE con a) (varE $ setLoc loc var) as

    nilVec loc = varE $ setLoc loc nilVecVar
    consVec loc = app2 loc consVecVar

    fromRecUpdate loc a upds = foldl go a upds
      where
        go v (field, val) = app2 (H.getLoc field) (recordUpdateVar field) val v


defaultContext :: TypeContext
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
  , (altVar, forA $ monoT $ a `arr` (a `arr` a))
  , (failCaseVar, forA $ monoT a)
  ] ++ tupleConVars ++ tupleAtVars ++ textExprVars
  where
    forA = forAllT "a"
    forAB = forA . forAllT "b"
    a = varT "a"
    b = varT "b"
    arr = arrowT

    opT2 x = monoT $ x `arr` (x `arr` x)
    boolOp2 = opT2 boolT
    intOp2 = opT2 intT
    cmpOp2 = forA $ monoT $ a `arr` (a `arr` boolT)

    tupleConVars = fmap toTuple [2..maxTupleSize]
      where
        toTuple :: Int -> (VarName, Signature)
        toTuple size = (tupleConVar size, tupleConType size)

        tupleConType :: Int -> Signature
        tupleConType size = foldr (\v mt -> forAllT v mt) (monoT ty) vs
          where
            vs = fmap v [0 .. size-1]
            ty = foldr (\lhs rhs -> arrowT (varT lhs) rhs) (tupleCon size) vs

    tupleAtVars = [ toTuple size idx | size <- [2..maxTupleSize], idx <- [0 .. size-1]]
      where
        toTuple :: Int -> Int -> (VarName, Signature)
        toTuple size idx = (tupleAtVar size idx, tupleAtType size idx)

        tupleAtType :: Int -> Int -> Signature
        tupleAtType size idx = pred $ monoT $ (tupleCon size) `arr` (varT $ v idx)
          where
            pred = foldr (.) id $ fmap (\n -> forAllT (v n)) [0 .. size-1]

    tupleCon :: Int -> Type
    tupleCon size = tupleT $ fmap (varT . v) [0..size-1]

    v n = VarName noLoc $ mappend "a" (showt n)

    textExprVars =
      [ (appendTextVar, monoT $ textT `arr` (textT `arr` textT))
      , (lengthTextVar, monoT $ textT `arr` intT)
      , convertExpr IntToText intT
      , convertExpr BoolToText boolT
      , convertExpr ScriptToText scriptT
      ] ++ (fmap (\alg -> (textHashVar alg, monoT $ textT `arr` textT)) [Sha256, Blake2b256])
      where
        convertExpr tag ty = (convertToTextVar tag, monoT $ ty `arr` textT)


intE, textE, boolE :: Loc -> H.Term VarName

intE loc = varE $ setLoc loc intVar
textE loc = varE $ setLoc loc textVar
boolE loc = varE $ setLoc loc boolVar

intVar, textVar, boolVar, notVar, negateVar, boxVar, scriptVar :: VarName

intVar = "Int"
textVar = "Text"
boolVar = "Bool"
boxVar = "Box"
scriptVar = secretVar' "Script"
notVar = secretVar' "not"
negateVar = secretVar' "negate"

tupleAtVar :: Int -> Int -> VarName
tupleAtVar size n = VarName noLoc $ secretVar $ mconcat ["tupleAt-", showt size, "-", showt n]

tupleConVar :: Int -> VarName
tupleConVar size = VarName noLoc $ secretVar $ mappend "tuple" (showt size)

ifVar, pkVar :: VarName

ifVar = secretVar' "if"
pkVar = secretVar' "pk"

intT :: Type
intT = conT intVar []


andVar, orVar, plusVar, minusVar, timesVar, divVar,
  equalsVar, notEqualsVar, lessThanVar,
  greaterThanVar, lessThanEqualsVar, greaterThanEqualsVar :: VarName

andVar  = secretVar' "and"
orVar   = secretVar' "or"
plusVar = secretVar' "plus"
minusVar = secretVar' "minus"
timesVar = secretVar' "times"
divVar   = secretVar' "div"
equalsVar = secretVar' "equals"
notEqualsVar = secretVar' "notEquals"
lessThanVar  = secretVar' "lessThan"
greaterThanVar = secretVar' "greaterThan"
lessThanEqualsVar = secretVar' "lessThanEquals"
greaterThanEqualsVar = secretVar' "greaterThanEquals"

nilVecVar, consVecVar, appendVecVar, vecAtVar, lengthVecVar, mapVecVar, foldVecVar :: VarName

nilVecVar = secretVar' "nilVec"
consVecVar = secretVar' "consVec"
appendVecVar = secretVar' "appendVec"
vecAtVar = secretVar' "vecAt"
lengthVecVar = secretVar' "lengthVec"
mapVecVar = secretVar' "mapVec"
foldVecVar = secretVar' "foldVec"

appendTextVar, lengthTextVar :: VarName

appendTextVar = secretVar' "appendText"
lengthTextVar = secretVar' "lengthText"

convertToTextVar :: TextTypeTag -> VarName
convertToTextVar tag = VarName noLoc $ secretVar $ mappend "convertToText" (showt tag)

textHashVar :: HashAlgo -> VarName
textHashVar hashAlgo = VarName noLoc $ secretVar $ mappend "textHash" (showt hashAlgo)


getBoxIdVar, getBoxValueVar, getBoxScriptVar, getBoxArgVar :: VarName

getBoxIdVar = secretVar' "getBoxId"
getBoxValueVar = secretVar' "getBoxValue"
getBoxScriptVar = secretVar' "getBoxScript"
getBoxArgVar = secretVar' "getBoxArg"

undefVar :: VarName
undefVar = secretVar' "undefined"

traceVar :: VarName
traceVar = secretVar' "trace"

heightVar, inputVar, outputVar, selfVar, inputsVar, outputsVar, getVarVar :: VarName

heightVar = secretVar' "height"
inputVar = secretVar' "input"
outputVar = secretVar' "output"
selfVar = secretVar' "self"
inputsVar = secretVar' "inputs"
outputsVar = secretVar' "outputs"
getVarVar = secretVar' "getVar"

altVar, failCaseVar :: VarName

altVar = secretVar' "altCases"
failCaseVar = secretVar' "failCase"



---------------------------------------------------------

userTypesToTypeContext :: UserTypeCtx -> TypeContext
userTypesToTypeContext (UserTypeCtx m _) =
     foldMap fromUserType m
  <> foldMap getSelectors m
  where
    fromUserType u@UserType{..} = H.Context $ M.fromList $ fromCase =<< M.toList userType'cases
      where
        resT = toResT u
        appArgsT = toArgsT u
        fromCase (cons, args) = (VarName noLoc $ consName'name cons, ty) : (recFieldSelectors ++ recFieldUpdates)
          where
            ty = appArgsT $ monoT $ V.foldr (\a res -> arrowT a res) resT $ getConsTypes args

            onFields f = case args of
              ConsDef _         -> []
              RecordCons fields -> f fields

            recFieldSelectors = onFields $ \fields ->
              V.toList $ fmap fromRecSelector fields

            recFieldUpdates = onFields $ \fields ->
              V.toList $ fmap fromRecUpdate fields

        fromRecSelector RecordField{..} = (recordField'name, ty)
          where
            ty = appArgsT $ monoT $ arrowT resT recordField'type

        fromRecUpdate RecordField{..} = (recordUpdateVar recordField'name, ty)
          where
            ty = appArgsT $ monoT $ arrowT recordField'type (arrowT resT resT)

    getSelectors ut@UserType{..} = M.foldMapWithKey toSel userType'cases
      where
        resT = toResT ut
        appArgsT = toArgsT ut
        toSelType cons n ty = appArgsT $ monoT $ arrowT resT ty

        toSel cons ts = H.Context $ M.fromList $ fmap (first $ VarName noLoc) $
          zipWith (\n ty -> (selectorNameVar cons n, toSelType cons n ty)) [0 ..] $ V.toList $ getConsTypes ts

    toResT UserType{..} = con' userType'name $ fmap var' userType'args

    toArgsT UserType{..} ty = foldr (\a res -> forAllT a res) ty userType'args

    con' VarName{..} args = conT (VarName varName'loc varName'name) args
    var' VarName{..} = varT $ VarName varName'loc varName'name

selectorNameVar :: ConsName -> Int -> T.Text
selectorNameVar cons n = secretVar $ mconcat ["sel_", consName'name cons, "_", showt n]

recordUpdateVar :: VarName -> VarName
recordUpdateVar field = field { varName'name = secretVar $ mconcat ["update_", varName'name field] }

secretVar' :: VarName -> VarName
secretVar' v = v { varName'name = secretVar $ varName'name v }

