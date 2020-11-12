-- | This module defines type-inference utilities.
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

import Hex.Common.Text (showt)

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Fix hiding ((~>))
import Data.Text (Text)

import Language.HM (appE, varE, lamE, conT, monoT, forAllT, stripSignature)

import Hschain.Utxo.Lang.Desugar hiding (app1, app2, app3)
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types  (ArgType(..), argTypes)
import Hschain.Utxo.Lang.Monad

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Language.HM as H

import qualified Hschain.Utxo.Lang.Const as Const

data EmptyPrim = EmptyPrim
  deriving (Show)

instance H.IsPrim EmptyPrim where
  type PrimLoc EmptyPrim = Loc
  type PrimVar EmptyPrim = Text
  getPrimType EmptyPrim = H.conT noLoc "Unit" []

-- | Monad for type-inference.
newtype InferM a = InferM (FreshVar (Either Error) a)
  deriving newtype (Functor, Applicative, Monad, MonadFreshVar, MonadError Error)

instance MonadLang InferM where

-- | Get value of type-inference monad.
runInferM :: InferM a -> Either Error a
runInferM (InferM m) = runFreshVar m

-- | Maximum supported tuple size in the language.
maxTupleSize :: Int
maxTupleSize = 6

-- | Infer type for expression.
--
-- > inferExpr context expr
--
-- It takes context of already infered expressions (signatures for free variables)
-- and expression ot infer the type.
inferExpr :: InferCtx -> Lang -> InferM Type
inferExpr (InferCtx typeCtx userTypes) =
  (InferM . lift . eitherTypeError . H.inferType (defaultContext <> typeCtx)) <=< reduceExpr userTypes

-- | Convert expression of our language to term of simpler language
-- that can be type-checked by the library.
reduceExpr :: UserTypeCtx -> Lang -> InferM (H.Term EmptyPrim Loc Text)
reduceExpr ctx@UserTypeCtx{..} (Fix expr) = case expr of
  Var _loc var              -> pure $ fromVarName var
  Apply loc a b             -> liftA2 (appE loc) (rec a) (rec b)
  InfixApply loc a name b   -> liftA2 (\fa fb -> fromInfixApply loc fa name fb) (rec a) (rec b)
  Cons loc name args        -> fmap (fromCons loc name) (mapM rec $ V.toList args)
  Lam loc pat a             -> case pat of
                                  PVar src var -> fmap (lamE src $ varName'name var) (rec a)
                                  _            -> do
                                    v <- getFreshVar loc
                                    rec $ Fix $ Lam loc (PVar loc v) (Fix $ CaseOf loc (Fix $ Var loc v) [CaseExpr pat a])
  LamList loc vs a          -> rec $ unfoldLamList loc vs a
  Let loc binds a           -> fromLet loc binds =<< rec a
  PrimLet loc binds a       -> fromPrimLet loc binds =<< rec a
  -- cases
  CaseOf loc e alts         -> rec =<< caseToLet selectorNameVar loc e alts
  Ascr loc a ty             -> fmap (\term -> fromAscr loc term $ stripSignature ty) (rec a)
  PrimE loc prim            -> pure $ fromPrim loc prim
  If loc cond t e           -> liftA3 (fromIf loc) (rec cond) (rec t) (rec e)
  -- operations
  UnOpE loc unOp a          -> fmap (fromUnOp loc unOp) (rec a)
  BinOpE loc binOp a b      -> liftA2 (fromBinOp loc binOp) (rec a) (rec b)
  Tuple loc vs              -> fmap (fromTuple loc) $ mapM rec vs
  -- sigmas
  SigmaE loc sigma          -> fmap (fromSigma loc) $ mapM rec sigma
  -- vectors
  VecE loc v                -> fmap (fromVec loc) $ mapM rec v
  -- text
  TextE loc txt             -> fmap (fromText loc) $ mapM rec txt
  -- text
  BytesE loc txt            -> fmap (fromBytes loc) $ mapM rec txt
  -- boxes
  BoxE loc box              -> fmap (fromBox loc) $ mapM rec box
  -- debug
  Trace loc a b             -> liftA2 (fromTrace loc) (rec a) (rec b)
  -- environment
  GetEnv loc envId          -> fmap (fromGetEnv loc) $ mapM rec envId
  -- btc-style signatures
  CheckSig loc a b          -> liftA2 (app2 loc checkSigVar) (rec a) (rec b)
  CheckMultiSig loc a b c   -> liftA3 (app3 loc checkMultiSigVar) (rec a) (rec b) (rec c)
  AltE loc a b              -> liftA2 (app2 loc altVar) (rec a) (rec b)
  FailCase loc              -> return $ varE loc failCaseVar
  -- records
  RecConstr loc cons fields -> fromRecCons loc cons fields
  RecUpdate loc a upds      -> liftA2 (fromRecUpdate loc) (rec a) (mapM (\(field, x) -> fmap (field, ) $ rec x) upds)
  where
    rec = reduceExpr ctx

    fromInfixApply loc a name b =
      appE loc (appE loc (fromVarName name) a) b

    fromVarName v = varE (varName'loc v) (varName'name v)

    fromCons loc cons args = foldl (\f arg -> appE loc f arg) (fromVarName $ consToVarName cons) args

    fromRecCons loc cons fields = do
      args <- orderRecordFieldsFromContext ctx cons fields
      fmap (fromCons loc cons) $ mapM rec args

    fromLet loc binds e = fmap (\bs -> foldr (H.letE loc) e bs) $ mapM toBind (sortBindGroups binds)
      where
        toBind Bind{..} = do
          rhs <- rec =<< altGroupToExpr bind'alts
          return $ H.Bind
            { H.bind'loc = varName'loc  bind'name
            , H.bind'lhs = varName'name bind'name
            , H.bind'rhs = rhs
            }

    fromPrimLet loc primBinds e = fmap (\bs -> foldr (H.letE loc) e bs) $ mapM toBind primBinds
      where
        toBind (name, rhs) = do
          rhs' <- rec rhs
          return $ H.Bind
            { H.bind'loc = varName'loc  name
            , H.bind'lhs = varName'name name
            , H.bind'rhs = rhs'
            }

    fromAscr loc a ty = H.assertTypeE loc a ty

    fromPrim loc prim = ($ loc) $ case prim of
      PrimInt _    -> intE
      PrimString _ -> textE
      PrimBool _   -> boolE
      PrimSigma _  -> sigmaE
      PrimBytes _  -> bytesE

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

    fromSigma _ = \case
      Pk loc a         -> fromPk loc a
      SAnd loc a b     -> app2 loc sigmaAndVar a b
      SOr loc a b      -> app2 loc sigmaOrVar a b
      SPrimBool loc a  -> app1 loc toSigmaVar a

    fromVec _ = \case
      NewVec loc vs      -> V.foldr (consVec loc) (nilVec loc) vs
      VecAppend loc a b  -> app2 loc appendVecVar a b
      VecAt loc a n      -> app2 loc vecAtVar a n
      VecLength loc      -> varE loc lengthVecVar
      VecMap loc         -> varE loc mapVecVar
      VecFold loc        -> varE loc foldVecVar
      VecAndSigma loc    -> varE loc andSigmaVecVar
      VecOrSigma loc     -> varE loc orSigmaVecVar

    fromText _ = \case
      TextAppend loc a b            -> app2 loc appendTextVar a b
      ConvertToText loc textTypeTag -> varE loc (convertToTextVar textTypeTag)
      TextLength loc                -> varE loc lengthTextVar

    fromBytes _ = \case
      BytesAppend loc a b            -> app2 loc appendBytesVar a b
      BytesLength loc a              -> app1 loc lengthBytesVar a
      SerialiseToBytes loc tag a     -> app1 loc (serialiseToBytesVar tag) a
      DeserialiseFromBytes loc tag a -> app1 loc (deserialiseToBytesVar tag) a
      BytesHash loc hashAlgo a       -> app1 loc (bytesHashVar hashAlgo) a

    fromBox _ = \case
      BoxAt loc a field -> fromBoxField loc a field

    fromBoxField loc a field = case field of
      BoxFieldId         -> app1 loc getBoxIdVar a
      BoxFieldValue      -> app1 loc getBoxValueVar a
      BoxFieldScript     -> app1 loc getBoxScriptVar a
      BoxFieldArgList ty -> app1 loc (getBoxArgVar' ty) a
      BoxFieldPostHeight -> app1 loc getBoxPostHeightVar a

    fromTrace loc msg a = app2 loc traceVar msg a

    fromGetEnv _ = \case
      Height loc    -> varE loc heightVar
      Input loc  a  -> app1 loc inputVar a
      Output loc a  -> app1 loc outputVar a
      Self loc      -> varE loc selfVar
      Inputs loc    -> varE loc inputsVar
      Outputs loc   -> varE loc outputsVar
      GetVar loc ty -> varE loc (getEnvVarName ty)

    app1 loc var a = appE loc (varE loc var) a
    app2 loc var a b = appE loc (appE loc (varE loc var) a) b
    app3 loc var a b c = appE loc (app2 loc var a b) c
    appNs loc var as = foldl (\con a -> appE loc con a) (varE loc var) as

    nilVec loc = varE loc nilVecVar
    consVec loc = app2 loc consVecVar

    fromRecUpdate _ a upds = foldl go a upds
      where
        go v (field, val) = app2 (H.getLoc field) (recordUpdateVar field) val v


defaultContext :: TypeContext
defaultContext = H.Context $ M.fromList $
  -- primitives
  [ (intVar,    monoT intT)
  , (textVar,   monoT textT)
  , (boolVar,   monoT boolT)
  , (sigmaVar,  monoT sigmaT)
  , (bytesVar,  monoT bytesT)
  -- if
  , (ifVar,     forA $ monoT $ boolT `arr` (a `arr` (a `arr` a)))
  -- pk
  , (pkVar,     monoT $ bytesT `arr` sigmaT)
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
  -- sigma expressions
  , (sigmaOrVar, monoT $ sigmaT `arr` (sigmaT `arr` sigmaT))
  , (sigmaAndVar, monoT $ sigmaT `arr` (sigmaT `arr` sigmaT))
  , (toSigmaVar, monoT $ boolT `arr` sigmaT)
  -- signatures
  , (checkSigVar, monoT $ bytesT `arr` (intT `arr` boolT))
  , (checkMultiSigVar, monoT $ intT `arr` (listT bytesT `arr` (listT intT `arr` boolT)))
  -- vec expressions
  , (nilVecVar, forA $ monoT $ listT a)
  , (consVecVar, forA $ monoT $ a `arr` (listT a `arr` listT a))
  , (appendVecVar, forA $ monoT $ listT a `arr` (listT a `arr` listT a))
  , (vecAtVar, forA $ monoT $ listT a `arr` (intT `arr` a))
  , (lengthVecVar, forA $ monoT $ listT a `arr` intT)
  , (mapVecVar, forAB $ monoT $ (a `arr` b) `arr` (listT a `arr` listT b))
  , (foldVecVar, forAB $ monoT $ (b `arr` (a `arr` b)) `arr` (b `arr` (listT a `arr` b)))
  , (andSigmaVecVar, monoT $ listT sigmaT `arr` sigmaT)
  , (orSigmaVecVar, monoT $ listT sigmaT `arr` sigmaT)
  , (getBoxIdVar, monoT $ boxT `arr` textT)
  , (getBoxValueVar, monoT $ boxT `arr` intT)
  , (getBoxPostHeightVar, monoT $ boxT `arr` intT)
  , (getBoxScriptVar, monoT $ boxT `arr` scriptT)
  , (undefVar, forA $ monoT a)
  , (traceVar, forA $ monoT $ textT `arr` (a `arr` a))
  , (heightVar, monoT intT)
  , (inputVar, monoT $ intT `arr` boxT)
  , (outputVar, monoT $ intT `arr` boxT)
  , (selfVar, monoT boxT)
  , (inputsVar, monoT $ listT boxT)
  , (outputsVar, monoT $ listT boxT)
  , (getVarVar, forA $ monoT $ intT `arr` a)
  , (altVar, forA $ monoT $ a `arr` (a `arr` a))
  , (failCaseVar, forA $ monoT a)
  ] ++ tupleConVars ++ tupleAtVars ++ textExprVars ++ bytesExprVars ++ getBoxArgVars
  where
    getBoxArgVars =
      fmap (\ty -> (getBoxArgVar' ty, monoT $ boxT `arr` (listT $ argTagToType ty))) argTypes

    forA = forAllT noLoc "a"
    forAB = forA . forAllT noLoc "b"
    a = varT "a"
    b = varT "b"
    arr = arrowT

    opT2 x = monoT $ x `arr` (x `arr` x)
    boolOp2 = opT2 boolT
    intOp2 = opT2 intT
    cmpOp2 = forA $ monoT $ a `arr` (a `arr` boolT)

    tupleConVars = fmap toTuple [2..maxTupleSize]
      where
        toTuple :: Int -> (Text, Signature)
        toTuple size = (tupleConVar size, tupleConType size)

        tupleConType :: Int -> Signature
        tupleConType size = foldr (\var mt -> forAllT noLoc var mt) (monoT ty) vs
          where
            vs = fmap v [0 .. size-1]
            ty = foldr (\lhs rhs -> arrowT (varT lhs) rhs) (tupleCon size) vs

    tupleAtVars = [ toTuple size idx | size <- [2..maxTupleSize], idx <- [0 .. size-1]]
      where
        toTuple :: Int -> Int -> (Text, Signature)
        toTuple size idx = (tupleAtVar size idx, tupleAtType size idx)

        tupleAtType :: Int -> Int -> Signature
        tupleAtType size idx = predicate $ monoT $ (tupleCon size) `arr` (varT $ v idx)
          where
            predicate = foldr (.) id $ fmap (\n -> forAllT noLoc (v n)) [0 .. size-1]

    tupleCon :: Int -> Type
    tupleCon size = tupleT $ fmap (varT . v) [0..size-1]

    v n = mappend "a" (showt n)

    textExprVars =
      [ (appendTextVar, monoT $ textT `arr` (textT `arr` textT))
      , (lengthTextVar, monoT $ textT `arr` intT)
      , convertExpr IntToText intT
      , convertExpr BoolToText boolT
      , convertExpr ScriptToText scriptT
      ] ++ (fmap (\alg -> (bytesHashVar alg, monoT $ bytesT `arr` bytesT)) [Sha256])
      where
        convertExpr tag ty = (convertToTextVar tag, monoT $ ty `arr` textT)

    bytesExprVars =
      [ (appendBytesVar, monoT $ bytesT `arr` (bytesT `arr` bytesT))
      , (lengthBytesVar, monoT $ bytesT `arr` intT)
      ] ++ (fmap (\tag -> (serialiseToBytesVar tag, monoT $ argTagToType tag `arr` bytesT)) argTypes)
        ++ (fmap (\tag -> (deserialiseToBytesVar tag, monoT $ bytesT `arr` argTagToType tag)) argTypes)


intE, textE, boolE, bytesE, sigmaE :: loc -> H.Term prim loc Text

intE loc = varE loc intVar
textE loc = varE loc textVar
bytesE loc = varE loc bytesVar
boolE loc = varE loc boolVar
sigmaE loc = varE loc sigmaVar

intVar, textVar, bytesVar, boolVar, sigmaVar, notVar, negateVar :: Text

intVar = secretVar "Int"
textVar = secretVar "Text"
bytesVar = secretVar "Bytes"
boolVar = secretVar "Bool"
sigmaVar = secretVar "Sigma"
notVar = secretVar "not"
negateVar = secretVar "negate"

tupleAtVar :: Int -> Int -> Text
tupleAtVar size n = secretVar $ mconcat ["tupleAt-", showt size, "-", showt n]

tupleConVar :: Int -> Text
tupleConVar size = secretVar $ mappend "tuple" (showt size)

ifVar, pkVar :: Text

ifVar = secretVar "if"
pkVar = secretVar "pk"


andVar, orVar, plusVar, minusVar, timesVar, divVar,
  equalsVar, notEqualsVar, lessThanVar,
  greaterThanVar, lessThanEqualsVar, greaterThanEqualsVar :: Text

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

nilVecVar, consVecVar, appendVecVar, vecAtVar, lengthVecVar, mapVecVar, foldVecVar, andSigmaVecVar, orSigmaVecVar :: Text

nilVecVar = secretVar "nilVec"
consVecVar = secretVar "consVec"
appendVecVar = secretVar "appendVec"
vecAtVar = secretVar "vecAt"
lengthVecVar = secretVar "lengthVec"
mapVecVar = secretVar "mapVec"
foldVecVar = secretVar "foldVec"
andSigmaVecVar = secretVar "andSigma"
orSigmaVecVar = secretVar "orSigma"

checkSigVar, checkMultiSigVar :: Text

checkSigVar = secretVar Const.checkSig
checkMultiSigVar = secretVar Const.checkMultiSig

appendTextVar, lengthTextVar :: Text

appendTextVar = secretVar "appendText"
lengthTextVar = secretVar "lengthText"

appendBytesVar :: Text
appendBytesVar = secretVar Const.appendBytes

lengthBytesVar :: Text
lengthBytesVar = secretVar Const.lengthBytes

serialiseToBytesVar, deserialiseToBytesVar :: ArgType -> Text

serialiseToBytesVar   tag = secretVar $ Const.serialiseBytes (argTypeName tag)
deserialiseToBytesVar tag = secretVar $ Const.deserialiseBytes (argTypeName tag)

convertToTextVar :: TextTypeTag -> Text
convertToTextVar tag = secretVar $ mappend "convertToText" (showt tag)

bytesHashVar :: HashAlgo -> Text
bytesHashVar hashAlgo = secretVar $ mappend "bytesHash" (showt hashAlgo)


getBoxIdVar, getBoxValueVar, getBoxScriptVar, getBoxPostHeightVar :: Text

getBoxIdVar = secretVar Const.getBoxId
getBoxValueVar = secretVar Const.getBoxValue
getBoxScriptVar = secretVar Const.getBoxScript
getBoxPostHeightVar = secretVar Const.getBoxPostHeight

undefVar :: Text
undefVar = secretVar "undefined"

traceVar :: Text
traceVar = secretVar "trace"

heightVar, inputVar, outputVar, selfVar, inputsVar, outputsVar, getVarVar :: Text

heightVar = secretVar "height"
inputVar = secretVar "input"
outputVar = secretVar "output"
selfVar = secretVar "self"
inputsVar = secretVar "inputs"
outputsVar = secretVar "outputs"
getVarVar = secretVar "getVar"

altVar, failCaseVar :: Text

altVar = secretVar "altCases"
failCaseVar = secretVar "failCase"

sigmaAndVar, sigmaOrVar, toSigmaVar :: Text

sigmaAndVar = "sigmaAnd"
sigmaOrVar  = "sigmaOr"
toSigmaVar  = "toSigma"

---------------------------------------------------------

-- | Extract type-context for constructors that are defined by the user
-- or record getters and modifiers.
userTypesToTypeContext :: UserTypeCtx -> TypeContext
userTypesToTypeContext (UserTypeCtx m _ _ _) =
     foldMap fromUserType m
  <> foldMap getSelectors m
  where
    fromUserType u@UserType{..} = H.Context $ M.fromList $ fromCase =<< M.toList userType'cases
      where
        resT = toResT u
        appArgsT = toArgsT u
        fromCase (cons, args) = (consName'name cons, ty) : (recFieldSelectors ++ recFieldUpdates)
          where
            ty = appArgsT $ monoT $ V.foldr (\a res -> arrowT a res) resT $ getConsTypes args

            onFields f = case args of
              ConsDef _         -> []
              RecordCons fields -> f fields

            recFieldSelectors = onFields $ \fields ->
              V.toList $ fmap fromRecSelector fields

            recFieldUpdates = onFields $ \fields ->
              V.toList $ fmap fromRecUpdate fields

        fromRecSelector RecordField{..} = (varName'name recordField'name, ty)
          where
            ty = appArgsT $ monoT $ arrowT resT recordField'type

        fromRecUpdate RecordField{..} = (recordUpdateVar recordField'name, ty)
          where
            ty = appArgsT $ monoT $ arrowT recordField'type (arrowT resT resT)

    getSelectors ut@UserType{..} = M.foldMapWithKey toSel userType'cases
      where
        resT = toResT ut
        appArgsT = toArgsT ut
        toSelType ty = appArgsT $ monoT $ arrowT resT ty
        toConstSelType =
          appArgsT $ forAllT noLoc freshVar $ monoT $ arrowT resT
            $ arrowT (varT freshVar) (varT freshVar)
          where
            freshVar = mappend "a" $ mconcat $ fmap varName'name userType'args

        toSel cons ts =
          case V.toList $ getConsTypes ts of
            [] -> toConstSel cons
            xs  -> toArgSel cons xs

        toConstSel cons = H.Context $ M.singleton (selectorNameVar cons 0) toConstSelType

        toArgSel cons ts = H.Context $ M.fromList $
          zipWith (\n ty -> (selectorNameVar cons n, toSelType ty)) [0 ..] ts


    toResT UserType{..} = con' userType'name $ fmap var' userType'args

    toArgsT UserType{..} ty = foldr (\a res -> forAllT noLoc (varName'name a) res) ty userType'args

    con' VarName{..} = conT varName'loc varName'name
    var' VarName{..} = H.varT varName'loc varName'name

selectorNameVar :: ConsName -> Int -> T.Text
selectorNameVar cons n = secretVar $ mconcat ["sel_", consName'name cons, "_", showt n]

recordUpdateVar :: VarName -> Text
recordUpdateVar field = secretVar $ mconcat ["update_", varName'name field]

getBoxArgVar' :: ArgType -> Text
getBoxArgVar' = secretVar . getBoxArgVar

