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

import Type.Check.HM (appE, varE, lamE, conT, monoT, forAllT, stripSignature)

import Hschain.Utxo.Lang.Desugar hiding (app1, app2, app3)
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Lib.Base (baseLibTypeContext)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Type.Check.HM as H

import qualified Hschain.Utxo.Lang.Const as Const
import qualified Hschain.Utxo.Lang.Desugar as D

data HschainLang

data EmptyPrim = EmptyPrim
  deriving (Show)

instance H.Lang HschainLang where
  type Src  HschainLang = Loc
  type Var  HschainLang = Text
  type Prim HschainLang = EmptyPrim

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
  (InferM . lift . eitherTypeError . H.inferType (baseLibTypeContext <> userTypesCtx <> defaultContext <> typeCtx)) <=< reduceExpr userTypes
  where
    userTypesCtx = userTypesToTypeContext userTypes

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
  Tuple loc vs              -> fmap (fromTuple loc) $ mapM rec vs
  List loc vs               -> fmap (fromList loc) $ mapM rec vs
  NegApp loc a              -> fmap (appE loc (varE loc "negate")) (rec a)
  AltE loc a b              -> liftA2 (app2 loc altVar) (rec a) (rec b)
  FailCase loc              -> return $ varE loc failCaseVar
  -- records
  RecConstr loc cons fields -> fromRecCons loc cons fields
  RecUpdate loc a upds      -> liftA2 (fromRecUpdate loc) (rec a) (mapM (\(field, x) -> fmap (field, ) $ rec x) upds)
  AntiQuote loc _ _         -> throwError $ ParseError loc "AntiQuote encountered"
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

    fromTuple loc vs = appNs loc (tupleConVar size) $ V.toList vs
      where
        size = V.length vs

    fromList loc vs = V.foldr (consVec loc) (nilVec loc) vs

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
  , (altVar, forA $ monoT $ a `arr` (a `arr` a))
  , (failCaseVar, forA $ monoT a)
  -- lists
  , (nilVecVar,  forA $ monoT $ listT a)
  , (consVecVar, forA $ monoT $ a `arr` (listT a `arr` listT a))
  ] ++ tupleConVars ++ tupleAtVars
  where
    forA = forAllT noLoc "a"
    a = varT "a"
    arr = arrowT

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


intE, textE, boolE, bytesE, sigmaE :: loc -> H.Term prim loc Text

intE loc = varE loc intVar
textE loc = varE loc textVar
bytesE loc = varE loc bytesVar
boolE loc = varE loc boolVar
sigmaE loc = varE loc sigmaVar

intVar, textVar, bytesVar, boolVar, sigmaVar :: Text

intVar = secretVar "Int"
textVar = secretVar "Text"
bytesVar = secretVar "Bytes"
boolVar = secretVar "Bool"
sigmaVar = secretVar "Sigma"

tupleAtVar :: Int -> Int -> Text
tupleAtVar size n = secretVar $ mconcat ["tupleAt-", showt size, "-", showt n]

tupleConVar :: Int -> Text
tupleConVar size = secretVar $ mappend "tuple" (showt size)

nilVecVar, consVecVar :: Text

nilVecVar = secretVar "nil"
consVecVar = secretVar "cons"

ifVar :: Text
ifVar = secretVar "if"

altVar, failCaseVar :: Text

altVar = secretVar "altCases"
failCaseVar = secretVar "failCase"

---------------------------------------------------------

-- | Extract type-context for constructors that are defined by the user
-- or record getters and modifiers.
userTypesToTypeContext :: UserTypeCtx -> TypeContext
userTypesToTypeContext (UserTypeCtx m _ _ _ _) =
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

------------------------------------------------------------
--


caseToLet :: MonadLang m =>
  (ConsName -> Int -> Text) -> Loc -> Lang -> [CaseExpr Lang] -> m Lang
caseToLet toSelectorName loc expr cases = do
  v <- getFreshVar loc
  fmap (Fix . Let loc [simpleBind v expr]) $ caseToLet' toSelectorName loc v cases

caseToLet' :: MonadLang m =>
  (ConsName -> Int -> Text) -> Loc -> VarName -> [CaseExpr Lang] -> m Lang
caseToLet' toSelectorName topLoc var cases = fmap (foldr (\(loc, a) rest -> Fix $ AltE loc a rest) failCase) $ mapM fromCase cases
  where
    toVarExpr loc v = Fix $ Var loc $ VarName loc $ varName'name v

    fromCase CaseExpr{..} = fmap (H.getLoc caseExpr'lhs, ) $ case caseExpr'lhs of
      PVar ploc pvar -> return $ Fix $ Let ploc [simpleBind pvar $ toVarExpr ploc var] caseExpr'rhs
      PWildCard _ -> return $ caseExpr'rhs
      PPrim ploc p -> return $ Fix $ If ploc (eqPrim ploc var p) caseExpr'rhs failCase
      PCons ploc cons pats ->
        case pats of
          [] -> constCons ploc cons
          _  -> argCons ploc cons pats
      PTuple ploc pats -> do
        (vs, rhs') <- reduceSubPats pats caseExpr'rhs
        let size = length vs
            bg = zipWith (\n v -> simpleBind v (tupleAt (varName'loc v) size n $ toVarExpr ploc var)) [0..] vs
        return $ Fix $ Let ploc bg rhs'
      where
        tupleAt :: Loc -> Int -> Int -> Fix E -> Lang
        tupleAt loc size n e = Fix $ Apply loc (Fix $ Var loc $ VarName loc $ tupleAtVar size n) e

        constCons ploc cons = return $
          D.app2 (Fix $ Var ploc $ VarName ploc $ toSelectorName cons 0) (toVarExpr ploc var) caseExpr'rhs

        argCons ploc cons pats = do
          (vs, rhs') <- reduceSubPats pats caseExpr'rhs
          let bg = zipWith (\n v -> simpleBind v (Fix $ Apply (varName'loc v) (selector ploc cons n) $ toVarExpr ploc var)) [0..] vs
          return $ Fix $ Let ploc bg rhs'

    selector ploc cons n = Fix $ Var ploc (VarName ploc (toSelectorName cons n))

    failCase = Fix $ FailCase topLoc

    eqPrim ploc v p = Fix $ InfixApply ploc (toVarExpr ploc v) (VarName ploc Const.equals) (Fix $ PrimE ploc p)

reduceSubPats :: forall m . MonadLang m => [Pat] -> Lang -> m ([VarName], Lang)
reduceSubPats pats rhs = runStateT (mapM go pats) rhs
  where
    go :: Pat -> StateT Lang m VarName
    go pat = case pat of
      PVar _ var -> return var
      _          -> do
        expr <- get
        let loc = H.getLoc pat
        var  <- lift $ getFreshVar loc
        put $ Fix $ CaseOf loc (Fix $ Var loc var) $ [CaseExpr pat expr]
        return var

