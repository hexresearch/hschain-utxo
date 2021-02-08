-- | Functions that operate on module level.
module Hschain.Utxo.Lang.Exec.Module(
    evalModule
  , checkMainModule
  , trimModuleByMain
  , fixTopLevelPatBinds
  , toUserTypeCtx
  , checkUserTypeInCtx
  , appendExecCtx
) where

import Control.Applicative (Alternative(..))

import Hex.Common.Text

import Control.Monad.State.Strict

import Data.Bifunctor
import Data.Either
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Sequence (ViewL(..))
import Data.Text (Text)

import Hschain.Utxo.Lang.Const (reservedNames)
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Module
import Hschain.Utxo.Lang.UserType
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Lib.Base (baseNames, baseLibTypes)

import qualified Type.Check.HM as H
import qualified Data.Graph as G
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Sequence as Seq

checkUserTypeInCtx :: Set VarName -> UserTypeCtx -> UserType -> Maybe Error
checkUserTypeInCtx definedValues ctx ut = fmap TypeDeclError $
      typeIsDefined (userType'name ut)
  <|> (group $ fmap consIsDefined (M.keys $ userType'cases ut))
  <|> (group $ fmap recFieldIsDefined ((\(name, defn) -> fmap (name, ) $ getRecFields defn) =<< constrs))
  <|> (group $ fmap (uncurry checkKinds) argTypeNamesWithArity)
  where
    group = L.foldl' (<|>) Nothing
    consDeclError = ConsDeclError (userType'name ut)

    constrs = M.toList $ userType'cases ut
    argTypeNamesWithArity = getTypeNamesWithArity =<< (\(name, defn) -> fmap (name, ) $ V.toList $ getConsTypes defn) =<< constrs

    typeIsDefined name
      | isPrimType name = Nothing
      | otherwise       = fmap (TypeIsDefined name . userType'name) $ M.lookup name (userTypeCtx'types ctx)

    consIsDefined cons = fmap (consDeclError cons . ConsIsDefined . userType'name . consInfo'def) $ M.lookup cons (userTypeCtx'constrs ctx)

    recFieldIsDefined (cons, field)
      | S.member (varName'name field) reservedNames = Just $ consDeclError cons $ RecFieldIsReservedName field
      | S.member field definedValues                = Just $ consDeclError cons $ RecFieldIsDefinedAsValue field
      | otherwise                                    =
            fmap (consDeclError cons . RecFieldIsDefinedInCons field . fst) $ M.lookup (varName'name field) (userTypeCtx'recFields ctx)

    checkKinds :: ConsName -> (VarName, Int) -> Maybe TypeDeclError
    checkKinds cons (tyName, arity) =
      case M.lookup tyName (userTypeCtx'types ctx) of
        Just ty -> let expectedArity = getTypeArity ty
                   in  if expectedArity /= arity
                         then Just $ consDeclError cons $ TypeAppError tyName (KindError expectedArity arity)
                         else Nothing
        Nothing ->
          if isPrimType tyName
            then
              if arity == 0
                then Nothing
                else Just $ consDeclError cons $ TypeAppError tyName (KindError 0 arity)
            else
              Just $ consDeclError cons $ TypeArgIsNotDefined tyName

    getRecFields = \case
      RecordCons fields -> V.toList $ fmap recordField'name fields
      _                 -> []

    getTypeNamesWithArity :: (ConsName, Type) -> [(ConsName, (VarName, Int))]
    getTypeNamesWithArity (cons, (H.Type ty)) = fmap (cons, ) $ foldFix go ty
      where
        go = \case
          H.ConT loc name args -> (VarName loc name, length args) : concat args
          H.ArrowT _ a b       -> a <> b
          H.TupleT _ ts        -> mconcat ts
          H.ListT _ a          -> a
          H.VarT _ _           -> []

    getTypeArity = length . userType'args

isPrimType :: VarName -> Bool
isPrimType VarName{..} = S.member varName'name primTypes

primTypes :: Set Text
primTypes = S.fromList ["Int", "Bool", "Sigma", "()", "Text", "Bytes"]

checkUserTypes :: Set VarName -> [UserType] -> Maybe Error
checkUserTypes definedVals uts = L.foldl' (<|>) Nothing $ fmap checkSubgroup typeSubgroups
  where
    typeSubgroups = reverse $ fst $ foldl go ([], []) $ orderTypesByDeps uts
      where
        go (groups, approvedTypes) ty = ((ty, approvedTypes) : groups, ty : approvedTypes)

    checkSubgroup (ty, approved) = checkUserTypeInCtx definedVals (toUserTypeCtxNoBase approved) ty

    toUserTypeCtxNoBase typeDecls =
      setupUserTypeInfo $ (\ts -> UserTypeCtx ts mempty mempty mempty mempty) $ M.fromList $ fmap (\x -> (userType'name x, x)) typeDecls

-- | Convert raw module data to context information that can be used
-- to evaluate expressions that depend on this module.
evalModule :: TypeContext -> Module -> Either Error ModuleCtx
evalModule typeCtx m = evalModule' typeCtx =<< fixTopLevelPatBinds (appendRecordFuns m)

fixTopLevelPatBinds :: Module -> Either Error Module
fixTopLevelPatBinds m
  | any isPat decls = fmap (\bs -> m { module'binds = bs }) $ fixDecls $ module'binds m
  | otherwise       = Right m
  where
    decls = binds'decls $ module'binds m

    isPat = \case
      PatBind{..} -> True
      _           -> False

    fixDecls Binds{..} =
      case L.partition isMain binds'decls of
        ([mainBind], rest) -> Right $ Binds mainTypes [addRest (Binds restTypes rest) $ mainBind]
        _                  -> Left $ PatError MissingMain
      where
        restTypes = M.delete "main" binds'types
        mainTypes = M.intersection binds'types (M.singleton "main" ())

        addRest rest b = case b of
          FunBind{..} -> b { bind'alts = fmap (addToAlt rest) bind'alts }
          PatBind{..} -> b { bind'alt  = addToAlt rest bind'alt }

        addToAlt rest a = a { alt'where = Just rest <> alt'where a }

    isMain = \case
      FunBind{..} -> bind'name == "main"
      _           -> False


evalModule' :: TypeContext -> Module -> Either Error ModuleCtx
evalModule' typeCtx Module{..} = do
  maybe (Right ()) Left $ checkUserTypes definedVals userTypes
  runInferM $ do
    (binds, typeCtxInfer) <- InferM $ lift $ runStateT (mapM (checkBind $ binds'types module'binds) (binds'decls module'binds)) (typeCtx <> userTypeCtx)
    fmap setExports $ toModuleCtx typeCtxInfer $ Binds (binds'types module'binds) binds
  where
    userTypes = M.elems $ userTypeCtx'types module'userTypes
    definedVals = case typeCtx of
      H.Context defs -> S.fromList $ fmap (VarName noLoc) $ M.keys defs

    userTypeCtx = userTypesToTypeContext module'userTypes

    setExports m = case moduleHead'exports =<< module'head of
      Just (ExportList _ items) -> m { moduleCtx'exports = Just items }
      _                         -> m

    toModuleCtx :: TypeContext -> Binds Lang -> InferM ModuleCtx
    toModuleCtx typeCtxInfer bs = fmap (\es -> ModuleCtx
      { moduleCtx'types = InferCtx ((restrictToDefinedNames es $ typeCtxInfer <> (H.Context types)) <> userTypeCtx) module'userTypes
      , moduleCtx'exprs = ExecCtx (M.fromList es)
      , moduleCtx'exports = Nothing
      }) exprs
      where
        restrictToDefinedNames es  (H.Context m) = H.Context $ M.intersection m (M.fromList $ fmap (first varName'name) es)

        types = M.mapKeys varName'name $ binds'types bs

        exprs = mapM procBind $ binds'decls bs
          where
            procBind = \case
              FunBind{..} -> fmap (bind'name, ) $ desugar module'userTypes =<< altGroupToExpr bind'alts
              PatBind{..} -> unexpected "PatBind in evalModule"

    checkBind :: Map VarName Signature -> Bind Lang -> StateT TypeContext (Either Error) (Bind Lang)
    checkBind types bind = case bind of
      FunBind{..} -> do
        ctx <- get
        ty <- lift $ runInferM $ inferExpr (InferCtx ctx module'userTypes) =<< altGroupToExpr bind'alts
        let bindTy = M.lookup bind'name types
            typeIsOk =
              case bindTy of
                Just userTy -> if (isRight $ H.subtypeOf (H.stripSignature userTy) ty) then Nothing else (Just userTy)
                Nothing     -> Nothing
        case typeIsOk of
          Just userTy -> do
            lift $ Left $ TypeError $ H.UnifyErr (H.getLoc userTy) (H.stripSignature userTy) ty
          Nothing     -> do
            let resTy = fromMaybe (H.typeToSignature ty) bindTy
            put $ ctx <> H.Context (M.singleton (varName'name bind'name) resTy)
            return bind
      PatBind{..} -> unexpected "PatBind in checkBind"

data SelectIndex = SelectIndex
  { selectIndex'size  :: !Int
  , selectIndex'id    :: !Int
  } deriving (Show, Eq)

appendRecordFuns :: Module -> Module
appendRecordFuns m =
  m { module'binds = recordFuns <> module'binds m }
  where
    recordFuns = selectors <> updaters

    selectors = mconcat $ extractSelectors =<< types

    updaters = mconcat $ extractUpdaters =<< types

    types = M.elems $ userTypeCtx'types $ module'userTypes m

    onRecTypes extract UserType{..} = onField extract =<< (M.toList $ userType'cases)

    onField fromField (cons, def) = case def of
      ConsDef _ -> []
      RecordCons fields ->
        let size = V.length fields
            sel  = SelectIndex size
        in  zipWith (\n x -> fromField cons (sel n) x) [0..] $ V.toList fields

    extractSelectors = onRecTypes $ \cons index RecordField{..} ->
      simpleBind recordField'name (selectorFun cons index)

    selectorFun cons SelectIndex{..} =
      Fix $ Lam noLoc (PCons noLoc cons args) $ Fix $ Var noLoc vx
      where
        args = V.toList $ (V.replicate selectIndex'size $ PWildCard noLoc) V.// [(selectIndex'id, pvx)]
        vx   = VarName noLoc "x"
        pvx  = PVar noLoc vx

    extractUpdaters = onRecTypes $ \cons index RecordField{..} ->
      simpleBind (recordFieldUpdateFunName $ recordField'name) (updaterFun cons index)

    updaterFun cons SelectIndex{..} =
      Fix $ LamList noLoc [pvx, PCons noLoc cons inArgs] $ Fix $ Cons noLoc cons outArgs
      where
        inArgs  = V.toList $ (V.fromList $ fmap (PVar noLoc) args) V.// [(selectIndex'id, wild)]
        outArgs = (V.fromList $ fmap (Fix . Var noLoc) args) V.// [(selectIndex'id, Fix $ Var noLoc vx)]
        args    = fmap (VarName noLoc . mappend "z" . showt) [0 .. selectIndex'size - 1]

        vx   = VarName noLoc "x"
        pvx  = PVar noLoc vx
        wild = PWildCard noLoc

recordFieldUpdateFunName :: VarName -> VarName
recordFieldUpdateFunName VarName{..} = VarName
  { varName'loc  = varName'loc
  , varName'name = secretVar $ mappend "update_" varName'name
  }

------------------------------------------------------------
-- compile main module

moduleToMainExpr :: MonadLang m => TypeContext -> Module -> m Lang
moduleToMainExpr typeCtx prog = do
  modCtx <- liftEither $ evalModule typeCtx prog
  return $ appendExecCtx (moduleCtx'exprs modCtx) (Fix $ Var noLoc "main")

-- | type-checks all bindings in the module.
--
-- It takes type-context for all free-variables in the module and module itself
-- and returns Nothing if module is correct or Just error in case that some types are ill defined.
checkMainModule :: TypeContext -> Module -> Maybe Error
checkMainModule types m =
  either Just (const Nothing) $
    runInferM $ do
      modCtx <- liftEither $ evalModule types m
      let ctx  = moduleCtx'types modCtx
          ctx' = ctx { inferCtx'binds = types <> inferCtx'binds ctx }
      inferExpr ctx' =<< moduleToMainExpr types m


appendExecCtx :: ExecCtx -> Lang -> Lang
appendExecCtx ctx expr
  | null binds = expr
  | otherwise  = Fix $ Let (H.getLoc expr) binds expr
  where
    binds = mconcat $ fmap (\(var, rhs) -> simpleBind var rhs) $ M.toList $
      execCtx'vars $ pruneExecCtx expr ctx

-- | It removes all bindings from context that not used in the given expresiion
pruneExecCtx :: Lang -> ExecCtx -> ExecCtx
pruneExecCtx expr (ExecCtx ctx) =
  ExecCtx $ M.intersection ctx $ varsToMap vars
  where
    varsToMap vs = M.fromList $ fmap (, ()) $ S.toList vs

    vars = getUsedVars ctx (freeVars expr) S.empty

    getUsedVars m cur res
      | S.null cur = res
      | otherwise = getUsedVars (M.difference m curMap) next (cur <> res)
      where
        next = foldMap (\var -> maybe S.empty freeVars $ M.lookup var m) cur
        curMap = varsToMap cur

--------------------------------------------
--

data TrimCtx = TrimCtx
  { trimCtx'included     :: Set VarName
  , trimCtx'resultBinds  :: [Bind Lang]
  , trimCtx'srcBinds     :: Map VarName (Bind Lang)
  , trimCtx'types        :: Map VarName Signature
  }

newTrimCtx :: Binds Lang -> TrimCtx
newTrimCtx binds = TrimCtx
  { trimCtx'included    = S.empty
  , trimCtx'resultBinds = []
  , trimCtx'srcBinds    = src
  , trimCtx'types       = binds'types binds
  }
  where
    src = M.fromList $ (\b -> fmap (, b) (bindNames b)) =<< binds'decls binds

trimResult :: TrimCtx -> Binds Lang
trimResult TrimCtx{..} = Binds
  { binds'types = trimCtx'types `M.intersection` (M.fromList $ fmap (, ()) $ S.toList trimCtx'included)
  , binds'decls = trimCtx'resultBinds
  }

isIncluded :: VarName -> TrimCtx -> Bool
isIncluded name TrimCtx{..} = S.member name trimCtx'included

getBindByName :: VarName -> TrimCtx -> Maybe (Bind Lang)
getBindByName name TrimCtx{..} = M.lookup name trimCtx'srcBinds

insertBind :: Bind Lang -> TrimCtx -> TrimCtx
insertBind b ctx = ctx
  { trimCtx'resultBinds = b : trimCtx'resultBinds ctx
  , trimCtx'included    = trimCtx'included ctx <> (S.fromList $ bindNames b)
  }

-- | Removes all bindings that are not reachable from main function.
trimModuleByMain :: MonadLang m => Module -> m Module
trimModuleByMain m = fmap (\bs -> m { module'binds = bs }) $
  go (newTrimCtx $ module'binds m) (Seq.fromList [VarName noLoc "main"])
  where
    go ctx names = case Seq.viewl names of
      EmptyL       -> pure $ trimResult ctx
      name :< rest ->
        if isIncluded name ctx
          then go ctx rest
          else case getBindByName name ctx of
                 Just bind -> go (insertBind bind ctx) (getFreeVars bind <> rest)
                 Nothing   ->
                        if isPreludeFun name
                          then go ctx rest
                          else throwError $ ExecError $ UnboundVariables [name]

    getFreeVars :: Bind Lang -> Seq.Seq VarName
    getFreeVars = Seq.fromList . S.toList . foldMap (freeVarsAlt . fmap freeVars) . bindAlts

    baseNamesSet = S.fromList $ constrNames <> recordFieldNames <> baseNames
    isPreludeFun v = S.member (varName'name v) baseNamesSet

    constrNames = fmap consName'name $ M.keys $ userTypeCtx'constrs $ module'userTypes m
    recordFieldNames = M.keys $ userTypeCtx'recFields $ module'userTypes m

toUserTypeCtx :: [UserType] -> UserTypeCtx
toUserTypeCtx typeDecls =
  setupUserTypeInfo $ (\ts -> UserTypeCtx (baseLibTypes <> ts) mempty mempty mempty mempty) $ M.fromList $ fmap (\x -> (userType'name x, x)) typeDecls

orderTypesByDeps :: [UserType] -> [UserType]
orderTypesByDeps = G.flattenSCCs . G.stronglyConnComp . fmap toDep
  where
    toDep ut = (ut, userType'name ut, getTypeDeps ut)

    getTypeDeps :: UserType -> [VarName]
    getTypeDeps UserType{..} = getTypeNames =<< (V.toList . getConsTypes) =<< M.elems userType'cases

    getTypeNames :: Type -> [VarName]
    getTypeNames (H.Type ty) = foldFix go ty
      where
        go = \case
          H.ConT loc name args -> VarName loc name : concat args
          H.ArrowT _ a b       -> a <> b
          H.TupleT _ ts        -> mconcat ts
          H.ListT _ a          -> a
          H.VarT _ _           -> []

