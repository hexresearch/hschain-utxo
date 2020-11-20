-- | Turns polymorphic programs to monomorphic ones
module Hschain.Utxo.Lang.Compile.Monomorphize(
    makeMonomorphic
  , specifyCompareOps
) where

import Hex.Common.Text

import Control.Arrow (first)
import Control.Monad.State.Strict

import Data.Fix
import Data.Foldable (toList)
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Sequence (Seq)

import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.Expr      (PrimOp(..))
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (primToType)
import Hschain.Utxo.Lang.Core.Types             (Name, Typed(..))
import Hschain.Utxo.Lang.Types (argTypes)
import Hschain.Utxo.Lang.Expr ( Loc, noLoc, boolT, VarName(..), argTypeName
                              , funT, boolT, typeCoreToType)

import qualified Language.HM as H
import qualified Language.HM.Subst as H

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

-- | Makes types monomorphic.
makeMonomorphic :: MonadLang m => TypedLamProg -> m TypedLamProg
makeMonomorphic (AnnLamProg prog) = runMono progMap (makeMono context)
  where
    progMap = M.fromList $ fmap (\x -> (varName'name $ def'name x, x)) prog
    context = fmap getDefType progMap

type Mono a = StateT MonoSt (Either Error) a
type ProgMap = Map Name TypedDef
type Context = Map Name (H.Type () Name)
type Subst = H.Subst () Name

runMono :: MonadLang m => ProgMap -> Mono a -> m TypedLamProg
runMono sourceProg m = liftEither $ fmap (AnnLamProg . M.elems . monoSt'resultProg) $ execStateT m initSt
  where
    initSt = MonoSt
      { monoSt'seeds = initSeed
      , monoSt'sourceProg = sourceProg
      , monoSt'resultProg = M.empty
      }
    initSeed = Seq.singleton $ Typed "main" boolT

data MonoSt = MonoSt
  { monoSt'seeds       :: Seq (Typed (H.Type () Name) Name)
    -- ^ Free variables found in the definition, with monomorphic types
  , monoSt'sourceProg  :: ProgMap
    -- ^ original set of definitions (and also we add specified definitions here)
  , monoSt'resultProg  :: ProgMap
    -- ^ Result of the algorithm
  }

makeMono :: Context -> Mono ()
makeMono ctx = do
  st <- get
  mapM_ (procSeed ctx $ monoSt'resultProg st) $ monoSt'seeds st

procSeed :: Context -> ProgMap -> Typed (H.Type () Name) Name -> Mono ()
procSeed ctx resultProgMap seed
  | isMonoT $ typed'type seed = do
      progMap <- getSourceProg
      case M.lookup name progMap of
        Just defn -> withCheckCache name (procDef ctx defn)
        Nothing   -> failedToFindMonoType noLoc name
  | otherwise  = failedToFindMonoType noLoc name
  where
    name = typed'value seed

    withCheckCache key act
      | M.member key resultProgMap = return ()
      | otherwise                  = act

procDef :: Context -> TypedDef -> Mono ()
procDef ctx defn = do
  (newSeeds, defn') <- substDef ctx defn
  addSeeds newSeeds
  insertResultDef defn'

substDef :: Context -> TypedDef -> Mono ([Typed (H.Type () Name) Name], TypedDef)
substDef ctx defn@Def{..} = do
  res <- substExpr (SubstCtx locals mempty ctx') def'body
  return $ (substResult'seeds res, defn { def'body = substResult'expr res } )
  where
    ctx' = ctx <> argsT
    argsT = M.fromList $ fmap (\Typed{..} -> (typed'value, typed'type)) def'args
    locals = S.fromList $ fmap typed'value def'args

-- | Substitution context
data SubstCtx = SubstCtx
  { substCtx'locals    :: Set Name  -- ^ set of names visible from the local scope
  , substCtx'letBinds  :: Set Name  -- ^ set of bind-names from the closest upper scope
  , substCtx'types     :: Context   -- ^ available type definitions (both local and global)
  }

-- | we use this type to substitute local polymorphic definitions
-- with specified versions. We should allow let-polymorphism, i.e.
-- the possibility of usage the same polymorphic function in different contexts
-- (with different type specializations)
data LocalSubst = LocalSubst
  { localSubst'subst :: Subst  -- ^ type substitution
  , localSubst'name  :: Name   -- ^ new name for specialized version of definition
  }
  | LocalIdentitySubst         -- ^ substitution is needed
  deriving (Eq, Ord)

newtype LetSubst = LetSubst (Map Name (Set LocalSubst))

instance Semigroup LetSubst where
  (LetSubst a) <> (LetSubst b) = LetSubst $ M.unionWith (<>) a b

instance Monoid LetSubst where
  mempty = LetSubst M.empty

singletonLetSubst :: Name -> LocalSubst -> LetSubst
singletonLetSubst name subst = LetSubst $ M.singleton name $ S.singleton subst

removeLetSubsts :: [Name] -> LetSubst -> LetSubst
removeLetSubsts names (LetSubst m) = LetSubst $ M.difference m (M.fromList $ fmap (, ()) names)

-- Result of substitution of expression
data SubstResult = SubstResult
  { substResult'seeds       :: [Typed (H.Type () Name) Name]
    -- ^ free variables that expression depends on
  , _substResult'localSubst :: LetSubst        -- ^ type substitution for local definitions
  , substResult'expr        :: TypedExprLam    -- ^ result expression
  }

substExpr :: SubstCtx -> TypedExprLam -> Mono SubstResult
substExpr env (Fix (Ann ty expr)) =
  case expr of
    EVar loc name               -> onVar loc name
    EPrim loc prim              -> onPrim loc prim
    EAp loc f a                 -> onAp loc f a
    ELet loc bs e               -> onLet loc bs e
    ELam loc args e             -> onLam loc args e
    EIf loc c t e               -> onIf loc c t e
    ECase loc e alts            -> onCase loc e alts
    EConstr loc conTy m n       -> onConstr loc conTy m n
    EAssertType loc e ety       -> onAssertType loc e ety
    EBottom loc                 -> onBottom loc
    EPrimOp _ _                 -> undefined
  where
    rec = substExpr env

    -- | TODO: consider rewrite for poly to mono-types
    onVar loc name =
      case M.lookup name $ substCtx'types env of
        Just varTy
          | isMonoT varTy -> do
              let letSubsts | isLetLocal = singletonLetSubst name LocalIdentitySubst
                            | otherwise  = mempty
              return $ SubstResult (seeds varTy) letSubsts (Fix $ Ann varTy $ EVar loc name)
          | otherwise -> do
              (ty', subst) <- unifySubst ty varTy
              if isMonoT ty'
                then do
                  let name' = varName'name $ getSubstName subst (VarName loc name)
                      resE = Fix $ Ann ty' $ EVar loc name'
                  globalSeeds <- checkGlobalSubst subst ty' loc name
                  let localSubst | isLetLocal = singletonLetSubst name
                                              $ LocalSubst { localSubst'subst = subst
                                                           , localSubst'name = name'}
                                 | otherwise  = mempty
                  return (SubstResult (globalSeeds <> seeds ty') localSubst resE)
                else failedToFindMonoType noLoc name
        Nothing -> unboundVariable $ VarName loc name
        where
          isLetLocal = S.member name (substCtx'letBinds env)
          isLocal = S.member name (substCtx'locals env)
          isGlobal = not isLocal

          seeds t
            | isLocal   = []
            | otherwise = [Typed name t]

          checkGlobalSubst subst varMonoTy src varName
            | isGlobal  = do
                defn  <- getSourceDef src varName
                let defn' = specifyDef subst defn
                insertSourceDef defn'
                return [Typed (varName'name $ def'name defn') varMonoTy]
            | otherwise = return []

    onPrim loc prim = return $ SubstResult [] mempty
      (Fix $ Ann (typeCoreToType $ primToType $ primLoc'value prim) $ EPrim loc prim)

    onAp loc f a
      | haveMonoTs [f, a] = do
        (SubstResult fF fL fE) <- rec f
        (SubstResult aF aL aE) <- rec a
        return $ SubstResult (fF <> aF) (fL <> aL) (Fix $ Ann ty $ EAp loc fE aE)
      | otherwise = do
        ty' <- unify (H.arrowT () (H.varT () "$1" ) ty) (getAnnType f )
        (SubstResult fF fL fE) <- rec $ setAnn ty' f
        case H.extractArrow ty' of
          Just (lhs, _) -> do
            aT' <- unify lhs (getAnnType a)
            (SubstResult aF aL aE) <- rec $ setAnn aT' a
            return $ SubstResult (fF <> aF) (fL <> aL) (Fix $ Ann ty' $ EAp loc fE aE)
          Nothing -> failedToFindMonoType noLoc "ap-nothing"


    onLet loc binds body = do
      ty' <- unify ty (getAnnType body)
      SubstResult bodyF localSubst bodyE <- substExpr ctx' $ setAnn ty' body
      (binds', bindSeeds) <- substLocalLetBinds ctx' bindMap bindNames S.empty localSubst
      let localSubst' = removeLetSubsts (fmap (typed'value . fst) binds') localSubst
      return $ SubstResult (bodyF <> bindSeeds) localSubst' (Fix $ Ann ty' $ ELet loc binds' bodyE)
      where
        bindNames   = S.fromList $ fmap (typed'value . fst) binds
        bindTypeCtx = M.fromList $ fmap ((\(Typed name t) -> (name, t)) . fst) binds

        ctx' = SubstCtx locals' bindNames types'
        locals' = bindNames   <> substCtx'locals env
        types'  = bindTypeCtx <> substCtx'types env

        bindMap = M.fromList $ fmap (\(b, e) -> (typed'value b, (b, e))) binds

    flatSubst (LetSubst m) = foldMap fromLocalSubsts $ M.toList m
      where
        fromLocalSubsts (name, subst) = fmap (name,) $ toList subst

    substLocalLetBinds bindCtx bindMap bindNames prevBinds letSubst =
      fmap unique $ substLocalLetBindList bindCtx bindMap bindNames prevBinds ([], []) $ flatSubst letSubst

    substLocalLetBindList bindCtx bindMap bindNames prevBinds res letSubsts =
      case letSubsts of
        []   -> return res
        a:as -> do
          (binds, seeds, nextSubst) <- substLocalBind a
          let newNames = S.fromList $ fmap (typed'value . fst) binds
          substLocalLetBindList bindCtx bindMap bindNames (prevBinds <> newNames) (res <> (binds, seeds)) (nextSubst <> as)
      where
        substLocalBind (name, locSub) =
          case M.lookup name bindMap of
            Just (var, rhs) ->
              case locSub of
                LocalIdentitySubst       -> checkAlreadyDefined name $ do
                  SubstResult seeds localSubst rhs' <- substExpr bindCtx rhs
                  let binds = [(var, rhs')]
                      nextSubst = flatSubst localSubst
                  return (binds, seeds, nextSubst)
                LocalSubst subst newName -> checkAlreadyDefined newName $ do
                  SubstResult seeds localSubst rhs' <- substExpr bindCtx $ applySubstAnnExpr subst rhs
                  let binds = [(Typed newName (H.apply subst $ typed'type var), rhs')]
                      nextSubst = flatSubst localSubst
                  return (binds, seeds, nextSubst)

            Nothing -> unboundVariable $ VarName (H.getLoc expr) name

        checkAlreadyDefined name cont =
          if S.member name prevBinds
            then return ([], [], [])
            else cont

    unique (binds, seeds) = (L.nubBy ((==) `on` (typed'value. fst)) binds, L.nub seeds)

    onLam loc args body
      | all (isMonoT . typed'type) args && (isMonoT $ getAnnType body) = do
          let ctx' = (M.fromList $ zip argNames argTs) <> (substCtx'types env)
              locals' = S.fromList argNames <> (substCtx'locals env)
          (SubstResult bodyF bodyL bodyE) <- substExpr (SubstCtx locals' (substCtx'letBinds env) ctx') body
          return $ SubstResult bodyF bodyL (Fix $ Ann ty $ ELam loc args bodyE)
      | otherwise = do
          ty' <- unify ty lamTy
          let (lamArgsT, lamRhsT) = H.extractFunType ty'
              ctx' = (M.fromList $ zip argNames lamArgsT) <> (substCtx'types env)
              locals' = S.fromList argNames <> (substCtx'locals env)
          (SubstResult bodyF bodyL bodyE) <- substExpr (SubstCtx locals' (substCtx'letBinds env) ctx') $ setAnn lamRhsT body
          let argsE = zipWith Typed argNames lamArgsT
          return $ SubstResult bodyF bodyL (Fix $ Ann ty' $ ELam loc argsE bodyE)
          where
            lamTy    = funT (fmap typed'type args) (getAnnType body)
            argNames = fmap typed'value args
            argTs    = fmap typed'type args

    onIf loc c t e = do
      (SubstResult cF cL cE) <- rec $ setAnn boolT c
      (SubstResult tF tL tE) <- rec $ setAnn ty t
      (SubstResult eF eL eE) <- rec $ setAnn ty e
      return $ SubstResult (mconcat [cF, tF, eF]) (mconcat [cL, tL, eL]) (Fix $ Ann ty $ EIf loc cE tE eE)

    onCase loc e alts = do
      SubstResult eSeeds eLocalSubst e' <- rec e
      if isMonoExpr e'
        then do
          let eT = getAnnType e'
          (altSeeds, altLocalSubst, alts') <- foldM (substAlt eT) (mempty, mempty, mempty) alts
          return $ SubstResult (eSeeds <> altSeeds) (eLocalSubst <> altLocalSubst) (Fix $ Ann ty $ ECase loc e' alts')
        else failedToFindMonoType loc "For Case-expr"
      where
        substAlt eT (resSeeds, resLocSubst, res) ca@CaseAlt{..} = do
          (ty', subst) <- unifySubst eT caseAlt'constrType
          let args  = fmap (\a -> a { typed'type = H.apply subst $ typed'type a }) caseAlt'args
              types = M.fromList $ fmap (\a -> (typed'value a, typed'type a)) args
              ctx'  = SubstCtx
                (substCtx'locals env <> S.fromList (fmap typed'value args))
                (substCtx'letBinds env)
                (types <> substCtx'types env)
          SubstResult seeds locSubst rhs <- substExpr ctx' caseAlt'rhs
          let alt = ca { caseAlt'args       = args
                       , caseAlt'constrType = ty'
                       , caseAlt'rhs        = rhs
                       }
          return (resSeeds <> seeds, resLocSubst <> locSubst, alt : res )


    onConstr loc conTy m n
      | isMonoT conTy = return $ SubstResult [] mempty (Fix $ Ann conTy $ EConstr loc conTy m n)
      | otherwise = do
          ty' <- unify ty conTy
          if isMonoT ty'
            then return $ SubstResult [] mempty (Fix $ Ann ty' $ EConstr loc ty' m n)
            else failedToFindMonoType loc $ mconcat ["Constr-", showt m, "-", showt n]

    -- | TODO: consider polymorphic type annotations
    onAssertType loc e ety = do
      (SubstResult eF eL eE) <- rec e
      return $ SubstResult eF eL (Fix $ Ann ty $ EAssertType loc eE ety)

    onBottom loc = return $ SubstResult [] mempty (Fix $ Ann ty $ EBottom loc)

    setAnn t (Fix (Ann _ e)) = Fix $ Ann t e

    haveMonoTs = all (isMonoT . getAnnType)

specifyDef :: Subst -> TypedDef -> TypedDef
specifyDef subst def@Def{..} = def
  { def'name = getSubstName subst def'name
  , def'args = fmap (\t -> t{ typed'type = H.apply subst $ typed'type t }) def'args
  , def'body = applySubstAnnExpr subst def'body
  }

applySubstAnnExpr :: Subst -> TypedExprLam -> TypedExprLam
applySubstAnnExpr subst = cata $ \case
  Ann ty expr -> Fix $ Ann (H.apply subst ty) $
    case expr of
      ELet loc binds e   -> ELet loc (fmap (first applyTyped) binds) e
      ELam loc args a    -> ELam loc (fmap applyTyped args) a
      EConstr loc t m n  -> EConstr loc (H.apply subst t) m n
      ECase loc e alts   -> ECase loc e (fmap applyAlt alts)
      other              -> other
  where
    applyTyped t = t { typed'type = H.apply subst $ typed'type t }
    applyAlt alt@CaseAlt{..} = alt
      { caseAlt'args       = fmap applyTyped caseAlt'args
      , caseAlt'constrType = H.apply subst caseAlt'constrType
      }


getSubstName :: Subst -> VarName -> VarName
getSubstName (H.Subst m) (VarName loc name) = VarName loc $
  mconcat $ L.intersperse delim $ (name :) $ fmap (toTypeName . snd) $ M.toList m
  where
    delim = ":"

    toTypeName (H.Type x) = flip cata x $ \case
      H.VarT   _ v    -> v
      H.ConT   _ n xs -> joinNames $ "Cons" : n : xs
      H.ArrowT _ f a  -> joinNames [f, a]
      H.TupleT _ xs   -> joinNames $ "Tuple" : xs
      H.ListT  _ a    -> joinNames ["List", a]

    joinNames = mconcat . L.intersperse "_"

isMonoExpr :: TypedExprLam -> Bool
isMonoExpr = isMonoT . getAnnType

isMonoT :: H.Type loc Name -> Bool
isMonoT (H.Type x) = flip cata x $ \case
  H.VarT _ _     -> False
  H.ConT _ _ as  -> and as
  H.ArrowT _ a b -> a && b
  H.ListT _ a    -> a
  H.TupleT _ as  -> and as

addSeeds :: [Typed (H.Type () Name) Name] -> Mono ()
addSeeds seeds =
  modify' $ \st -> st { monoSt'seeds = monoSt'seeds st <> Seq.fromList seeds }

insertResultDef :: TypedDef -> Mono ()
insertResultDef defn =
  modify' $ \st -> st { monoSt'resultProg = M.insert (varName'name $ def'name defn) defn $ monoSt'resultProg st }

insertSourceDef :: TypedDef -> Mono ()
insertSourceDef defn =
  modify' $ \st -> st { monoSt'sourceProg = M.insert (varName'name $ def'name defn) defn $ monoSt'sourceProg st }

getSourceProg :: Mono ProgMap
getSourceProg = fmap monoSt'sourceProg get

getSourceDef :: Loc -> Name -> Mono TypedDef
getSourceDef loc name = do
  mDef <- fmap (M.lookup name . monoSt'sourceProg) get
  maybe (unboundVariable $ VarName loc name) pure mDef

getDefType :: TypedDef -> H.Type () Name
getDefType Def{..} = foldr (\a b -> H.arrowT () a b) rhs args
  where
    args = fmap typed'type def'args
    rhs  = getAnnType def'body

getAnnType :: TypedExprLam -> H.Type () Name
getAnnType (Fix (Ann ty _)) = ty

unify :: (Show loc, Eq loc) => H.Type loc Name -> H.Type loc Name -> Mono (H.Type loc Name)
unify tA tB = fmap fst $ unifySubst tA tB

unifySubst :: (Show loc, Eq loc) => H.Type loc Name -> H.Type loc Name -> Mono (H.Type loc Name, H.Subst loc Name)
unifySubst tA tB = case H.unifyTypes tA tB of
  Right subst -> return $ (H.apply subst tB, subst)
  Left err    -> throwError $ TypeError $ H.mapLoc (const noLoc) err


-- | Substitutes polymorphic comparison operators to
-- monomorphic ones. After type checking we have precise
-- information to what operator we should specify.
-- If it still remains polymorphic we throw an error
-- that we have failed to specify the types.
specifyCompareOps :: MonadLang m => TypedLamProg -> m TypedLamProg
specifyCompareOps = liftTypedLamProg $ cataM $ \case
  Ann ty expr -> fmap (Fix . Ann ty) $ case expr of
    EVar loc "listAt" -> EPrimOp loc . OpListAt     <$> getLParam1 "listAt" loc ty
    EVar loc "length" -> EPrimOp loc . OpListLength <$> getLParam1 "length" loc ty
    EVar loc name
      | Just op <- fromCompName name -> do
          cmpT <- fromCompType name loc ty
          return $ EPrimOp loc $ op (H.mapLoc (const ()) cmpT)
    other -> pure other
  where
    fromCompName name = case name of
      "==" -> Just OpEQ
      "/=" -> Just OpNE
      "<"  -> Just OpLT
      "<=" -> Just OpLE
      ">"  -> Just OpGT
      ">=" -> Just OpGE
      _    -> Nothing

    getLParam1 name loc (H.Type (Fix ty)) = case ty of
      H.ArrowT _ (Fix (H.ListT _ t)) _ -> return (H.Type t)
      _                                -> failedToFindMonoType loc name

    fromCompType name loc (H.Type (Fix ty)) = case ty of
      H.ArrowT _ a (Fix (H.ArrowT _ b (Fix (H.ConT _ "Bool" []))))
        | isPrimType a && a == b -> pure $ H.Type a
        | isMonoT $ H.Type a     -> compareForNonPrim loc
        | otherwise              -> failedToFindMonoType loc name
      _ -> compareForNonPrim loc

    isPrimType (Fix x) = case x of
      H.ConT _ name [] -> isPrimTypeName name
      _                -> False
      where
        isPrimTypeName name = any ((name ==) . argTypeName) argTypes

