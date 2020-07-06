-- | Turns polymorphic programs to monomorphic ones
module Hschain.Utxo.Lang.Compile.Monomorphize(
  makeMonomorphic
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
import Hschain.Utxo.Lang.Compile.Infer (TypedProg)
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (primToType, funT)
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Typed(..), Type)
import Hschain.Utxo.Lang.Expr (Loc, noLoc, boolT, VarName(..))

import qualified Language.HM as H
import qualified Language.HM.Subst as H

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

-- | Makes types monomorphic.
makeMonomorphic :: MonadLang m => TypedProg -> m TypedProg
makeMonomorphic prog = runMono progMap (makeMono context)
  where
    progMap = M.fromList $ fmap (\x -> (def'name x, x)) prog
    context = fmap getDefType progMap

type Mono a = StateT MonoSt (Either Error) a
type ProgMap = Map Name TypedDef
type TypedDef = AnnComb Type (Typed Name)
type Context = Map Name Type
type Subst = H.Subst () Name
type TypedExpr = AnnExpr Type (Typed Name)

runMono :: MonadLang m => ProgMap -> Mono a -> m TypedProg
runMono sourceProg m = liftEither $ fmap (M.elems . monoSt'resultProg) $ execStateT m initSt
  where
    initSt = MonoSt
      { monoSt'seeds = initSeed
      , monoSt'sourceProg = sourceProg
      , monoSt'resultProg = M.empty
      }

    initSeed = Seq.singleton $ Typed "main" $ fromType boolT

fromType :: H.Type Loc Name -> Type
fromType = H.mapLoc (const ())

data MonoSt = MonoSt
  { monoSt'seeds       :: Seq (Typed Name)  -- ^ Free variables found in the definition,
                                            --   with monomorphic types
  , monoSt'sourceProg  :: ProgMap           -- ^ original set of definitions (and also we add specified definitions here)
  , monoSt'resultProg  :: ProgMap           -- ^ Result of the algorithm
  }

makeMono :: Context -> Mono ()
makeMono ctx = do
  st <- get
  mapM_ (procSeed ctx $ monoSt'resultProg st) $ monoSt'seeds st

procSeed :: Context -> ProgMap -> Typed Name -> Mono ()
procSeed ctx resultProgMap seed
  | isMonoT $ typed'type seed = do
      progMap <- getSourceProg
      case M.lookup name progMap of
        Just defn -> withCheckCache name (procDef ctx defn)
        Nothing   -> errorUnboundVar noLoc name
  | otherwise  = errorNoMonoType name
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

substDef :: Context -> TypedDef -> Mono ([Typed Name], TypedDef)
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
  { substResult'seeds       :: [Typed Name]               -- ^ free variables that expression depends on
  , _substResult'localSubst :: LetSubst                   -- ^ type substitution for local definitions
  , substResult'expr        :: AnnExpr Type (Typed Name)  -- ^ result expression
  }

substExpr :: SubstCtx -> AnnExpr Type (Typed Name) -> Mono SubstResult
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
    EBottom loc                 -> onBottom loc
  where
    rec = substExpr env

    -- | TODO: consider rewrite for poly to mono-types
    onVar loc name =
      case M.lookup name $ substCtx'types env of
        Just varTy -> do
          if isMonoT varTy
            then do
              let letSubsts = if isLetLocal then (singletonLetSubst name LocalIdentitySubst) else mempty
              return $ SubstResult (seeds varTy) letSubsts (Fix $ Ann varTy $ EVar loc name)
            else do
              (ty', subst) <- unifySubst ty varTy
              if isMonoT ty'
                then do
                  let name' = getSubstName subst name
                      resE = Fix $ Ann ty' $ EVar loc name'
                  globalSeeds <- checkGlobalSubst subst ty' loc name
                  let localSubst = if isLetLocal then singletonLetSubst name (LocalSubst { localSubst'subst = subst, localSubst'name = name'}) else mempty
                  return (SubstResult (globalSeeds <> seeds ty') localSubst resE)
                else errorNoMonoType name
        Nothing -> errorUnboundVar loc name
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
                return [Typed (def'name defn') varMonoTy]
            | otherwise = return []


    onPrim loc prim = return $ SubstResult [] mempty (Fix $ Ann (primToType prim) $ EPrim loc prim)

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
          Nothing -> errorNoMonoType "ap-nothing"


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

            Nothing -> errorUnboundVar (H.getLoc expr) name

        checkAlreadyDefined name cont =
          if S.member name prevBinds
            then return ([], [], [])
            else cont

    unique (binds, seeds) = (L.nubBy ((==) `on` (typed'value. fst)) binds, L.nub seeds)

    onLam loc args body
      | all (isMonoT . typed'type) args && (isMonoT $ getAnnType body) = do
          let ctx' = (M.fromList $ zip argNames argTypes) <> (substCtx'types env)
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
            lamTy = funT (fmap typed'type args) (getAnnType body)
            argNames = fmap typed'value args
            argTypes = fmap typed'type args

    onIf loc c t e = do
      (SubstResult cF cL cE) <- rec $ setAnn (fromType boolT) c
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
        else errorNoMonoType "For Case-expr"
      where
        substAlt eT (resSeeds, resLocSubst, res) ca@CaseAlt{..} = do
          (ty', subst) <- unifySubst eT caseAlt'constrType
          let args  = fmap (\a -> a { typed'type = H.apply subst $ typed'type a }) caseAlt'args
              types = M.fromList $ fmap (\a -> (typed'value a, typed'type a)) args
              ctx'  = SubstCtx (substCtx'locals env <> S.fromList (fmap typed'value args)) (substCtx'letBinds env) (types <> substCtx'types env)
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
            else errorNoMonoType $ mconcat ["Constr-", showt m, "-", showt n]

    onBottom loc = return $ SubstResult [] mempty (Fix $ Ann ty $ EBottom loc)

    setAnn t (Fix (Ann _ e)) = Fix $ Ann t e

    haveMonoTs = all (isMonoT . getAnnType)

specifyDef :: Subst -> TypedDef -> TypedDef
specifyDef subst def@Def{..} = def
  { def'name = getSubstName subst def'name
  , def'args = fmap (\t -> t{ typed'type = H.apply subst $ typed'type t }) def'args
  , def'body = applySubstAnnExpr subst def'body
  }

applySubstAnnExpr :: Subst -> TypedExpr -> TypedExpr
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


getSubstName :: Subst -> Name -> Name
getSubstName (H.Subst m) name =
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

isMonoExpr :: TypedExpr -> Bool
isMonoExpr = isMonoT . getAnnType

isMonoT :: Type -> Bool
isMonoT (H.Type x) = flip cata x $ \case
  H.VarT _ _     -> False
  H.ConT _ _ as  -> and as
  H.ArrowT _ a b -> a && b
  H.ListT _ a    -> a
  H.TupleT _ as  -> and as

addSeeds :: [Typed Name] -> Mono ()
addSeeds seeds =
  modify' $ \st -> st { monoSt'seeds = monoSt'seeds st <> Seq.fromList seeds }

insertResultDef :: TypedDef -> Mono ()
insertResultDef defn =
  modify' $ \st -> st { monoSt'resultProg = M.insert (def'name defn) defn $ monoSt'resultProg st }

insertSourceDef :: TypedDef -> Mono ()
insertSourceDef defn =
  modify' $ \st -> st { monoSt'sourceProg = M.insert (def'name defn) defn $ monoSt'sourceProg st }

getSourceProg :: Mono ProgMap
getSourceProg = fmap monoSt'sourceProg get

getSourceDef :: Loc -> Name -> Mono TypedDef
getSourceDef loc name = do
  mDef <- fmap (M.lookup name . monoSt'sourceProg) get
  maybe (errorUnboundVar loc name) pure mDef

getDefType :: TypedDef -> Type
getDefType Def{..} = foldr (\a b -> H.arrowT () a b) rhs args
  where
    args = fmap typed'type def'args
    rhs  = getAnnType def'body

getAnnType :: AnnExpr Type (Typed Name) -> Type
getAnnType (Fix (Ann ty _)) = ty

unify :: Type -> Type -> Mono Type
unify tA tB = fmap fst $ unifySubst tA tB

unifySubst :: Type -> Type -> Mono (Type, Subst)
unifySubst tA tB = case H.unifyTypes tA tB of
  Right subst -> return $ (H.apply subst tB, subst)
  Left err    -> throwError $ TypeError $ H.mapLoc (const noLoc) err

errorUnboundVar :: MonadError Error m => Loc -> Name -> m a
errorUnboundVar loc name = throwError $ ExecError $ UnboundVariables [VarName loc name]

errorNoMonoType :: MonadError Error m => Name -> m a
errorNoMonoType name = throwError $ MonoError $ FailedToFindMonoType name