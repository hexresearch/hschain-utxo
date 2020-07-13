-- | Defines type-inference algorithm.
module Language.HM.Infer(
    Context(..)
  , insertContext
  , inferType
  , inferTerm
  , subtypeOf
  , unifyTypes
) where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Fix
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Maybe

import Language.HM.Term
import Language.HM.Subst
import Language.HM.Type
import Language.HM.TypeError
import Language.HM.TyTerm

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
{-
import Debug.Trace
import Text.Show.Pretty
-}

-- Synonims to simplify typing
type Context' loc v = Context (Origin loc) v
type Type' loc v = Type (Origin loc) v
type Term' prim loc v = Term prim (Origin loc) v
type TyTerm' prim loc v = TyTerm prim (Origin loc) v
type Signature' loc v = Signature (Origin loc) v
type Subst' loc v = Subst (Origin loc) v
type Bind' loc v a = Bind (Origin loc) v a
type VarSet' loc v = VarSet (Origin loc) v
type CaseAlt' loc v = CaseAlt (Origin loc) v

-- | Context holds map of proven signatures for free variables in the expression.
newtype Context loc v = Context { unContext :: Map v (Signature loc v) }
  deriving (Show, Eq, Semigroup, Monoid)

insertContext :: Ord v => v -> Type loc v -> Context loc v -> Context loc v
insertContext v ty (Context m) = Context $ M.insert v (monoT ty) m

instance CanApply Context where
  apply subst = Context . fmap (apply subst) . unContext

insertCtx :: Ord v => v -> Signature loc v ->  Context loc v -> Context loc v
insertCtx v sign (Context ctx) = Context $ M.insert v sign ctx


-- | We leave in the context only terms that are truly needed.
-- To check the term we need only variables that are free in the term.
-- So we can safely remove everything else and speed up lookup times.
restrictContext :: Ord v => Term prim loc v -> Context loc v -> Context loc v
restrictContext t (Context ctx) = Context $ M.intersection ctx fv
  where
    fv = M.fromList $ fmap (, ()) $ S.toList $ freeVars t

markProven :: Ord v => Context loc v -> Context (Origin loc) v
markProven = Context . M.map (mapLoc Proven) . unContext

markUserCode :: Term prim loc v -> Term prim (Origin loc) v
markUserCode = mapLoc UserCode

chooseUserOrigin :: Show a => Origin a -> Origin a -> a
chooseUserOrigin x y = case (x, y) of
  (UserCode a, _) -> a
  (_, UserCode a) -> a
  _               -> fromOrigin x

-- | Type-tag for source locations to distinguish proven types from those
-- that have to be checked.
--
-- We use it on unification failure to show source locations in the user code and not in the
-- expression that is already was proven.
data Origin a
  = Proven a
  -- ^ Proven source code location
  | UserCode a
  -- ^ User source code (we type-check it)
  deriving (Show, Functor)

fromOrigin :: Origin a -> a
fromOrigin = \case
  Proven   a -> a
  UserCode a -> a

instance Eq a => Eq (Origin a) where
  (==) = (==) `on` fromOrigin

instance Ord a => Ord (Origin a) where
  compare = compare `on` fromOrigin

instance HasLoc a => HasLoc (Origin a) where
  type Loc (Origin a) = Loc a
  getLoc = getLoc . fromOrigin

-- | Type-inference monad.
-- Contains integer counter for fresh variables and possibility to report type-errors.
newtype InferM loc var a = InferM (StateT Int (Except (TypeError loc var)) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadError (TypeError loc var))

-- | Runs inference monad.
runInferM :: InferM loc var a -> Either (TypeError loc var) a
runInferM (InferM m) = runExcept $ evalStateT m 0

-- | Type-inference function.
-- We provide a context of already proven type-signatures and term to infer the type.
inferType :: (IsVar var, IsPrim prim, PrimLoc prim ~ loc, PrimVar prim ~ var, Show loc, Eq loc)
  => Context loc var -> Term prim loc var -> Either (TypeError loc var) (Type loc var)
inferType ctx term =
  fmap (normaliseType . mapLoc fromOrigin . (\(_, ty, _) -> ty)) $
    runInferM $ infer (markProven $ restrictContext term ctx) (markUserCode term)

-- | Infers types for all subexpressions of the given term.
-- We provide a context of already proven type-signatures and term to infer the type.
inferTerm :: (IsVar var, IsPrim prim, PrimLoc prim ~ loc, PrimVar prim ~ var, Show loc, Eq loc)
  => Context loc var -> Term prim loc var -> Either (TypeError loc var) (Type loc var, TyTerm prim loc var)
inferTerm ctx term =
  fmap ((\(_, ty, tyTerm) -> (toType ty, toTyTerm tyTerm))) $
    runInferM $ infer (markProven $ restrictContext term ctx) (markUserCode term)
  where
    toType   = normaliseType . mapLoc fromOrigin
    toTyTerm = mapType normaliseType . mapLoc fromOrigin

type Out prim loc var = (Subst (Origin loc) var, Type (Origin loc) var, TyTerm prim (Origin loc) var)
type InferOut prim loc var = InferM loc var (Out prim loc var)

infer :: (Eq loc, IsVar var, Show loc, IsPrim prim, PrimLoc prim ~ loc, PrimVar prim ~ var)
  => Context' loc var -> Term' prim loc var -> InferOut prim loc var
infer ctx (Term (Fix x)) = case x of
  Var loc v           -> inferVar ctx loc v
  Prim loc p          -> inferPrim loc p
  App loc a b         -> inferApp ctx loc (Term a) (Term b)
  Lam loc v r         -> inferLam ctx loc v (Term r)
  Let loc vs a        -> inferLet ctx loc (fmap (fmap Term) vs) (Term a)
  LetRec loc vs a     -> inferLetRec ctx loc (fmap (fmap Term) vs) (Term a)
  AssertType loc a ty -> inferAssertType ctx loc (Term a) ty
  Constr loc ty tag n -> inferConstr loc ty tag n
  Case loc e alts     -> inferCase ctx loc (Term e) (fmap (fmap Term) alts)
  Bottom loc          -> inferBottom loc

inferVar :: (Show loc, IsVar v)
  => Context (Origin loc) v -> Origin loc -> v -> InferOut prim loc v
inferVar ctx loc v = {- trace (unlines ["VAR", ppShow ctx, ppShow v]) $ -}
  fmap (\ty -> (mempty, ty, tyVarE ty loc v)) $ maybe err (newInstance . setLoc loc) $ M.lookup v (unContext ctx)
  where
    err = throwError $ NotInScopeErr (fromOrigin loc) v

inferPrim :: (Ord v, IsPrim prim, loc ~ PrimLoc prim, v ~ PrimVar prim)
  => Origin loc -> prim -> InferOut prim loc v
inferPrim loc prim =
  return (mempty, ty, tyPrimE ty loc prim)
  where
    ty = mapLoc UserCode $ getPrimType prim

inferApp :: (Eq loc, IsVar v, Show loc, IsPrim prim, loc ~ PrimLoc prim, v ~ PrimVar prim)
  => Context' loc v -> Origin loc -> Term' prim loc v -> Term' prim loc v -> InferOut prim loc v
inferApp ctx loc f a = {- trace (unlines ["APP", ppShow ctx, ppShow f, ppShow a]) $ -} do
  tvn <- fmap (varT loc) $ freshVar
  res <- inferTerms ctx [f, a]
  case res of
    (phi, [(tf, f'), (ta, a')]) -> liftEither $ fmap (\subst ->
                                                    let ty   = apply subst tvn
                                                        term = tyAppE ty loc (apply subst f') (apply subst a')
                                                    in  (subst, ty, term)) $ unify phi tf (arrowT loc ta tvn)
    _               -> error "Impossible has happened!"

inferLam :: (Eq loc, IsVar v, Show loc, IsPrim prim, loc ~ PrimLoc prim, v ~ PrimVar prim)
  => Context' loc v -> Origin loc -> v -> Term' prim loc v -> InferOut prim loc v
inferLam ctx loc x body = do
  tvn <- freshVar
  (phi, tbody, bodyTyTerm) <- infer (ctx1 tvn) body
  let ty = arrowT loc (apply phi (varT loc tvn)) tbody
  return (phi, ty, tyLamE ty loc x bodyTyTerm)
  where
    ctx1 tvn = insertCtx x (newVar loc tvn) ctx

inferLet :: (Eq loc, IsVar v, Show loc, IsPrim prim, loc ~ PrimLoc prim, v ~ PrimVar prim)
  => Context' loc v
  -> Origin loc
  -> [Bind' loc v (Term' prim loc v)]
  -> Term' prim loc v
  -> InferOut prim loc v
inferLet ctx loc vs body = do
  (phi, rhsTyTerms) <- inferTerms ctx $ fmap bind'rhs vs
  let (tBinds, termBinds) = unzip rhsTyTerms
  ctx1 <- addDecls (zipWith (\a t -> fmap (const t) a) vs tBinds) (apply phi ctx)
  (subst, tbody, bodyTerm) <- infer ctx1 body
  let tyBinds = zipWith (\bind rhs -> bind { bind'rhs = rhs }) vs termBinds
  return (subst <> phi, tbody, tyLetE tbody loc tyBinds bodyTerm)

inferLetRec :: forall prim loc v . (Eq loc, IsVar v, Show loc, IsPrim prim, loc ~ PrimLoc prim, v ~ PrimVar prim)
  => Context' loc v
  -> Origin loc
  -> [Bind' loc v (Term' prim loc v)]
  -> Term' prim loc v
  -> InferOut prim loc v
inferLetRec ctx topLoc vs body = do
  lhsCtx <- getTypesLhs vs
  (phi, rhsTyTerms) <- inferTerms (ctx <> Context (M.fromList lhsCtx)) exprBinds
  let (tBinds, bindsTyTerms) = unzip rhsTyTerms
  (ctx1, lhsCtx1, subst) <- liftEither $ unifyRhs ctx lhsCtx phi tBinds
  inferBody bindsTyTerms ctx1 lhsCtx1 subst body
  where
    exprBinds = fmap bind'rhs vs
    locBinds  = fmap bind'loc vs

    getTypesLhs :: [Bind' loc v (Term' prim loc v)] -> InferM loc v [(v, Signature' loc v)]
    getTypesLhs lhs = mapM (\b -> fmap ((bind'lhs b, ) . newVar (bind'loc b)) freshVar) lhs

    unifyRhs context lhsCtx phi tBinds =
      fmap (\subst -> (context1, lhsCtx1, subst)) $ unifyl phi ts tBinds
      where
        context1 = apply phi context
        lhsCtx1  = fmap (second $ apply phi) lhsCtx
        ts = fmap (oldBvar . snd) lhsCtx1

    oldBvar = cata go . unSignature
      where
        go  = \case
          MonoT t       -> t
          ForAllT _ _ t -> t

    inferBody termBinds context lhsCtx subst expr = do
      ctx1 <- addDecls (zipWith (\loc (v, ty) -> Bind loc v ty) locBinds $ fmap (second $ oldBvar . apply subst) lhsCtx) $ apply subst context
      (phi, ty, bodyTerm) <- infer ctx1 expr
      let tyBinds = zipWith (\bind rhs -> bind { bind'rhs = rhs }) vs termBinds
      return (phi <> subst, ty, tyLetRecE ty topLoc tyBinds bodyTerm)

inferAssertType :: (Eq loc, IsVar v, Show loc, IsPrim prim, loc ~ PrimLoc prim, v ~ PrimVar prim)
  => Context' loc v
  -> Origin loc
  -> Term' prim loc v
  -> Type' loc v
  -> InferOut prim loc v
inferAssertType ctx loc a ty = do
  (phi, tA, aTyTerm) <- infer ctx a
  subst <- liftEither $ genSubtypeOf phi ty tA
  return (subst <> phi, ty, tyAssertTypeE loc aTyTerm ty)

inferConstr :: (Eq loc, IsVar v) => Origin loc -> Type' loc v -> v -> Int -> InferOut prim loc v
inferConstr loc ty tag arity = do
  vT <- newInstance $ typeToSignature ty
  return $  (mempty, vT, tyConstrE loc vT tag arity)

inferCase :: forall prim loc v .
     (Eq loc, IsVar v, Show loc, IsPrim prim, loc ~ PrimLoc prim, v ~ PrimVar prim)
  => Context' loc v
  -> Origin loc -> Term' prim loc v -> [CaseAlt' loc v (Term' prim loc v)]
  -> InferOut prim loc v
inferCase ctx loc e caseAlts = do
  (phi, tE, tyTermE) <- infer ctx e
  (psi, tRes, tyAlts) <- inferAlts phi tE caseAlts
  return (psi, tRes, apply psi $ tyCaseE tRes loc tyTermE $ fmap (applyAlt psi) tyAlts)
  where
    inferAlts :: Subst' loc v -> Type' loc v -> [CaseAlt' loc v (Term' prim loc v)] -> InferM loc v (Subst' loc v, Type' loc v, [CaseAlt' loc v (TyTerm' prim loc v)])
    inferAlts substE tE alts =
      fmap (\(subst, _, tRes, as) -> (subst, tRes, L.reverse as)) $ foldM go (substE, tE, tE, []) alts
      where
        go (subst, tyTop, _, res) alt = do
          (phi, tRes, alt') <- inferAlt alt
          subst' <- liftEither $ unify (subst <> phi) (apply phi tyTop) (apply phi $ caseAlt'constrType alt')
          return (subst', apply subst' tyTop, apply subst' tRes, applyAlt subst' alt' : res)


    inferAlt :: CaseAlt' loc v (Term' prim loc v) -> InferM loc v (Subst' loc v, Type' loc v, CaseAlt' loc v (TyTerm' prim loc v))
    inferAlt preAlt = do
      alt <- newCaseAltInstance preAlt
      let argVars = fmap  (\ty -> (snd $ typed'value ty, (fst $ typed'value ty, typed'type ty))) $ caseAlt'args alt
          ctx1 = Context (M.fromList $ fmap (second $ monoT . snd) argVars) <> ctx
      (subst, tRes, tyTermRhs) <- infer ctx1 $ caseAlt'rhs alt
      let args = fmap (\(v, (argLoc, tv)) -> Typed (apply subst tv) (argLoc, v)) argVars
          alt' = alt
                  { caseAlt'rhs = tyTermRhs
                  , caseAlt'args = args
                  , caseAlt'constrType = apply subst $ caseAlt'constrType alt
                  }
      return (subst, tRes, alt')

    newCaseAltInstance :: CaseAlt' loc v (Term' prim loc v) -> InferM loc v (CaseAlt' loc v (Term' prim loc v))
    newCaseAltInstance alt = do
      tv <- newInstance $ typeToSignature $ getCaseType alt
      let (argsT, resT)= splitFunT tv
      return $ alt
        { caseAlt'constrType = resT
        , caseAlt'args = zipWith (\aT ty -> ty { typed'type = aT }) argsT $ caseAlt'args alt
        }

    getCaseType :: CaseAlt' loc v (Term' prim loc v) -> Type' loc v
    getCaseType CaseAlt{..} = funT (fmap typed'type caseAlt'args) caseAlt'constrType

    splitFunT :: Type' loc v -> ([Type' loc v], Type' loc v)
    splitFunT arrT = go [] arrT
      where
        go argsT (Type (Fix t)) = case t of
          ArrowT _loc a b -> go (Type a : argsT) (Type b)
          other           -> (reverse argsT, Type $ Fix other)


    funT :: [Type' loc v] -> Type' loc v -> Type' loc v
    funT argsT resT = foldr (\a b -> arrowT (getLoc a) a b) resT argsT

    applyAlt subst alt@CaseAlt{..} = alt
      { caseAlt'constrType = apply subst caseAlt'constrType
      , caseAlt'args       = fmap applyTyped caseAlt'args
      , caseAlt'rhs        = apply subst caseAlt'rhs
      }
      where
        applyTyped ty@Typed{..} = ty { typed'type = apply subst $ typed'type }

inferBottom :: IsVar v => Origin loc -> InferOut prim loc v
inferBottom loc = do
  ty <- fmap (varT loc) freshVar
  return (mempty, ty, tyBottomE ty loc)

newInstance :: IsVar v => Signature' loc v -> InferM loc v (Type' loc v)
newInstance = fmap (uncurry apply) . cataM go . unSignature
  where
    go = \case
      MonoT ty -> return (mempty, ty)
      ForAllT loc v (Subst m, ty) -> fmap (\nv -> (Subst $ M.insert v (varT loc nv) m, ty)) freshVar

newVar :: IsVar v => Origin loc -> v -> Signature' loc v
newVar loc tvn = monoT $ varT loc tvn

freshVar :: IsVar v => InferM loc v v
freshVar = do
  n <- get
  put $ n + 1
  return $ intToVar n

inferTerms :: (Eq loc, IsVar v, Show loc, IsPrim prim, loc ~ PrimLoc prim, v ~ PrimVar prim)
  => Context' loc v
  -> [Term' prim loc v]
  -> InferM loc v (Subst' loc v, [(Type' loc v, TyTerm' prim loc v)])
inferTerms ctx ts = case ts of
  []   -> return $ (mempty, [])
  a:as -> do
    (phi, ta, termA)  <- infer ctx a
    (psi, tas) <- inferTerms (apply phi ctx) as
    return (psi <> phi, (apply psi ta, apply psi termA) : tas)

-- | Unification function. Checks weather two types unify.
-- First argument is current substitution.
unify :: (IsVar v, Show loc)
  => Subst' loc v
  -> Type' loc v
  -> Type' loc v
  -> Either (TypeError loc v) (Subst' loc v)
unify phi (Type (Fix x)) (Type (Fix y)) = {- trace (unlines ["UNIFY", ppShow tx, ppShow ty]) $ -}
  case (x, y) of
    (VarT loc tvn, t) ->
        let phiTvn = applyVar phi loc tvn
            phiT   = apply phi (Type (Fix t))
        in  if phiTvn `eqIgnoreLoc` varT loc tvn
              then extend phi loc tvn phiT
              else unify phi phiTvn phiT
    (a, VarT locB v) -> unify phi (varT locB v) (Type $ Fix a) -- (conT locA name $ fmap Type ts)
    (ConT locA n xs, ConT locB m ys) ->
      if n == m
        then unifyl phi (fmap Type xs) (fmap Type ys)
        else unifyErr locA locB
    (ArrowT _ a1 a2, ArrowT _ b1 b2) -> unifyl phi (fmap Type [a1, a2]) (fmap Type [b1, b2])
    (TupleT locA xs, TupleT locB ys) ->
      if length xs == length ys
        then unifyl phi (fmap Type xs) (fmap Type ys)
        else unifyErr locA locB
    (ListT _ a, ListT _ b) -> unify phi (Type a) (Type b)
    (a, b) -> unifyErr (getLoc $ Type $ Fix a) (getLoc $ Type $ Fix b)
  where
    unifyErr locA locB =
      Left $ UnifyErr (chooseUserOrigin locA locB)
                      (mapLoc fromOrigin $ Type (Fix x))
                      (mapLoc fromOrigin $ Type (Fix y))

eqIgnoreLoc :: Eq v => Type loc v -> Type loc v -> Bool
eqIgnoreLoc = (==) `on` mapLoc (const ())

applyVar :: IsVar v => Subst' loc v -> Origin loc -> v -> Type' loc v
applyVar (Subst subst) loc v = fromMaybe (varT loc v) $ M.lookup v subst

extend :: IsVar v => Subst' loc v -> Origin loc -> v -> Type' loc v -> Either (TypeError loc v) (Subst' loc v)
extend phi loc tvn ty
  | varT loc tvn `eqIgnoreLoc` ty = Right phi
  | memberVarSet tvn (tyVars ty)  = Left $ OccursErr (fromOrigin loc) (mapLoc fromOrigin ty)
  | otherwise                     = Right $ delta tvn ty <> phi

unifyl :: (IsVar v, Show loc)
  => Subst' loc v
  -> [Type' loc v]
  -> [Type' loc v]
  -> Either (TypeError loc v) (Subst' loc v)
unifyl subst as bs = foldr go (Right subst) $ zip as bs
  where
    go (a, b) eSubst = (\t -> unify t a b) =<< eSubst

-- | Checks if first argument one type is subtype of the second one.
subtypeOf :: (IsVar v, Show loc)
  => Type loc v -> Type loc v -> Either (TypeError loc v) (Subst loc v)
subtypeOf a b = fmap fromSubstOrigin $ genSubtypeOf mempty (mapLoc Proven a) (mapLoc UserCode b)

fromSubstOrigin :: Ord v => Subst' loc v -> Subst loc v
fromSubstOrigin = Subst . M.map (mapLoc fromOrigin) . unSubst

genSubtypeOf :: (IsVar v, Show loc)
  => Subst' loc v
  -> Type' loc v
  -> Type' loc v
  -> Either (TypeError loc v) (Subst' loc v)
genSubtypeOf phi tx@(Type (Fix x)) ty@(Type (Fix y)) = case (x, y) of
  (_, VarT _ _) -> unify phi tx ty
  (ConT locA n xs, ConT locB m ys) ->
    if n == m
      then subtypeOfL phi (fmap Type xs) (fmap Type ys)
      else subtypeErr locA locB
  (ArrowT _ a1 a2, ArrowT _ b1 b2) -> subtypeOfL phi (fmap Type [a1, a2]) (fmap Type [b1, b2])
  (TupleT locA as, TupleT locB bs) ->
    if length as == length bs
      then subtypeOfL phi (fmap Type as) (fmap Type bs)
      else subtypeErr locA locB
  (ListT _ a, ListT _ b) -> genSubtypeOf phi (Type a) (Type b)
  (VarT locA _, _) -> subtypeErr locA (getLoc ty)
  _ -> subtypeErr (getLoc tx) (getLoc ty)
  where
    subtypeErr locA locB =
      Left $ SubtypeErr (chooseUserOrigin locA locB) (mapLoc fromOrigin tx) (mapLoc fromOrigin ty)

subtypeOfL :: (IsVar v, Show loc)
  => Subst' loc v -> [Type' loc v] -> [Type' loc v] -> Either (TypeError loc v) (Subst' loc v)
subtypeOfL subst as bs = foldr go (Right subst) $ zip as bs
  where
    go (a, b) eSubst = (\t -> genSubtypeOf t a b) =<< eSubst

addDecls :: IsVar v
  => [Bind (Origin loc) v (Type' loc v)]
  -> Context' loc v
  -> InferM loc v (Context' loc v)
addDecls vs ctx =
  foldM  (\c b -> addDecl unknowns b c) ctx vs
  where
    unknowns = foldMap tyVars $ unContext ctx

addDecl :: forall loc v . IsVar v
  => VarSet' loc v
  -> Bind' loc v (Type' loc v)
  -> Context' loc v
  -> InferM loc v (Context' loc v)
addDecl unknowns b ctx = do
  scheme <- toScheme unknowns (bind'rhs b)
  return $ Context . M.insert (bind'lhs b) scheme . unContext $ ctx
  where
    toScheme :: VarSet' loc v -> Type' loc v -> InferM loc v (Signature' loc v)
    toScheme uVars ty = do
      (subst, newVars) <- fmap (\xs -> (toSubst xs, fmap (\((loc, _), v) -> (loc, v)) xs)) $
          mapM (\sv -> fmap ((sv, )) freshVar) $ varSetToList schematicVars
      return $ foldr (uncurry forAllT) (monoT (apply subst ty)) newVars
      where
        schematicVars = tyVars ty `differenceVarSet` uVars

    toSubst = Subst . M.fromList . fmap (\((loc, v), a) -> (v, varT loc a))

-------------------------------------------------------
-- pretty letters for variables in the result type

-- | Converts variable names to human-readable format.
normaliseType :: (IsVar v, Eq loc) => Type loc v -> Type loc v
normaliseType ty = apply (normaliseSubst ty) ty

normaliseSubst :: (HasTypeVars m, Eq loc, IsVar v) => m loc v -> Subst loc v
normaliseSubst x =
  Subst $ M.fromList $
    zipWith (\(nameA, loc) nameB -> (nameA, varT loc nameB)) (tyVarsInOrder x) prettyLetters

------------------------------------------------
--

-- | Checks weather two types unify. If they do it returns substitution that unifies them.
unifyTypes :: (Show loc, IsVar v) => Type loc v -> Type loc v -> Either (TypeError loc v) (Subst loc v)
unifyTypes a b = fmap fromSubstOrigin $ unify mempty (mapLoc Proven a) (mapLoc UserCode b)


