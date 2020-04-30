module Language.HM.Infer(
    Context(..)
  , inferType
  , subtypeOf
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

import qualified Data.Map.Strict as M

type Context' loc v = Context (Origin loc) v
type Type' loc v = Type (Origin loc) v
type Term' loc v = Term (Origin loc) v
type Signature' loc v = Signature (Origin loc) v
type Subst' loc v = Subst (Origin loc) v
type Bind' loc v a = Bind (Origin loc) v a
type VarSet' loc v = VarSet (Origin loc) v

newtype Context loc v = Context { unContext :: Map v (Signature loc v) }
  deriving (Show, Eq, Semigroup, Monoid)

instance CanApply Context where
    apply subst = Context . fmap (apply subst) . unContext


markProven :: Ord v => Context loc v -> Context (Origin loc) v
markProven = Context . M.map (mapLoc Proven) . unContext

markUserCode :: Term loc v -> Term (Origin loc) v
markUserCode = mapLoc UserCode

chooseUserOrigin :: Origin a -> Origin a -> a
chooseUserOrigin x y = case (x, y) of
  (UserCode a, _) -> a
  (_, UserCode a) -> a
  _               -> fromOrigin x

data Origin a
  = Proven a
  | UserCode a
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

newtype InferM loc var a = InferM (StateT Int (Except (TypeError loc var)) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadError (TypeError loc var))

runInferM :: InferM loc var a -> Either (TypeError loc var) a
runInferM (InferM m) = runExcept $ evalStateT m 0

inferType :: (IsVar var, Eq loc) => Context loc var -> Term loc var -> Either (TypeError loc var) (Type loc var)
inferType ctx term =
  fmap (normaliseType . mapLoc fromOrigin . snd) $ runInferM $ infer (markProven ctx) (markUserCode term)

type Out loc var = (Subst (Origin loc) var, Type (Origin loc) var)
type InferOut loc var = InferM loc var (Out loc var)

infer :: IsVar var => Context' loc var -> Term' loc var -> InferOut loc var
infer ctx (Term (Fix x)) = case x of
  Var loc v           -> inferVar ctx loc v
  App loc a b         -> inferApp ctx loc (Term a) (Term b)
  Lam loc v r         -> inferLam ctx loc v (Term r)
  Let loc vs a        -> inferLet ctx loc (fmap (fmap Term) vs) (Term a)
  LetRec loc vs a     -> inferLetRec ctx loc (fmap (fmap Term) vs) (Term a)
  AssertType loc a ty -> inferAssertType ctx loc (Term a) ty

inferVar :: IsVar v => Context (Origin loc) v -> Origin loc -> v -> InferOut loc v
inferVar ctx loc v = fmap (mempty, ) $ maybe err newInstance $ M.lookup v (unContext ctx)
  where
    err = throwError $ NotInScopeErr (fromOrigin loc) v

inferApp :: IsVar v => Context' loc v -> Origin loc -> Term' loc v -> Term' loc v -> InferOut loc v
inferApp ctx loc f a = do
  tvn <- fmap (varT loc) $ freshVar
  res <- inferTerms ctx [f, a]
  case res of
    (phi, [tf, ta]) -> liftEither $ fmap (\subst -> (subst, apply subst tvn)) $ unify phi tf (arrowT loc ta tvn)
    _               -> error "Impossible has happened!"

inferLam :: IsVar v => Context' loc v -> Origin loc -> v -> Term' loc v -> InferOut loc v
inferLam ctx loc x body = do
  tvn <- freshVar
  (phi, tbody) <- infer (ctx1 tvn) body
  return (phi, arrowT loc (apply phi (varT loc tvn)) tbody)
  where
    ctx1 tvn = Context . M.insert x (newVar loc tvn) . unContext $ ctx

inferLet :: IsVar v
  => Context' loc v
  -> Origin loc
  -> [Bind' loc v (Term' loc v)]
  -> Term' loc v
  -> InferOut loc v
inferLet ctx _ vs body = do
  (phi, tTerms) <- inferTerms ctx $ fmap bind'rhs vs
  ctx1 <- addDecls (zipWith (\a t -> fmap (const t) a) vs tTerms) (apply phi ctx)
  (subst, tbody) <- infer ctx1 body
  return (subst <> phi, tbody)

inferLetRec :: forall loc v . IsVar v
  => Context' loc v
  -> Origin loc
  -> [Bind' loc v (Term' loc v)]
  -> Term' loc v
  -> InferOut loc v
inferLetRec ctx _ vs body = do
  lhsCtx <- getTypesLhs vs
  (phi, tBinds) <- inferTerms (ctx <> Context (M.fromList lhsCtx)) exprBinds
  (ctx1, lhsCtx1, subst) <- liftEither $ unifyRhs ctx lhsCtx phi tBinds
  inferBody ctx1 lhsCtx1 subst body
  where
    exprBinds = fmap bind'rhs vs
    locBinds  = fmap bind'loc vs

    getTypesLhs :: [Bind' loc v (Term' loc v)] -> InferM loc v [(v, Signature' loc v)]
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

    inferBody context lhsCtx subst expr = do
      ctx1 <- addDecls (zipWith (\loc (v, ty) -> Bind loc v ty) locBinds $ fmap (second $ oldBvar . apply subst) lhsCtx) $ apply subst context
      (phi, ty) <- infer ctx1 expr
      return (phi <> subst, ty)

inferAssertType :: IsVar v
  => Context' loc v
  -> Origin loc
  -> Term' loc v
  -> Type' loc v
  -> InferOut loc v
inferAssertType ctx _ a ty = do
  (phi, tA) <- infer ctx a
  subst <- liftEither $ genSubtypeOf phi ty tA
  return (subst <> phi, ty)

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

inferTerms :: IsVar v
  => Context' loc v
  -> [Term' loc v]
  -> InferM loc v (Subst' loc v, [Type' loc v])
inferTerms ctx ts = case ts of
  []   -> return $ (mempty, [])
  a:as -> do
    (phi, ta)  <- infer ctx a
    (psi, tas) <- inferTerms (apply phi ctx) as
    return (psi <> phi, apply psi ta : tas)

unify :: IsVar v
  => Subst' loc v
  -> Type' loc v
  -> Type' loc v
  -> Either (TypeError loc v) (Subst' loc v)
unify phi (Type (Fix x)) (Type (Fix y)) = case (x, y) of
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

unifyl :: IsVar v
  => Subst' loc v
  -> [Type' loc v]
  -> [Type' loc v]
  -> Either (TypeError loc v) (Subst' loc v)
unifyl subst as bs = foldr go (Right subst) $ zip as bs
  where
    go (a, b) eSubst = (\t -> unify t a b) =<< eSubst

subtypeOf :: IsVar v => Type loc v -> Type loc v -> Either (TypeError loc v) (Subst loc v)
subtypeOf a b = fmap fromSubstOrigin $ genSubtypeOf mempty (mapLoc Proven a) (mapLoc UserCode b)

fromSubstOrigin :: Ord v => Subst' loc v -> Subst loc v
fromSubstOrigin = Subst . M.map (mapLoc fromOrigin) . unSubst

genSubtypeOf :: IsVar v
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

subtypeOfL :: IsVar v => Subst' loc v -> [Type' loc v] -> [Type' loc v] -> Either (TypeError loc v) (Subst' loc v)
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

normaliseType :: (IsVar v, Eq loc) => Type loc v -> Type loc v
normaliseType ty = apply (normaliseSubst ty) ty

normaliseSubst :: (HasTypeVars m, Eq loc, IsVar v) => m loc v -> Subst loc v
normaliseSubst x =
  Subst $ M.fromList $
    zipWith (\(nameA, loc) nameB -> (nameA, varT loc nameB)) (tyVarsInOrder x) prettyLetters

