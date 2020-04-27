module Language.HM.AlgorithmW(
    Context(..)
  , inferType
  , subtypeOf
) where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Coerce
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe

import Language.HM.Term
import Language.HM.Subst
import Language.HM.Type
import Language.HM.TypeError


import qualified Data.Map.Strict as M
import qualified Data.Set as S

newtype Context v = Context { unContext :: Map v (Signature v)
  } deriving (Show, Eq, Semigroup, Monoid)

instance CanApply Context where
    apply subst = Context . fmap (apply subst) . unContext

data InferM v a where
  InferM :: IsVar v => StateT Int (Except (TypeError v)) a -> InferM v a

unInferM :: IsVar v => InferM v a -> StateT Int (Except (TypeError v)) a
unInferM (InferM m) = m

instance IsVar v => Functor (InferM v) where
  fmap f (InferM a) = InferM (fmap f a)

instance IsVar v => Applicative (InferM v) where
  pure = InferM . pure
  InferM f <*> InferM a = InferM $ f <*> a

instance IsVar v => Monad (InferM v) where
  InferM a >>= f = InferM $ a >>= (unInferM . f)

instance (IsVar v) => MonadError (TypeError v) (InferM v) where
  throwError = InferM . throwError
  catchError (InferM a) f = InferM $ catchError a (unInferM . f)

instance IsVar v => MonadState Int (InferM v) where
  get = InferM get
  put = InferM . put
  state = InferM . state

runInferM :: IsVar v => InferM v a -> Either (TypeError v) a
runInferM (InferM m) = runExcept $ evalStateT m 0

inferType :: IsVar v => Context v -> Term v -> Either (TypeError v) (Type v)
inferType ctx term = fmap snd $ runInferM $ infer ctx term

infer :: IsVar v => Context v -> Term v -> InferM v (Subst v, Type v)
infer ctx (Term (Fix x)) = case x of
  Var v           -> inferVar ctx v
  App a b         -> inferApp ctx (Term a) (Term b)
  Lam v r         -> inferLam ctx v (Term r)
  Let vs a        -> inferLet ctx (fmap (second Term) vs) (Term a)
  LetRec vs a     -> inferLetRec ctx (fmap (second Term) vs) (Term a)
  AssertType a ty -> inferAssertType ctx (Term a) ty

inferVar :: IsVar v => Context v -> v -> InferM v (Subst v, Type v)
inferVar ctx v = fmap (mempty, ) $ maybe err newInstance $ M.lookup v (unContext ctx)
  where
    err = throwError $ NotInScopeErr (getLoc v) v

inferApp :: IsVar v => Context v -> Term v -> Term v -> InferM v (Subst v, Type v)
inferApp ctx f a = do
  tvn <- fmap varT freshVar
  res <- inferTerms ctx [f, a]
  case res of
    (phi, [tf, ta]) -> liftEither $ fmap (\subst -> (subst, apply subst tvn)) $ unify phi tf (arrowT ta tvn)
    _               -> error "Impossible has happened!"

inferLam :: IsVar v => Context v -> v -> Term v -> InferM v (Subst v, Type v)
inferLam ctx x body = do
  tvn <- freshVar
  (phi, tbody) <- infer (ctx1 tvn) body
  return (phi, arrowT (apply phi (varT tvn)) tbody)
  where
    ctx1 tvn = coerce (M.insert x (newVar tvn)) ctx

inferLet :: IsVar v => Context v -> [(v, Term v)] -> Term v -> InferM v (Subst v, Type v)
inferLet ctx vs body = do
  (phi, tTerms) <- inferTerms ctx $ fmap snd vs
  ctx1 <- addDecls (zip (fmap fst vs) tTerms) (apply phi ctx)
  (subst, tbody) <- infer ctx1 body
  return (subst <> phi, tbody)

addDecls :: IsVar v => [(v, Type v)] -> Context v -> InferM v (Context v)
addDecls vs ctx =
  foldM  (\c (v, t) -> addDecl unknowns v t c) ctx vs
  where
    unknowns = foldMap tyVars $ unContext ctx

addDecl :: IsVar v => S.Set v -> v -> Type v -> Context v -> InferM v (Context v)
addDecl unknowns v t ctx = do
  scheme <- toScheme unknowns t
  return $ coerce (M.insert v scheme) ctx
  where
    toScheme uVars ty = do
      (subst, newVars) <- fmap (\xs -> (toSubst xs, fmap snd xs)) $
          mapM (\sv -> fmap ((sv, )) freshVar) $ S.toList schematicVars
      return $ foldr forAllT (monoT (apply subst ty)) newVars
      where
        schematicVars = tyVars ty `S.difference` uVars

    toSubst = Subst . M.fromList . fmap (second varT)

inferLetRec :: forall v . IsVar v
  => Context v -> [(v, Term v)] -> Term v -> InferM v (Subst v, Type v)
inferLetRec ctx vs body = do
  lhsCtx <- getTypesLhs nameBinds
  (phi, tBinds) <- inferTerms (ctx <> Context (M.fromList lhsCtx)) exprBinds
  (ctx1, lhsCtx1, subst) <- liftEither $ unifyRhs ctx lhsCtx phi tBinds
  inferBody ctx1 lhsCtx1 subst body
  where
    nameBinds = fmap fst vs
    exprBinds = fmap snd vs

    getTypesLhs :: [v] -> InferM v [(v, Signature v)]
    getTypesLhs lhs = mapM (\n -> fmap ((n, ) . newVar) freshVar) lhs

    unifyRhs context lhsCtx phi tBinds =
      fmap (\subst -> (context1, lhsCtx1, subst)) $ unifyl phi ts tBinds
      where
        context1 = apply phi context
        lhsCtx1  = fmap (second $ apply phi) lhsCtx
        ts = fmap (oldBvar . snd) lhsCtx1

    oldBvar = cata go . unSignature
      where
        go  = \case
          MonoT t     -> t
          ForAllT _ t -> t

    inferBody context lhsCtx subst expr = do
      ctx1 <- addDecls (fmap (second $ oldBvar . apply subst) lhsCtx) $ apply subst context
      (phi, ty) <- infer ctx1 expr
      return (phi <> subst, ty)

inferAssertType :: IsVar v => Context v -> Term v -> Type v -> InferM v (Subst v, Type v)
inferAssertType ctx a ty = do
  (phi, tA) <- infer ctx a
  subst <- liftEither $ genSubtypeOf phi ty tA
  return (subst <> phi, ty)


inferTerms :: IsVar v => Context v -> [Term v] -> InferM v (Subst v, [Type v])
inferTerms ctx ts = case ts of
  []   -> return $ (mempty, [])
  a:as -> do
    (phi, ta)  <- infer ctx a
    (psi, tas) <- inferTerms (apply phi ctx) as
    return (psi <> phi, apply psi ta : tas)

newInstance :: IsVar v => Signature v -> InferM v (Type v)
newInstance = fmap (uncurry apply) . cataM go . unSignature
  where
    go = \case
      MonoT ty -> return (mempty, ty)
      ForAllT v (Subst m, ty) -> fmap (\nv -> (Subst $ M.insert v (varT nv) m, ty)) freshVar

newVar :: IsVar v => v -> Signature v
newVar tvn = monoT $ varT tvn

freshVar :: IsVar v => InferM v v
freshVar = do
  n <- get
  put $ n + 1
  return $ intToVar n

unify :: IsVar v => Subst v -> Type v -> Type v -> Either (TypeError v) (Subst v)
unify phi (Type (Fix x)) (Type (Fix y)) = case (x, y) of
  (VarT tvn, t) ->
      let phiTvn = applyVar phi tvn
          phiT   = apply phi (Type (Fix t))
      in  if phiTvn == varT tvn
            then extend phi tvn phiT
            else unify phi phiTvn phiT
  (ConT name ts, VarT v) -> unify phi (varT v) (conT name $ fmap Type ts)
  (ConT n xs, ConT m ys) ->
    if n == m
      then unifyl phi (fmap Type xs) (fmap Type ys)
      else Left $ UnifyErr (getLoc n) (Type (Fix x)) (Type (Fix y)) -- todo: right source location!!!

unifyl :: IsVar v => Subst v -> [Type v] -> [Type v] -> Either (TypeError v) (Subst v)
unifyl subst as bs = foldr go (Right subst) $ zip as bs
  where
    go (a, b) eSubst = (\t -> unify t a b) =<< eSubst

applyVar :: IsVar v => Subst v -> v -> Type v
applyVar (Subst subst) v = fromMaybe (varT v) $ M.lookup v subst

extend :: IsVar v => Subst v -> v -> Type v -> Either (TypeError v) (Subst v)
extend phi tvn ty
  | varT tvn == ty           = Right phi
  | S.member tvn (tyVars ty) = Left $ OccursErr (getLoc tvn) tvn ty
  | otherwise                = Right $ delta tvn ty <> phi

subtypeOf :: IsVar v => Type v -> Type v -> Either (TypeError v) (Subst v)
subtypeOf a b = genSubtypeOf mempty a b

genSubtypeOf :: IsVar v => Subst v -> Type v -> Type v -> Either (TypeError v) (Subst v)
genSubtypeOf phi tx@(Type (Fix x)) ty@(Type (Fix y)) = case (x, y) of
  (_, VarT _) -> unify phi tx ty
  (ConT n xs, ConT m ys) ->
    if n == m
      then subtypeOfL phi (fmap Type xs) (fmap Type ys)
      else Left $ SubtypeErr (getLoc n) tx ty -- todo: right source location!!!
  (VarT v, ConT _ _) ->
      Left $ SubtypeErr (getLoc v) tx ty -- todo: right source location!!!

subtypeOfL :: IsVar v => Subst v -> [Type v] -> [Type v] -> Either (TypeError v) (Subst v)
subtypeOfL subst as bs = foldr go (Right subst) $ zip as bs
  where
    go (a, b) eSubst = (\t -> genSubtypeOf t a b) =<< eSubst


{-

inferLet :: IsVar v => Context v -> [(v, Term v)] -> Term v -> InferM (Subst v, Type v)

inferLetRec :: forall v . IsVar v => Context v -> [(v, Term)] -> Term v -> InferM (Subst v, Type v)

inferAssertType :: Context v -> Term v -> Type v -> InferM (Subst v, Type v)

--------------------------------------------------------------

unify :: IsVar v => Subst v -> Type v -> Type v -> Either (TypeError v) (Subst v)
unify phi (Fix x) (Fix y) = case (x, y) of
  (VarT tvn, t) ->
      let phiTvn = applyVar phi tvn
          phiT   = apply phi (Fix t)
      in  if phiTvn == varT tvn
            then extend phi tvn phiT
            else unify phi phiTvn phiT
  (ConT name ts, VarT v) -> unify phi (varT v) (conT name ts)
  (ConT n xs, ConT m ys) ->
    if n == m
      then unifyl phi xs ys
      else Left $ UnifyErr (Fix x) (Fix y)

unifyl :: IsVar v => Subst v -> [Type v] -> [Type v] -> Either (TypeError v) (Subst v)
unifyl subst as bs = foldr go (Right subst) $ zip as bs
  where
    go (a, b) eSubst = (\t -> unify t a b) =<< eSubst

applyVar :: IsVar v => Subst v -> v -> Type v
applyVar subst v = fromMaybe (varT v) $ M.lookup v subst

extend :: IsVar v => Subst v -> v -> Type v -> Either (TypeError v) (Subst v)
extend phi tvn ty
  | varT tvn == ty           = Right phi
  | S.member tvn (tyVars ty) = Left $ OccursErr tvn ty
  | otherwise                = Right $ delta tvn ty <@> phi

delta :: IsVar v => v -> Type v -> Subst v
delta v ty = M.fromList [(v, ty)]

subtypeOf :: IsVar v => Type v -> Type v -> Either (TypeError (Loc v)) (Subst v)
subtypeOf a b = genSubtypeOf mempty a b

genSubtypeOf :: IsVar v => Subst v -> Type v -> Type v -> Either (TypeError (Loc v)) (Subst v)
genSubtypeOf phi (Fix x) (Fix y) = case (x, y) of
  (_, VarT v) -> unify phi (Fix x) (Fix y)
  (ConT n xs, ConT m ys) ->
    if n == m
      then subtypeOfL phi xs ys
      else Left $ SubtypeErr (Fix x) (Fix y)

subtypeOfL :: IsVar v => Subst v -> [Type v] -> [Type v] -> Either (TypeError (Loc v)) (Subst v)
subtypeOfL subst as bs = foldr go (Right subst) $ zip as bs
  where
    go (a, b) eSubst = (\t -> genSubtypeOf t a b) =<< eSubst

-}
