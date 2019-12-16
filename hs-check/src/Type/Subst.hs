module Type.Subst where

import Control.Monad.Except

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Maybe

import Type.Loc
import Type.Type
import Type.Pretty

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T

nullSubst :: Subst
nullSubst = Subst M.empty

compose :: Subst -> Subst -> Subst
compose subst2@(Subst s2) (Subst s1) = Subst $ M.map (apply (Subst s1)) s2 `M.union` s1
{-
Subst $ M.union s1' s2'
  where
    s1' = fmap (apply subst2) s1
    s2' = M.difference s2 s1
-}
merge :: Subst -> Subst -> Maybe Subst
merge (Subst s1) (Subst s2)
  | agree     = Just $ Subst $ M.union s1 s2
  | otherwise = Nothing
  where
    agree = and $ M.intersectionWith (==) s1 s2

singleton :: Tyvar -> Type -> Subst
singleton v t = Subst $ M.fromList [(v, t)]

class Types t  where
  apply   :: Subst -> t -> t
  getVars :: t -> Set Tyvar

instance Types Type where
  apply subst@(Subst s) = \case

    TVar loc u  -> fromMaybe (TVar loc u) $ M.lookup u s
    TAp loc a b -> TAp loc (rec a) (rec b)
    TFun loc a b -> TFun loc (rec a) (rec b)
    TTuple loc as -> TTuple loc (fmap rec as)
    t       -> t
    where
      rec = apply subst

  getVars = \case
    TVar _ u  -> S.singleton u
    TAp _ a b -> mappend (getVars a) (getVars b)
    TFun _ a b -> mappend (getVars a) (getVars b)
    TTuple _ as -> foldMap getVars as
    _       -> mempty

instance Types a => Types [a] where
  apply s = map (apply s)
  getVars = foldMap getVars

instance (Ord a, Types a) => Types (Set a) where
  apply s = S.map (apply s)
  getVars = foldMap getVars

instance Types t => Types (Qual t) where
  apply s (Qual loc ps t) = Qual loc (apply s ps) (apply s t)
  getVars (Qual _ ps t) = getVars ps <> getVars t

instance Types Pred where
  apply s (IsIn loc name t) = IsIn loc name (apply s t)
  getVars (IsIn _ _ t) = getVars t

instance Types Scheme where
  apply s (Forall loc ks qt) = Forall loc ks $ apply s qt
  getVars (Forall _ _ qt) = getVars qt

instance Types Assump where
  apply s (idx :>: t) = idx :>: apply s t
  getVars (_ :>: t) = getVars t

mostGeneralUnifier :: MonadError TypeError m => Type -> Type -> m Subst
mostGeneralUnifier x y = case (x, y) of
  (TAp _ a1 b1, TAp _ a2 b2) -> uniPair (a1, a2) (b1, b2)
  (TVar _ u, t) -> varBind u t
  (t, TVar _ u) -> varBind u t
  (TFun _ a1 b1, TFun _ a2 b2) -> uniPair (a1, a2) (b1, b2)
  (TTuple _ as, TTuple _ bs) -> uniList as bs
  (TCon _ c1, TCon _ c2) | c1 == c2 -> return nullSubst
  _  -> err x
  where
    uniPair (a1, a2) (b1, b2) = do
      s1 <- mostGeneralUnifier a1 a2
      s2 <- mostGeneralUnifier b1 b2
      return $ compose s2 s1

    uniList xs ys = case (xs, ys) of
      ([], []) -> return nullSubst
      (a:as, b:bs) -> do
        s1 <- mostGeneralUnifier a b
        s2 <- uniList as bs
        return $ compose s2 s1
      (a:_, []) -> err a
      ([], b:_) -> err b

    err a = throwError $ singleTypeError (getLoc a) "Types do not unify"

varBind :: MonadError TypeError m => Tyvar -> Type -> m Subst
varBind u t
  | t == TVar noLoc u       = return $ nullSubst
  | u `S.member` getVars t  = throwError $ singleTypeError (getLoc u) "occurs check fails"
  -- | kind u /= kind t        = throwError $ singleTypeError (getLoc u) $ mconcat ["kinds do not match: got ", pp $ kind u, " expected ", pp $ kind t]
  | otherwise               = return $ singleton u t
--   where
    -- pp = renderStrict . layoutPretty defaultLayoutOptions . pretty


match :: MonadError TypeError m => Type -> Type -> m Subst
match x y = case (x, y) of
  (TAp _ a1 b1, TAp _ a2 b2) -> uniPair (a1, a2) (b1, b2)
  (TFun _ a1 b1, TFun _ a2 b2) -> uniPair (a1, a2) (b1, b2)
  (TTuple _ as, TTuple _ bs) -> uniList as bs
  (TVar _ u, t) | kind u == kind t -> return $ singleton u t
  (TCon _ c1, TCon _ c2) | c1 == c2 -> return nullSubst
  _  -> err x
  where
    uniPair (a1, a2) (b1, b2) = do
      s1 <- mostGeneralUnifier a1 a2
      s2 <- mostGeneralUnifier b1 b2
      case merge s2 s1 of
        Just res -> return res
        Nothing  -> err x

    uniList xs ys = case (xs, ys) of
      ([], []) -> return nullSubst
      (a:as, b:bs) -> do
        s1 <- mostGeneralUnifier a b
        s2 <- uniList as bs
        case merge s2 s1 of
          Just res -> return res
          Nothing  -> err x
      (a:_, []) -> err a
      ([], b:_) -> err b

    err a = throwError $ singleTypeError (getLoc a) "Types do not match"

mostGeneralUnifierPred :: MonadError TypeError m => Pred -> Pred -> m Subst
mostGeneralUnifierPred = liftPred mostGeneralUnifier

matchPred :: MonadError TypeError m => Pred -> Pred -> m Subst
matchPred = liftPred match

liftPred :: MonadError TypeError m => (Type -> Type -> m Subst) -> Pred -> Pred -> m Subst
liftPred m (IsIn loc n1 t1) (IsIn _ n2 t2)
  | n1 == n2   = m t1 t2
  | otherwise  = throwError $ singleTypeError loc "classes differ"


quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall (getLoc qt) ks (apply s qt)
  where
    vs' = filter (`elem` vs) $ S.toList $ getVars qt
    ks  = map kind vs'
    s   = Subst $ M.fromList $ zip vs' $ fmap (TGen noLoc) [0..]

toScheme :: Type -> Scheme
toScheme ty = Forall (getLoc ty) [] (Qual (getLoc ty) [] ty)

findAssump :: MonadError TypeError m => Id -> [Assump] -> m Scheme
findAssump idx as =
  maybe err (pure . (\(_ :>: sc) -> sc)) $ L.find (\(idx' :>: _) -> (idx == idx')) as
  where
    err = throwError $ singleTypeError (getLoc idx) $ mconcat ["unbound identifier ", id'name idx]



