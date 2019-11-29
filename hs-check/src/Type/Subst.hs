module Type.Subst where

import Control.Monad.Except

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Maybe

import Type.Type

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T

newtype Subst = Subst { unSubst :: Map Tyvar Type }
  deriving (Show, Eq)

nullSubst :: Subst
nullSubst = Subst M.empty

compose :: Subst -> Subst -> Subst
compose subst2@(Subst s2) (Subst s1) = Subst $ M.union s1' s2'
  where
    s1' = fmap (apply subst2) s1
    s2' = M.difference s2 s1

merge :: MonadError TypeError m => Subst -> Subst -> m Subst
merge (Subst s1) (Subst s2)
  | agree     = return $ Subst $ M.union s1 s2
  | otherwise = throwError $ TypeError "merge fails"
  where
    agree = and $ M.intersectionWith (==) s1 s2

singleton :: Tyvar -> Type -> Subst
singleton v t = Subst $ M.fromList [(v, t)]

class Types t  where
  apply   :: Subst -> t -> t
  getVars :: t -> Set Tyvar

instance Types Type where
  apply subst@(Subst s) = \case

    TVar u  -> fromMaybe (TVar u) $ M.lookup u s
    TAp a b -> TAp (rec a) (rec b)
    t       -> t
    where
      rec = apply subst

  getVars = \case
    TVar u  -> S.singleton u
    TAp a b -> mappend (getVars a) (getVars b)
    _       -> mempty

instance Types a => Types [a] where
  apply s = map (apply s)
  getVars = foldMap getVars

instance (Ord a, Types a) => Types (Set a) where
  apply s = S.map (apply s)
  getVars = foldMap getVars

instance Types t => Types (Qual t) where
  apply s (Qual ps t) = Qual (apply s ps) (apply s t)
  getVars (Qual ps t) = getVars ps <> getVars t

instance Types Pred where
  apply s (IsIn name t) = IsIn name (apply s t)
  getVars (IsIn _ t) = getVars t

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks $ apply s qt
  getVars (Forall _ qt) = getVars qt

instance Types Assump where
  apply s (idx :>: t) = idx :>: apply s t
  getVars (_ :>: t) = getVars t

mostGeneralUnifier :: MonadError TypeError m => Type -> Type -> m Subst
mostGeneralUnifier x y = case (x, y) of
  (TAp a1 b1, TAp a2 b2) -> do
    s1 <- mostGeneralUnifier a1 a2
    s2 <- mostGeneralUnifier b1 b2
    return $ compose s2 s1
  (TVar u, t) -> varBind u t
  (t, TVar u) -> varBind u t
  (TCon c1, TCon c2) | c1 == c2 -> return nullSubst
  _  -> throwError $ TypeError "Types do not unify"

varBind :: MonadError TypeError m => Tyvar -> Type -> m Subst
varBind u t
  | t == TVar u             = return $ nullSubst
  | u `S.member` getVars t  = throwError $ TypeError "occurs check fails"
  | kind u /= kind t        = throwError $ TypeError "kinds do not match"
  | otherwise               = return $ singleton u t


match :: MonadError TypeError m => Type -> Type -> m Subst
match x y = case (x, y) of
  (TAp a1 b1, TAp a2 b2) -> do
    s1 <- mostGeneralUnifier a1 a2
    s2 <- mostGeneralUnifier b1 b2
    merge s2 s1
  (TVar u, t) | kind u == kind t -> return $ singleton u t
  (TCon c1, TCon c2) | c1 == c2 -> return nullSubst
  _  -> throwError $ TypeError "Types do not match"

mostGeneralUnifierPred :: MonadError TypeError m => Pred -> Pred -> m Subst
mostGeneralUnifierPred = liftPred mostGeneralUnifier

matchPred :: MonadError TypeError m => Pred -> Pred -> m Subst
matchPred = liftPred match

liftPred :: MonadError TypeError m => (Type -> Type -> m Subst) -> Pred -> Pred -> m Subst
liftPred m (IsIn n1 t1) (IsIn n2 t2)
  | n1 == n2   = m t1 t2
  | otherwise  = throwError $ TypeError "classes differ"


quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where
    vs' = filter (`elem` vs) $ S.toList $ getVars qt
    ks  = map kind vs'
    s   = Subst $ M.fromList $ zip vs' $ fmap TGen [0..]

toScheme :: Type -> Scheme
toScheme ty = Forall [] (Qual [] ty)

findAssump :: MonadError TypeError m => Id -> [Assump] -> m Scheme
findAssump idx as =
  maybe err (pure . (\(_ :>: sc) -> sc)) $ L.find (\(idx' :>: _) -> (idx == idx')) as
  where
    err = throwError $ TypeError $ mconcat ["unbound identifier ", idx]




