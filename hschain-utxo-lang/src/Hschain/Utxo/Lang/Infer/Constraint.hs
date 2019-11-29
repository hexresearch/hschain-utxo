module Hschain.Utxo.Lang.Infer.Constraint (
    Constraint
  , Unifier
  , Solve
  , runSolve
) where

import Control.Monad.Except
import Control.Monad.Identity

import Data.Fix

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Infer.Subst

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Constraint = (Type, Type, Lang)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubst, cs)

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2, expr): cs0) -> do
      su1  <- unifies expr t1 t2
      solver (su1 `compose` su, apply su1 cs0)

unifyMany :: Lang -> [Type] -> [Type] -> Solve Subst
unifyMany expr [] [] = return emptySubst
unifyMany expr (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies expr t1 t2
  su2 <- unifyMany expr (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany expr t1 t2 = throwError $ UnificationMismatch t1 t2

unifies ::  Lang -> Type -> Type -> Solve Subst
unifies expr (Fix ta) (Fix tb) = case (ta, tb) of
  (VarType a, t) -> bind a (Fix t)
  (t, VarType a) -> bind a (Fix t)
  (FunctionType a1 b1, FunctionType a2 b2) -> fromFunction (a1, b1) (a2, b2)
  (UknownType, UknownType) -> return emptySubst
  (BoolType, BoolType) -> return emptySubst
  (IntType, IntType) -> return emptySubst
  (MoneyType, MoneyType) -> return emptySubst
  (DoubleType, DoubleType) -> return emptySubst
  (StringType, StringType) -> return emptySubst
  (BoxType, BoxType) -> return emptySubst
  (VectorType a, VectorType b) -> unifies expr a b
  (TupleType as, TupleType bs) -> unifyMany expr as bs
  _              -> throwError $ UnificationFail (Fix ta) (Fix tb) expr
  where
    fromFunction (a1, b1) (a2, b2) = unifyMany expr [a1, b1] [a2, b2]

bind ::  TypeVar -> Type -> Solve Subst
bind a t
  | equalsToVar     = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ Subst $ M.singleton a t
  where
    equalsToVar = t == (Fix $ VarType a)

occursCheck ::  Substitutable a => TypeVar -> a -> Bool
occursCheck a t = S.member a (ftv t)

instance Substitutable Constraint where
   apply s (t1, t2, expr) = (apply s t1, apply s t2, expr)
   ftv (t1, t2, _) = ftv t1 `S.union` ftv t2

