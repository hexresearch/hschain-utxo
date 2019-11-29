module Hschain.Utxo.Lang.Infer.Subst (
    Subst(..)
  , Substitutable(..)
  , emptySubst
  , compose
) where

import Control.Monad

import Data.Fix
import Data.Map.Strict (Map)
import Data.Set (Set)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Infer.TypeEnv

import qualified Data.Map.Strict as M
import qualified Data.Set as S

newtype Subst = Subst { unSubst :: Map TypeVar Type }
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set TypeVar

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
compose s1 s2 = Subst $ M.map (apply s1) (unSubst s2) `M.union` (unSubst s1)

------------------------------
-- instances

instance Substitutable Type where
  ftv tyExpr = S.fromList $ cata fun tyExpr
    where
      fun :: TypeExpr [TypeVar] -> [TypeVar]
      fun = \case
        VarType tv -> [tv]
        UknownType -> []
        BoolType   -> []
        IntType    -> []
        MoneyType  -> []
        DoubleType -> []
        StringType -> []
        BoxType    -> []
        ScriptType -> []
        VectorType a     -> a
        TupleType xs     -> join xs
        FunctionType a b -> mconcat [a, b]

  apply (Subst subst) expr = cata fun expr
    where
      fun :: TypeExpr Type -> Type
      fun x = case x of
        VarType a -> M.findWithDefault (Fix x) a subst
        _         -> Fix x

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ M.map (apply s) env
  ftv (TypeEnv env) = ftv $ M.elems env

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = Subst $ foldr M.delete (unSubst s) as
  ftv (Forall as t) = ftv t `S.difference` S.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (S.union . ftv) S.empty

