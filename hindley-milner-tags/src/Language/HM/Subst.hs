-- | Capture-avoiding substitutions.
module Language.HM.Subst(
    CanApply(..)
  , Subst(..)
  , delta
) where

import Data.Fix
import qualified Data.Map.Strict as M

import Language.HM.Type

-- | Substitutions of type variables for monomorphic types.
newtype Subst v = Subst { unSubst :: M.Map v (Type v) }

instance Ord v => Semigroup (Subst v) where
  Subst s1 <> Subst s2 =
    Subst $ M.map (apply (Subst s1)) s2 `M.union` s1

instance Ord v => Monoid (Subst v) where
  mempty = Subst M.empty

delta :: IsVar v => v -> Type v -> Subst v
delta v ty = Subst $ M.singleton v ty

---------------------------------------------------------------

class CanApply f where
  apply :: Ord v => Subst v -> f v -> f v

instance CanApply Type where
  apply (Subst s) = cata go . unType
    where
      go = \case
        VarT v -> case M.lookup v s of
          Nothing -> varT v
          Just t  -> apply (Subst s) t
        ConT name args -> conT name args

instance CanApply Signature where
  apply (Subst s) (Signature (Fix expr)) = case expr of
    MonoT t     -> monoT $ apply (Subst s) t
    ForAllT x t -> forAllT x $ apply (Subst $ M.delete x s) (Signature t)

