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
newtype Subst loc v = Subst { unSubst :: M.Map v (Type loc v) }

instance Ord v => Semigroup (Subst loc v) where
  Subst s1 <> Subst s2 =
    Subst $ M.map (apply (Subst s1)) s2 `M.union` s1

instance Ord v => Monoid (Subst loc v) where
  mempty = Subst M.empty

delta :: IsVar v => v -> Type loc v -> Subst loc v
delta v ty = Subst $ M.singleton v ty

---------------------------------------------------------------

class CanApply f where
  apply :: Ord v => Subst loc v -> f loc v -> f loc v

instance CanApply Type where
  apply (Subst s) = cata go . unType
    where
      go = \case
        VarT loc v -> case M.lookup v s of
          Nothing -> varT loc v
          Just t  -> apply (Subst s) t
        ConT loc name args -> conT loc name args
        ArrowT loc a b     -> arrowT loc a b
        TupleT loc as      -> tupleT loc as
        ListT loc a        -> listT loc a

instance CanApply Signature where
  apply (Subst s) (Signature (Fix expr)) = case expr of
    MonoT t     -> monoT $ apply (Subst s) t
    ForAllT loc x t -> forAllT loc x $ apply (Subst $ M.delete x s) (Signature t)

