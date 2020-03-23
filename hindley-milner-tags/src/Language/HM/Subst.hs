--------------------------------------------------------------------------------

-- | Capture-avoiding substitutions.
module Language.HM.Subst where

--------------------------------------------------------------------------------

import Data.Fix
import Data.Text (Text)
import qualified Data.Map.Strict as M

import Language.HM.Type
import Language.HM.Term

--------------------------------------------------------------------------------

-- | Substitutions of type variables for monomorphic types.
newtype Subst src = Subst { unSubst :: M.Map Text (Type src) }

instance Functor Subst where
  fmap f = Subst . fmap (fmap f) . unSubst

emptySubst :: Subst src
emptySubst = Subst M.empty

--------------------------------------------------------------------------------

applyType :: forall src . Subst src -> Type src -> Type src
applyType (Subst s) = cata g . unType
    where
        g :: TypeF src (Type src) -> Type src
        g (VarT src x) = case M.lookup x s of
            Nothing -> varT src x
            Just t  -> applyType (Subst s) t
        g (ConT src x) = conT src x
        g (AppT src t0 t1) = appT src t0 t1
        g (ArrowT src t0 t1) = arrowT src t0 t1

applySignature :: forall src . Subst src -> Signature src -> Signature src
applySignature s = cata g . unSignature
    where
        g :: SignatureF src (Signature src) -> Signature src
        g (MonoT t) = monoT (applyType s t)
        g (ForAllT src x t) = forAllT src x (applySignature (Subst $ M.delete x $ unSubst s) t)

-- instance CanApply TyTerm where
applyTyTerm :: Subst src -> TyTerm src -> TyTerm src
applyTyTerm s = TyTerm . cata g . unTyTerm
  where
      g (TypedF t) = Fix $ TypedF (applyTyped applyType s t)

applyTyped :: (Subst src -> t -> t) -> Subst src -> Typed t (f r) -> Typed t (f r)
applyTyped app s (Typed x t) = Typed x (app s t)

-- | @s1@ '<@>' @s2@ applies @s1@ to @s2@.
(<@>) :: Subst src -> Subst src -> Subst src
(Subst s1) <@> (Subst s2) = Subst $ M.map (applyType (Subst s1)) s2 `M.union` s1

--------------------------------------------------------------------------------
