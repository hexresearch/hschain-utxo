--------------------------------------------------------------------------------

-- | Capture-avoiding substitutions.
module Language.HM.Subst where

--------------------------------------------------------------------------------

import Data.Fix
import Data.Text (Text)
import qualified Data.Map as M

import Language.HM.Type
import Language.HM.Term

--------------------------------------------------------------------------------

-- | Substitutions of type variables for monomorphic types.
newtype Subst src = Subst { unSubst :: M.Map Text (Tau src) }

emptySubst :: Subst src
emptySubst = Subst M.empty

--------------------------------------------------------------------------------

applyTau :: forall src . Subst src -> Tau src -> Tau src
applyTau (Subst s) = cata g . unTau
    where
        g :: TauF src (Tau src) -> Tau src
        g (VarT src x) = case M.lookup x s of
            Nothing -> varT src x
            Just t  -> applyTau (Subst s) t
        g (ConT src x) = conT src x
        g (AppT src t0 t1) = appT src t0 t1
        g (ArrowT src t0 t1) = arrowT src t0 t1

applySigma :: forall src . Subst src -> Sigma src -> Sigma src
applySigma s = cata g . unSigma
    where
        g :: SigmaF src (Sigma src) -> Sigma src
        g (MonoT t) = monoT (applyTau s t)
        g (ForAllT src x t) = forAllT src x (applySigma (Subst $ M.delete x $ unSubst s) t)

-- instance CanApply TyTerm where
applyTyTerm :: Subst src -> TyTerm src -> TyTerm src
applyTyTerm s = cata g
  where
      g (TypedF t) = Fix $ TypedF (applyTyped applyTau s t)

applyTyped :: (Subst src -> t -> t) -> Subst src -> Typed t (f r) -> Typed t (f r)
applyTyped app s (Typed x t) = Typed x (app s t)

-- | @s1@ '<@>' @s2@ applies @s1@ to @s2@.
(<@>) :: Subst src -> Subst src -> Subst src
(Subst s1) <@> (Subst s2) = Subst $ M.map (applyTau (Subst s1)) s2 `M.union` s1

--------------------------------------------------------------------------------
