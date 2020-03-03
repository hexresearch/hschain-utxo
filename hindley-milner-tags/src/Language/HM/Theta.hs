--------------------------------------------------------------------------------

-- | Capture-avoiding substitutions.
module Language.HM.Theta where

--------------------------------------------------------------------------------

import Data.Fix
import Data.Text (Text)
import qualified Data.Map as M

import Language.HM.Type
import Language.HM.Term

--------------------------------------------------------------------------------

-- | Substitutions of type variables for monomorphic types.
newtype Theta src = Theta { unTheta :: M.Map Text (Tau src) }

emptyTheta :: Theta src
emptyTheta = Theta M.empty

--------------------------------------------------------------------------------

applyTau :: forall src . Theta src -> Tau src -> Tau src
applyTau (Theta s) = cata g . unTau
    where
        g :: TauF src (Tau src) -> Tau src
        g (VarT src x) = case M.lookup x s of
            Nothing -> varT src x
            Just t  -> applyTau (Theta s) t
        g (ConT src x) = conT src x
        g (AppT src t0 t1) = appT src t0 t1
        g (ArrowT src t0 t1) = arrowT src t0 t1

applySigma :: forall src . Theta src -> Sigma src -> Sigma src
applySigma s = cata g . unSigma
    where
        g :: SigmaF src (Sigma src) -> Sigma src
        g (MonoT t) = monoT (applyTau s t)
        g (ForAllT src x t) = forAllT src x (applySigma (Theta $ M.delete x $ unTheta s) t)

-- instance CanApply TyTerm where
applyTyTerm :: Theta src -> TyTerm src -> TyTerm src
applyTyTerm s = cata g
  where
      g (TypedF t) = Fix $ TypedF (applyTyped applyTau s t)

applyTyped :: (Theta src -> t -> t) -> Theta src -> Typed t (f r) -> Typed t (f r)
applyTyped app s (Typed x t) = Typed x (app s t)

-- | @s1@ '<@>' @s2@ applies @s1@ to @s2@.
(<@>) :: Theta src -> Theta src -> Theta src
(Theta s1) <@> (Theta s2) = Theta $ M.map (applyTau (Theta s1)) s2 `M.union` s1

--------------------------------------------------------------------------------
