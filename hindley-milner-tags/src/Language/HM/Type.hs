--------------------------------------------------------------------------------

-- | This module contains the abstract syntax of Hindley-Milner types.
module Language.HM.Type (
    module Language.HM.Alpha,
    HasSrc(..),

    -- * Monomorphic types.
    TauF(..),
    Tau(..),
    varT,
    conT,
    appT,
    arrowT,

    -- * Polymorphic types.
    SigmaF(..),
    Sigma(..),
    forAllT,
    monoT,

    HasTypeVars(..),
    VarSet(..),
    differenceVarSet,
    getVar
) where

--------------------------------------------------------------------------------


import Data.Fix
import Data.Function (on)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Map as M

import Language.HM.Alpha

--------------------------------------------------------------------------------

class HasSrc f where
  type Src f :: *
  getSrc :: f -> Src f

instance HasSrc (Tau src) where
  type Src (Tau src) = src
  getSrc (Tau (Fix x)) = case x of
    VarT src _ -> src
    ConT src _ -> src
    AppT src _ _ -> src
    ArrowT src _ _ -> src

data TauF src r
    = VarT src Text
    | ConT src Text
    | AppT src r r
    | ArrowT src r r
    deriving (Eq, Show, Functor)

-- | Monomorphic types.
newtype Tau src = Tau { unTau :: Fix (TauF src) }
  deriving (Show, Eq)

-- | 'varT' @x@ constructs a type variable named @x@.
varT :: src -> Text -> Tau src
varT src = Tau . Fix . VarT src

-- | 'varT' @x@ constructs a type variable named @x@.
conT :: src -> Text -> Tau src
conT src = Tau . Fix . ConT src

-- | 'arrowT' @t0 t1@ constructs an arrow type from @t0@ to @t1@.
arrowT :: src -> Tau src -> Tau src -> Tau src
arrowT src (Tau t0) (Tau t1) = Tau $ Fix $ ArrowT src t0 t1

-- | 'appT' @t0 t1@ constructs a type application type from @t0@ to @t1@.
appT :: src -> Tau src -> Tau src -> Tau src
appT src (Tau t0) (Tau t1) = Tau $ Fix $ AppT src t0 t1

--------------------------------------------------------------------------------

data SigmaF src r
    = ForAllT src Text r
    | MonoT (Tau src)
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Polymorphic types.
newtype Sigma src = Sigma { unSigma :: Fix (SigmaF src) }

-- | 'forAllT' @x t@ universally quantifies @x@ in @t@.
forAllT :: src -> Text -> Sigma src -> Sigma src
forAllT src x (Sigma t) = Sigma $ Fix $ ForAllT src x t

-- | 'monoT' @t@ lifts a monomorophic type @t@ to a polymorphic one.
monoT :: Tau src -> Sigma src
monoT = Sigma . Fix . MonoT

instance AlphaEq (Sigma src) where
    alphaEq (Sigma sig0) (Sigma sig1) = sigmaEq M.empty (unFix sig0) (unFix sig1)
        where
            tauEq env (VarT _ x) (VarT _ y) = case M.lookup x env of
                -- the variable is bound in the left expression: check that
                -- it matches the name of the variable in the right expression
                -- that was bound at the same point
                Just y' -> y == y'
                -- the variable is free in the left expression: it should have
                -- the same name as the variable in the right expression
                Nothing -> x == y
            tauEq _ (ConT _ x) (ConT _ y) = x == y
            tauEq env (AppT _ x0 x1) (AppT _ y0 y1) =
                tauEq env (unFix x0) (unFix y0) &&
                tauEq env (unFix x1) (unFix y1)
            tauEq env (ArrowT _ x0 x1) (ArrowT _ x0' x1') =
                tauEq env (unFix x0) (unFix x0') &&
                tauEq env (unFix x1) (unFix x1')
            tauEq _ _ _ = False

            sigmaEq env (MonoT (Tau t0)) (MonoT (Tau t1)) =
                tauEq env (unFix t0) (unFix t1)
            sigmaEq env (ForAllT _ x t0) (ForAllT _ y t1) =
                sigmaEq (M.insert x y env) (unFix t0) (unFix t1)
            sigmaEq _ _ _ = False

--------------------------------------------------------------------------------

-- | The class of types which have free type variables.
class HasTypeVars m where
    -- | 'tyVars' @t@ calculates the set of free type variables in @t@.
    tyVars :: m src -> VarSet src

    -- | 'tyVarsInOrder' @t@ is like 'tyVars' @t@, except that the type
    -- variables are returned in the order in which they are encountered.
    tyVarsInOrder :: m src -> [(Text, src)]

instance HasTypeVars Tau where
    tyVars = cata go . unTau
        where
            go (VarT src x) = VarSet $ M.fromList [(x, src)]
            go (ConT _ _) = VarSet M.empty
            go (AppT _ (VarSet a) (VarSet b)) = VarSet $ a `M.union` b
            go (ArrowT _ (VarSet l) (VarSet r)) = VarSet $ l `M.union` r

    tyVarsInOrder = L.nubBy ((==) `on` fst) . cata go . unTau
        where
            go (VarT src x) = [(x, src)]
            go (ConT _ _) = []
            go (AppT _ a b) = a ++ b
            go (ArrowT _ l r) = l ++ r

instance HasTypeVars Sigma where
    tyVars = cata go . unSigma
        where
            go (MonoT t) = tyVars t
            go (ForAllT _ x (VarSet t)) = VarSet $ M.delete x t

    tyVarsInOrder = L.nubBy ((==) `on` fst) . cata go . unSigma
        where
            go (MonoT t) = tyVarsInOrder t
            go (ForAllT src x t) = L.deleteBy ((==) `on` fst) (x, src) t

--------------------------------------------------------------------------------

newtype VarSet src = VarSet { unVarSet :: Map Text src }

instance Semigroup (VarSet src) where
  (VarSet a) <> (VarSet b) = VarSet $ M.union a b

instance Monoid (VarSet src) where
  mempty = VarSet M.empty

differenceVarSet :: VarSet src -> VarSet src -> VarSet src
differenceVarSet (VarSet a) (VarSet b) = VarSet $ a `M.difference` b

getVar :: VarSet src -> Text -> Maybe src
getVar (VarSet m) var = M.lookup var m


