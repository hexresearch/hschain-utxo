--------------------------------------------------------------------------------

-- | This module contains the abstract syntax of Hindley-Milner types.
module Language.HM.Type (
    module Language.HM.Alpha,
    HasLoc(..),

    -- * Monomorphic types.
    TypeF(..),
    Type(..),
    varT,
    conT,
    appT,
    arrowT,

    -- * Polymorphic types.
    SignatureF(..),
    Signature(..),
    forAllT,
    monoT,
    stripSignature,

    HasTypeVars(..),
    VarSet(..),
    differenceVarSet,
    getVar
) where

--------------------------------------------------------------------------------

import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.Fix
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Map as M

import Language.HM.Alpha
import Text.Show.Deriving

--------------------------------------------------------------------------------

class HasLoc f where
  type Loc f :: *
  getLoc :: f -> Loc f

instance HasLoc (Type src) where
  type Loc (Type src) = src
  getLoc (Type (Fix x)) = case x of
    VarT src _ -> src
    ConT src _ -> src
    AppT src _ _ -> src
    ArrowT src _ _ -> src

instance HasLoc (Signature src) where
  type Loc (Signature src) = src
  getLoc (Signature (Fix x)) = case x of
    MonoT ty        -> getLoc ty
    ForAllT loc _ _ -> loc

data TypeF src r
    = VarT src Text
    | ConT src Text
    | AppT src r r
    | ArrowT src r r
    deriving (Eq, Ord, Show, Functor)

-- | Monomorphic types.
newtype Type src = Type { unType :: Fix (TypeF src) }
  deriving (Show, Eq, Ord)

-- | 'varT' @x@ constructs a type variable named @x@.
varT :: src -> Text -> Type src
varT src = Type . Fix . VarT src

-- | 'varT' @x@ constructs a type variable named @x@.
conT :: src -> Text -> Type src
conT src = Type . Fix . ConT src

-- | 'arrowT' @t0 t1@ constructs an arrow type from @t0@ to @t1@.
arrowT :: src -> Type src -> Type src -> Type src
arrowT src (Type t0) (Type t1) = Type $ Fix $ ArrowT src t0 t1

-- | 'appT' @t0 t1@ constructs a type application type from @t0@ to @t1@.
appT :: src -> Type src -> Type src -> Type src
appT src (Type t0) (Type t1) = Type $ Fix $ AppT src t0 t1

--------------------------------------------------------------------------------

data SignatureF src r
    = ForAllT src Text r
    | MonoT (Type src)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Polymorphic types.
newtype Signature src = Signature { unSignature :: Fix (SignatureF src)
  } deriving (Show, Eq, Ord)

instance Functor Signature where
  fmap f (Signature x) = Signature $ cata go x
    where
      go = \case
        ForAllT src var a -> Fix $ ForAllT (f src) var a
        MonoT ty          -> Fix $ MonoT $ fmap f ty

instance Functor Type where
  fmap f (Type x) = Type $ cata go x
    where
      go = \case
        VarT src name -> Fix $ VarT (f src) name
        ConT src name -> Fix $ ConT (f src) name
        AppT src a b  -> Fix $ AppT (f src) a b
        ArrowT src a b -> Fix $ ArrowT (f src) a b

-- | 'forAllT' @x t@ universally quantifies @x@ in @t@.
forAllT :: src -> Text -> Signature src -> Signature src
forAllT src x (Signature t) = Signature $ Fix $ ForAllT src x t

-- | 'monoT' @t@ lifts a monomorophic type @t@ to a polymorphic one.
monoT :: Type src -> Signature src
monoT = Signature . Fix . MonoT

instance AlphaEq (Signature src) where
    alphaEq (Signature sig0) (Signature sig1) = sigmaEq M.empty (unFix sig0) (unFix sig1)
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

            sigmaEq env (MonoT (Type t0)) (MonoT (Type t1)) =
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

instance HasTypeVars Type where
    tyVars = cata go . unType
        where
            go (VarT src x) = VarSet $ M.fromList [(x, src)]
            go (ConT _ _) = VarSet M.empty
            go (AppT _ (VarSet a) (VarSet b)) = VarSet $ a `M.union` b
            go (ArrowT _ (VarSet l) (VarSet r)) = VarSet $ l `M.union` r

    tyVarsInOrder = L.nubBy ((==) `on` fst) . cata go . unType
        where
            go (VarT src x) = [(x, src)]
            go (ConT _ _) = []
            go (AppT _ a b) = a ++ b
            go (ArrowT _ l r) = l ++ r

instance HasTypeVars Signature where
    tyVars = cata go . unSignature
        where
            go (MonoT t) = tyVars t
            go (ForAllT _ x (VarSet t)) = VarSet $ M.delete x t

    tyVarsInOrder = L.nubBy ((==) `on` fst) . cata go . unSignature
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

stripSignature :: Signature src -> Type src
stripSignature = cata go . unSignature
  where
    go = \case
      ForAllT _ _ r -> r
      MonoT ty -> ty

$(deriveShow1 ''TypeF)
$(deriveShow1 ''SignatureF)
$(deriveEq1 ''TypeF)
$(deriveEq1 ''SignatureF)
$(deriveOrd1 ''TypeF)
$(deriveOrd1 ''SignatureF)
