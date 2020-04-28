--------------------------------------------------------------------------------

-- | This module contains the abstract syntax of Hindley-Milner types.
module Language.HM.Type (
    module Language.HM.Alpha,
    IsVar(..),
    HasLoc(..),
    -- * Monomorphic types.
    TypeF(..),
    Type(..),
    varT,
    conT,
    arrowT,

    -- * Polymorphic types.
    SignatureF(..),
    Signature(..),
    forAllT,
    monoT,
    stripSignature,
    typeToSignature,

    HasTypeVars(..),
) where

--------------------------------------------------------------------------------

import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.Fix
import Data.Set (Set)

import qualified Data.List as L
import qualified Data.Set as S

import Language.HM.Alpha
import Text.Show.Deriving

--------------------------------------------------------------------------------

class HasLoc f where
  type Loc f :: *
  getLoc :: f -> Loc f


class (Ord v, HasLoc v) => IsVar v where
  arrowVar   :: Loc v -> v
  intToVar   :: Int -> v

instance IsVar v => HasLoc (Type v) where
  type Loc (Type v) = Loc v

  getLoc (Type (Fix x)) = case x of
    VarT v -> getLoc v
    ConT v _ -> getLoc v

instance IsVar v => HasLoc (Signature v) where
  type Loc (Signature v) = Loc v

  getLoc (Signature x) = cata go x
    where
      go = \case
        MonoT ty    -> getLoc ty
        ForAllT _ a -> a

data TypeF var r
    = VarT var
    | ConT var [r]
    deriving (Eq, Ord, Show, Functor)

-- | Monomorphic types.
newtype Type a = Type { unType :: Fix (TypeF a) }
  deriving (Show, Eq, Ord)

-- | 'varT' @x@ constructs a type variable named @x@.
varT :: var -> Type var
varT = Type . Fix . VarT

-- | 'varT' @x@ constructs a type variable named @x@.
conT :: var -> [Type var] -> Type var
conT name args = Type $ Fix $ ConT name $ fmap unType $ args

-- | 'arrowT' @t0 t1@ constructs an arrow type from @t0@ to @t1@.
arrowT :: IsVar v => Type v -> Type v -> Type v
arrowT t0 t1 = conT (arrowVar $ getLoc t0) [t0, t1]


--------------------------------------------------------------------------------

data SignatureF var r
    = ForAllT var r
    | MonoT (Type var)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Polymorphic types.
newtype Signature src = Signature { unSignature :: Fix (SignatureF src)
  } deriving (Show, Eq, Ord)

instance Functor Signature where
  fmap f (Signature x) = Signature $ cata go x
    where
      go = \case
        ForAllT var a -> Fix $ ForAllT (f var) a
        MonoT ty      -> Fix $ MonoT $ fmap f ty

instance Functor Type where
  fmap f (Type x) = Type $ cata go x
    where
      go = \case
        VarT name      -> Fix $ VarT $ f name
        ConT name args -> Fix $ ConT (f name) args

-- | 'forAllT' @x t@ universally quantifies @x@ in @t@.
forAllT :: v -> Signature v -> Signature v
forAllT x (Signature t) = Signature $ Fix $ ForAllT x t

-- | 'monoT' @t@ lifts a monomorophic type @t@ to a polymorphic one.
monoT :: Type src -> Signature src
monoT = Signature . Fix . MonoT

{-
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
-}


typeToSignature :: IsVar v => Type v -> Signature v
typeToSignature ty = foldr forAllT (monoT ty) vs
  where
    vs = tyVarsInOrder ty

--------------------------------------------------------------------------------

-- | The class of types which have free type variables.
class HasTypeVars f where
    -- | 'tyVars' @t@ calculates the set of free type variables in @t@.
    tyVars :: IsVar v => f v -> Set v

    -- | 'tyVarsInOrder' @t@ is like 'tyVars' @t@, except that the type
    -- variables are returned in the order in which they are encountered.
    tyVarsInOrder :: IsVar v => f v -> [v]

instance HasTypeVars Type where
    tyVars = cata go . unType
        where
            go (VarT v) = S.singleton v
            go (ConT _ args) = mconcat args

    tyVarsInOrder = L.nub . cata go . unType
        where
            go (VarT x) = [x]
            go (ConT _ xs) = mconcat xs

instance HasTypeVars Signature where
    tyVars = cata go . unSignature
        where
            go (MonoT t) = tyVars t
            go (ForAllT x t) = S.delete x t

    tyVarsInOrder = L.nub . cata go . unSignature
        where
            go (MonoT t) = tyVarsInOrder t
            go (ForAllT x t) = L.delete x t

--------------------------------------------------------------------------------

stripSignature :: Signature src -> Type src
stripSignature = cata go . unSignature
  where
    go = \case
      ForAllT _ r -> r
      MonoT ty -> ty

$(deriveShow1 ''TypeF)
$(deriveShow1 ''SignatureF)
$(deriveEq1 ''TypeF)
$(deriveEq1 ''SignatureF)
$(deriveOrd1 ''TypeF)
$(deriveOrd1 ''SignatureF)
