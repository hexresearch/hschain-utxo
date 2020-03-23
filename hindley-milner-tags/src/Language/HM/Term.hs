-- | This module contains the abstract syntax tree of the term language.
module Language.HM.Term where

import Data.Fix
import Data.Text (Text)

import Language.HM.Type

-- | The type of variable names.
type Var = Text

data TermF src v r
    = Var src v                   -- ^ Variables.
    | App src r r                 -- ^ Applications.
    | Abs src v r                 -- ^ Abstractions.
    | Let src v r r               -- ^ Let bindings.
    | AssertType src r (Signature src) -- ^ Assert type.
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | The type of terms.
newtype Term src = Term { unTerm :: Fix (TermF src Var) }
  deriving (Show, Eq)

instance Functor Term where
  fmap f (Term x) =  Term $ cata go x
    where
      go = \case
        Var src v    -> Fix $ Var (f src) v
        App src a b  -> Fix $ App (f src) a b
        Abs src v a  -> Fix $ Abs (f src) v a
        Let src v a b -> Fix $ Let (f src) v a b
        AssertType src r sig -> Fix $ AssertType (f src) r (fmap f sig)

-- | 'varE' @x@ constructs a variable whose name is @x@.
varE :: src -> Var -> Term src
varE src = Term . Fix . Var src

-- | 'appE' @l r@ constructs an application of @l@ to @r@.
appE :: src -> Term src -> Term src -> Term src
appE src (Term l) (Term r) = Term $ Fix $ App src l r

-- | 'absE' @x e@ constructs an abstraction of @x@ over @e@.
absE :: src -> Var -> Term src -> Term src
absE src x (Term e) = Term $ Fix $ Abs src x e

-- | 'letE' @x e0 e1@ constructs a binding of @e0@ to @x@ in @e1@.
letE :: src -> Var -> Term src -> Term src -> Term src
letE src x (Term e0) (Term e1) = Term $ Fix $ Let src x e0 e1

assertTypeE :: src -> Term src -> Signature src -> Term src
assertTypeE src (Term a) ty = Term $ Fix $ AssertType src a ty

--------------------------------------------------------------------------------

-- | Things with type annotations.
data Typed t a
    = Typed { untype :: a, tyAnn :: t }
    deriving (Show, Functor)

-- | Typed term variables.
newtype TyVar src = TyVar { unTyVar :: Typed (Signature src) Var }

instance Functor TyVar where
  fmap f (TyVar (Typed var sign)) = TyVar $ Typed var (fmap f sign)

newtype TypedF t f r = TypedF { unTypedF :: Typed t (f r) }
    deriving Show

instance Functor f => Functor (TypedF t f) where
    fmap f (TypedF t) = TypedF (fmap (fmap f) t)

-- | Typed terms.
newtype TyTerm src = TyTerm { unTyTerm :: Fix (TypedF (Type src) (TermF src (TyVar src))) }

instance Functor TyTerm where
  fmap f (TyTerm x) = TyTerm $ cata go x
    where
      go (TypedF (Typed a ann)) = Fix $ TypedF $ Typed (tfm a) (fmap f ann) --(fmap f a) (fmap f ann)

      tfm = \case
        Var src v     -> Var (f src) (fmap f v)
        App src a b   -> App (f src) a b
        Abs src v a   -> Abs (f src) (fmap f v) a
        Let src v a b -> Let (f src) (fmap f v) a b
        AssertType src r sig -> AssertType (f src) r (fmap f sig)

-- | 'tyVarE' @x t@ constructs a variable whose name is @x@ and whose type is
-- @t@.
tyVarE :: src -> TyVar src -> Type src -> TyTerm src
tyVarE src x t = TyTerm $ Fix $ TypedF $ Typed (Var src x) t

-- | 'tyAppE' @l r t@ constructs an application of @l@ to @r@ whose resulting
-- type is @t@.
tyAppE :: src -> TyTerm src -> TyTerm src -> Type src -> TyTerm src
tyAppE src (TyTerm l) (TyTerm r) t = TyTerm $ Fix $ TypedF $ Typed (App src l r) t

-- | 'tyAbsE' @x e t@ constructs an abstraction of @x@ over @t@ whose type
-- is @t@.
tyAbsE :: src -> TyVar src -> TyTerm src -> Type src -> TyTerm src
tyAbsE src x (TyTerm e) t = TyTerm $ Fix $ TypedF $ Typed (Abs src x e) t

-- | 'tyLetE' @x e0 e1 t@ constructs a binding of @e0@ to @x@ in @e1@ whose
-- resulting type is @t@.
tyLetE :: src -> TyVar src -> TyTerm src -> TyTerm src -> Type src -> TyTerm src
tyLetE src x (TyTerm e0) (TyTerm e1) t = TyTerm $ Fix $ TypedF $ Typed (Let src x e0 e1) t

