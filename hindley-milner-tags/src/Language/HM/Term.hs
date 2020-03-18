-- | This module contains the abstract syntax tree of the term language.
module Language.HM.Term where

import Data.Fix
import Data.Text (Text)

import Language.HM.Type

-- | The type of variable names.
type Var = Text

data TermF src v r
    = Var (Maybe src) v                   -- ^ Variables.
    | App (Maybe src) r r                 -- ^ Applications.
    | Abs (Maybe src) v r                 -- ^ Abstractions.
    | Let (Maybe src) v r r               -- ^ Let bindings.
    | AssertType (Maybe src) r (Signature src) -- ^ Assert type.
    deriving (Show, Functor, Foldable, Traversable)

-- | The type of terms.
type Term src = Fix (TermF src Var)

-- | 'varE' @x@ constructs a variable whose name is @x@.
varE :: Maybe src -> Var -> Term src
varE src = Fix . Var src

-- | 'appE' @l r@ constructs an application of @l@ to @r@.
appE :: Maybe src -> Term src -> Term src -> Term src
appE src l r = Fix $ App src l r

-- | 'absE' @x e@ constructs an abstraction of @x@ over @e@.
absE :: Maybe src -> Var -> Term src -> Term src
absE src x e = Fix $ Abs src x e

-- | 'letE' @x e0 e1@ constructs a binding of @e0@ to @x@ in @e1@.
letE :: Maybe src -> Var -> Term src -> Term src -> Term src
letE src x e0 e1 = Fix $ Let src x e0 e1

assertTypeE :: Maybe src -> Term src -> Signature src -> Term src
assertTypeE src a ty = Fix $ AssertType src a ty

--------------------------------------------------------------------------------

-- | Things with type annotations.
data Typed t a
    = Typed { untype :: a, tyAnn :: t }
    deriving (Show, Functor)

-- | Typed term variables.
type TyVar src = Typed (Signature src) Var

newtype TypedF t f r = TypedF { unTypedF :: Typed t (f r) }
    deriving Show

instance Functor f => Functor (TypedF t f) where
    fmap f (TypedF t) = TypedF (fmap (fmap f) t)

-- | Typed terms.
type TyTerm src = Fix (TypedF (Type src) (TermF src (TyVar src)))

-- | 'tyVarE' @x t@ constructs a variable whose name is @x@ and whose type is
-- @t@.
tyVarE :: Maybe src -> TyVar src -> Type src -> TyTerm src
tyVarE src x t = Fix $ TypedF $ Typed (Var src x) t

-- | 'tyAppE' @l r t@ constructs an application of @l@ to @r@ whose resulting
-- type is @t@.
tyAppE :: Maybe src -> TyTerm src -> TyTerm src -> Type src -> TyTerm src
tyAppE src l r t = Fix $ TypedF $ Typed (App src l r) t

-- | 'tyAbsE' @x e t@ constructs an abstraction of @x@ over @t@ whose type
-- is @t@.
tyAbsE :: Maybe src -> TyVar src -> TyTerm src -> Type src -> TyTerm src
tyAbsE src x e t = Fix $ TypedF $ Typed (Abs src x e) t

-- | 'tyLetE' @x e0 e1 t@ constructs a binding of @e0@ to @x@ in @e1@ whose
-- resulting type is @t@.
tyLetE :: Maybe src -> TyVar src -> TyTerm src -> TyTerm src -> Type src -> TyTerm src
tyLetE src x e0 e1 t = Fix $ TypedF $ Typed (Let src x e0 e1) t

--------------------------------------------------------------------------------
