-- | This module contains the abstract syntax tree of the term language.
module Language.HM.Term(
    Term(..)
  , TermF(..)
  , varE
  , appE
  , lamE
  , letE
  , letRecE
  , assertTypeE
) where

import Control.Arrow

import Data.Fix

import Language.HM.Type

data TermF v r
    = Var v                   -- ^ Variables.
    | App r r                 -- ^ Applications.
    | Lam v r                 -- ^ Abstractions.
    | Let [(v, r)] r          -- ^ Let bindings.
    | LetRec [(v, r)] r       -- ^ Recursive  let bindings
    | AssertType r (Type v)   -- ^ Assert type.
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | The type of terms.
newtype Term v = Term { unTerm :: Fix (TermF v) }
  deriving (Show, Eq)

instance Functor Term where
  fmap f (Term x) =  Term $ cata go x
    where
      go = \case
        Var v    -> Fix $ Var (f v)
        App a b  -> Fix $ App a b
        Lam v a  -> Fix $ Lam (f v) a
        Let vs b -> Fix $ Let (fmap (first f) vs) b
        LetRec vs b -> Fix $ LetRec (fmap (first f) vs) b
        AssertType r sig -> Fix $ AssertType r (fmap f sig)

-- | 'varE' @x@ constructs a variable whose name is @x@.
varE :: var -> Term var
varE = Term . Fix . Var

-- | 'appE' @l r@ constructs an application of @l@ to @r@.
appE :: Term v -> Term v -> Term v
appE (Term l) (Term r) = Term $ Fix $ App l r

-- | 'absE' @x e@ constructs an abstraction of @x@ over @e@.
lamE :: v -> Term v -> Term v
lamE x (Term e) = Term $ Fix $ Lam x e

-- | 'letE' @x e0 e1@ constructs a binding of @e0@ to @x@ in @e1@.
letE :: [(v, Term v)] -> Term v -> Term v
letE vs (Term e1) = Term $ Fix $ Let (fmap (second unTerm) vs) e1

-- | 'letE' @x e0 e1@ constructs a binding of @e0@ to @x@ in @e1@.
letRecE :: [(v, Term v)] -> Term v -> Term v
letRecE vs (Term e1) = Term $ Fix $ LetRec (fmap (second unTerm) vs) e1

assertTypeE :: Term v -> Type v -> Term v
assertTypeE (Term a) ty = Term $ Fix $ AssertType a ty

--------------------------------------------------------------------------------

instance IsVar v => HasLoc (Term v) where
  type Loc (Term v) = Loc v

  getLoc (Term (Fix x)) = case x of
    Var v -> getLoc v
    App a _ -> getLoc (Term a)
    Lam v _ -> getLoc v
    Let _ a -> getLoc (Term a)
    LetRec _ a -> getLoc (Term a)
    AssertType _ a -> getLoc a

