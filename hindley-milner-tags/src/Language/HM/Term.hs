-- | This module contains the abstract syntax tree of the term language.
module Language.HM.Term(
    Term(..)
  , TermF(..)
  , Bind(..)
  , varE
  , appE
  , lamE
  , letE
  , letRecE
  , assertTypeE
  , freeVars
) where

import Data.Fix
import Data.Set (Set)

import Language.HM.Type

import qualified Data.Set as S

data TermF loc v r
    = Var loc v                       -- ^ Variables.
    | App loc r r                     -- ^ Applications.
    | Lam loc v r                     -- ^ Abstractions.
    | Let loc [Bind loc v r] r        -- ^ Let bindings.
    | LetRec loc [Bind loc v r] r     -- ^ Recursive  let bindings
    | AssertType loc r (Type loc v)   -- ^ Assert type.
    deriving (Show, Eq, Functor, Foldable, Traversable)

data Bind loc var r = Bind
  { bind'loc :: loc
  , bind'lhs :: var
  , bind'rhs :: r
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | The type of terms.
newtype Term loc v = Term { unTerm :: Fix (TermF loc v) }
  deriving (Show, Eq)

instance Functor (Term loc) where
  fmap f (Term x) =  Term $ cata go x
    where
      go = \case
        Var loc v    -> Fix $ Var loc (f v)
        App loc a b  -> Fix $ App loc a b
        Lam loc v a  -> Fix $ Lam loc (f v) a
        Let loc vs a -> Fix $ Let loc (fmap (\b ->  b { bind'lhs = f $ bind'lhs b }) vs) a
        LetRec loc vs a -> Fix $ LetRec loc (fmap (\b ->  b { bind'lhs = f $ bind'lhs b }) vs) a
        AssertType loc r sig -> Fix $ AssertType loc r (fmap f sig)

-- | 'varE' @x@ constructs a variable whose name is @x@.
varE :: loc -> var -> Term loc var
varE loc = Term . Fix . Var loc

-- | 'appE' @l r@ constructs an application of @l@ to @r@.
appE :: loc -> Term loc v -> Term loc v -> Term loc v
appE loc (Term l) (Term r) = Term $ Fix $ App loc l r

-- | 'absE' @x e@ constructs an abstraction of @x@ over @e@.
lamE :: loc -> v -> Term loc v -> Term loc v
lamE loc x (Term e) = Term $ Fix $ Lam loc x e

-- | 'letE' @x e0 e1@ constructs a binding of @e0@ to @x@ in @e1@.
letE :: loc -> [Bind loc v (Term loc v)] -> Term loc v -> Term loc v
letE loc binds (Term e) = Term $ Fix $ Let loc (fmap (fmap unTerm) binds) e

-- | 'letE' @x e0 e1@ constructs a binding of @e0@ to @x@ in @e1@.
letRecE :: loc -> [Bind loc v (Term loc v)] -> Term loc v -> Term loc v
letRecE loc binds (Term e) = Term $ Fix $ LetRec loc (fmap (fmap unTerm) binds) e

assertTypeE :: loc -> Term loc v -> Type loc v -> Term loc v
assertTypeE loc (Term a) ty = Term $ Fix $ AssertType loc a ty

--------------------------------------------------------------------------------

instance HasLoc (Term loc v) where
  type Loc (Term loc v) = loc

  getLoc (Term (Fix x)) = case x of
    Var loc _   -> loc
    App loc _ _ -> loc
    Lam loc _ _ -> loc
    Let loc _ _ -> loc
    LetRec loc _ _ -> loc
    AssertType loc _ _ -> loc

instance LocFunctor Term where
  mapLoc f (Term x) = Term $ cata go x
    where
      go = \case
        Var loc v    -> Fix $ Var (f loc) v
        App loc a b  -> Fix $ App (f loc) a b
        Lam loc v a  -> Fix $ Lam (f loc) v a
        Let loc vs a -> Fix $ Let (f loc) (fmap (\b ->  b { bind'loc = f $ bind'loc b }) vs) a
        LetRec loc vs a -> Fix $ LetRec (f loc) (fmap (\b ->  b { bind'loc = f $ bind'loc b }) vs) a
        AssertType loc r sig -> Fix $ AssertType (f loc) r (mapLoc f sig)

freeVars :: Ord v => Term loc v -> Set v
freeVars = cata go . unTerm
  where
    go = \case
      Var    _ v          -> S.singleton v
      App    _ a b        -> mappend a b
      Lam    _ v a        -> S.delete v a
      Let    _ binds body -> let lhs = S.fromList $ fmap bind'lhs binds
                             in  mappend (freeBinds binds)
                                         (body `S.difference` lhs)
      LetRec _ binds body -> let lhs = S.fromList $ fmap bind'lhs binds
                             in  (mappend (freeBinds binds) body) `S.difference` lhs
      AssertType _ a _    -> a

    freeBinds = foldMap bind'rhs



