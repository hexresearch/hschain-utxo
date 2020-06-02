module Hschain.Utxo.Lang.Compile.Expr(
    Ann(..)
  , Def(..)
  , AnnDef(..)
  , CoreProg
  , AnnProg
  , AnnExpr
  , Expr
  , ExprF(..)
  , CaseAlt(..)
) where

import Data.Fix
import Hschain.Utxo.Lang.Core.Data.Prim

data Ann ann f a = Ann
  { ann'note  :: ann
  , ann'value :: f a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

type AnnExpr ann bind = Fix (Ann ann (ExprF bind))
type Expr bind = Fix (ExprF bind)

data AnnDef ann bind = AnnDef
  { annDef'name :: Name
  , annDef'args :: [bind]
  , annDef'body :: AnnExpr ann bind
  }

type AnnProg  ann bind = [AnnComb ann bind]
type CoreProg = [Comb Name]

type AnnComb ann bind = Def bind (AnnExpr ann bind)
type Comb bind = Def bind (Expr bind)

data Def bind rhs = Def
  { def'name :: Name
  , def'args :: [bind]
  , def'body :: rhs
  } deriving (Functor, Foldable, Traversable)

-- | Expressions of the Extended Core-language
data ExprF bind a
  = EVar Name
  -- ^ variables
  | EPrim !Prim
  -- ^ constant primitive
  | EAp  a a
  -- ^ application
  | ELet [(bind, a)] a
  -- ^ lent bindings
  | ELam [bind] a
  -- ^ lambda abstraction
  | EIf a a a
  -- ^ if expressions
  | ECase !a [CaseAlt a]
  -- ^ case alternatives
  | EConstr Type !Int !Int
  -- ^ constructor with tag and arity, also we should provide the type
  -- of constructor as afunction for a type-checker
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Case alternatives
data CaseAlt a = CaseAlt
  { caseAlt'tag   :: !Int
  -- ^ integer tag of the constructor
  -- (integer substitution for the name of constructor)
  , caseAlt'args  :: [Name]
  -- ^ arguments of the pattern matching
  , caseAlt'rhs   :: a
  -- ^ right-hand side of the case-alternative
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)


