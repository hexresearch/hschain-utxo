module Hschain.Utxo.Lang.Compile.Expr(
    Ann(..)
  , Def(..)
  , Comb
  , AnnComb
  , AnnDef(..)
  , CoreProg(..)
  , AnnProg
  , AnnExpr
  , Expr
  , ExprF(..)
  , CaseAlt(..)
) where

import Data.Fix
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Expr (Loc)

import qualified Language.HM as H

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
  } deriving (Show, Eq)

type AnnProg  ann bind = [AnnComb ann bind]
newtype CoreProg = CoreProg { unCoreProg :: [Comb Name] }

type AnnComb ann bind = Def bind (AnnExpr ann bind)
type Comb bind = Def bind (Expr bind)

data Def bind rhs = Def
  { def'name :: Name
  , def'args :: [bind]
  , def'body :: rhs
  } deriving (Functor, Foldable, Traversable, Show, Eq)

-- | Expressions of the Extended Core-language
data ExprF bind a
  = EVar Loc Name
  -- ^ variables
  | EPrim Loc !Prim
  -- ^ constant primitive
  | EAp Loc a a
  -- ^ application
  | ELet Loc [(bind, a)] a
  -- ^ lent bindings
  | ELam Loc [bind] a
  -- ^ lambda abstraction
  | EIf Loc a a a
  -- ^ if expressions
  | ECase Loc !a [CaseAlt bind a]
  -- ^ case alternatives
  | EConstr Loc Type !Int !Int
  -- ^ constructor with tag and arity, also we should provide the type
  -- of constructor as afunction for a type-checker
  | EBottom Loc
  -- ^ Value of any type that means failed programm.
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Case alternatives
data CaseAlt bind a = CaseAlt
  { caseAlt'loc   :: !Loc
  , caseAlt'tag   :: !Int
  -- ^ integer tag of the constructor
  -- (integer substitution for the name of constructor)
  , caseAlt'args  :: [Typed Name]
  -- ^ arguments of the pattern matching
  , caseAlt'constrType :: Type
  -- ^ Type of right hand side, it's the type that constructor belongs to
  , caseAlt'rhs   :: a
  -- ^ right-hand side of the case-alternative
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance H.HasLoc (Expr bind) where
  type Loc (Expr bind) = Loc
  getLoc (Fix expr) = H.getLoc expr

instance H.HasLoc (ExprF bind a) where
  type Loc (ExprF bind a) = Loc
  getLoc = \case
    EVar loc _         -> loc
    EPrim loc _        -> loc
    EAp loc _ _        -> loc
    ELet loc _ _       -> loc
    ELam loc _ _       -> loc
    EIf loc _ _ _      -> loc
    ECase loc _ _      -> loc
    EConstr loc _ _ _  -> loc
    EBottom loc        -> loc

