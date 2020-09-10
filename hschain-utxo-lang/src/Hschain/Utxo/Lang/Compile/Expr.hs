-- | Language for extended lambda calculus.
-- We compile our high-level language to this reduced language.
-- And then we compile from this language to core.
-- for this language we have type-inference that can derive types
-- for all subexpressions. This is useful to get rexplicit type-annotations
-- for fast type-checker in the core.
--
-- Lambda calculus is extended with very simple case-expressions.
-- Case expressions of this language can have only patterns based on constructors.
-- We can not use constants, wildcards or catch-all variables as patterns.
module Hschain.Utxo.Lang.Compile.Expr(
    Ann(..)
  , Def(..)
  , Comb
  , AnnComb
  , LamProg(..)
  , AnnLamProg(..)
  , AnnExprLam
  , ExprLam
  , ExprLamF(..)
  , CaseAlt(..)
  , PrimLoc(..)
  , TypedDef
  , TypedLamProg
  , TypedExprLam
  , getTypedDefType
  , liftTypedLamProg
) where

import Data.Fix
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Compile.Expr (PrimOp)
import Hschain.Utxo.Lang.Expr (Loc, VarName)

import qualified Language.HM as H

-- | Programms annotated with types
type TypedLamProg = AnnLamProg (H.Type () Name) (Typed (H.Type () Name) Name)

-- | Typed definitions of functions
type TypedDef = AnnComb (H.Type () Name) (Typed (H.Type () Name) Name)

-- | Typed expressions
type TypedExprLam = AnnExprLam (H.Type () Name) (Typed (H.Type () Name) Name)

-- | Annotation of the type with some additional information
data Ann ann f a = Ann
  { ann'note  :: ann  -- ^ value to annotate all nodes of the syntax tree
  , ann'value :: f a  -- ^ value of the syntax itself
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Annotated expressions
type AnnExprLam ann bind = Fix (Ann ann (ExprLamF bind))

-- | Expressions of extended lambda calculus
type ExprLam bind = Fix (ExprLamF bind)

-- | Annotated programm.
newtype AnnLamProg  ann bind = AnnLamProg { unAnnLamProg :: [AnnComb ann bind] }
  deriving (Show)

-- | Extended lambda calculus programm
newtype LamProg = LamProg { unLamProg :: [Comb Name] }
  deriving (Show)

-- | Annotated combinator
type AnnComb ann bind = Def bind (AnnExprLam ann bind)

-- | Combinator
type Comb bind = Def bind (ExprLam bind)

-- | Definition (or combinator).
data Def bind rhs = Def
  { def'name   :: VarName  -- ^ name of the definition
  , def'args   :: [bind]   -- ^ arguments
  , def'body   :: rhs      -- ^ body of the definition
  } deriving (Functor, Foldable, Traversable, Show, Eq)

-- | Expressions of the Extended Lambda calculus Core-language
data ExprLamF bind a
  = EVar !Loc !Name
  -- ^ variables
  | EPrim !Loc !PrimLoc
  -- ^ constant primitive
  | EPrimOp !Loc !PrimOp
  | EAp !Loc a a
  -- ^ application
  | ELet !Loc [(bind, a)] a
  -- ^ lent bindings
  | ELam !Loc [bind] a
  -- ^ lambda abstraction
  | EIf !Loc a a a
  -- ^ if expressions
  | ECase !Loc a [CaseAlt bind a]
  -- ^ case alternatives
  | EConstr !Loc !TypeCore !Int !Int
  -- ^ constructor with tag and arity, also we should provide the type
  -- of constructor as afunction for a type-checker
  | EAssertType !Loc a !TypeCore
  -- ^ Explicit type annotations
  | EBottom Loc
  -- ^ Value of any type that means failed programm.
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Case alternatives
data CaseAlt bind a = CaseAlt
  { caseAlt'loc   :: !Loc
  -- ^ source code location of the expression
  , caseAlt'tag   :: !Int
  -- ^ integer tag of the constructor
  -- (integer substitution for the name of constructor)
  , caseAlt'args  :: [Typed (H.Type () Name) Name]
  -- ^ arguments of the pattern matching
  , caseAlt'constrType :: TypeCore
  -- ^ Type of right hand side, it's the type that constructor belongs to
  , caseAlt'rhs   :: a
  -- ^ right-hand side of the case-alternative
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Primitive values with locations
data PrimLoc = PrimLoc
  { primLoc'loc   :: !Loc   -- ^ location
  , primLoc'value :: !Prim  -- ^ primitive value
  } deriving (Show, Eq)

instance H.HasLoc (ExprLam bind) where
  type Loc (ExprLam bind) = Loc
  getLoc (Fix expr) = H.getLoc expr

instance H.HasLoc (ExprLamF bind a) where
  type Loc (ExprLamF bind a) = Loc
  getLoc = \case
    EVar loc _          -> loc
    EPrim loc _         -> loc
    EPrimOp loc _       -> loc
    EAp loc _ _         -> loc
    ELet loc _ _        -> loc
    ELam loc _ _        -> loc
    EIf loc _ _ _       -> loc
    ECase loc _ _       -> loc
    EConstr loc _ _ _   -> loc
    EAssertType loc _ _ -> loc
    EBottom loc         -> loc


-- | Reads  type signature of typed def
getTypedDefType :: TypedDef -> TypeCore
getTypedDefType Def{..} = foldr (H.arrowT ()) res args
  where
    args = fmap typed'type def'args
    res  = ann'note $ unFix def'body

liftTypedLamProg :: Monad m => (TypedExprLam -> m TypedExprLam) -> TypedLamProg -> m TypedLamProg
liftTypedLamProg f (AnnLamProg combs) =  fmap AnnLamProg $ mapM liftComb combs
  where
    liftComb def = do
      body <- f $ def'body def
      return $ def { def'body = body }

