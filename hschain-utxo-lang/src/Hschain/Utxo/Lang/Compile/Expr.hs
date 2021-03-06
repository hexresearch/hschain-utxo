-- | Language for extended lambda calculus.
-- We compile our high-level language to this reduced language.
-- And then we compile from this language to core.
-- for this language we have type-inference that can derive types
-- for all subexpressions. This is useful to get explicit type-annotations
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
  , TypedName
  , TypedLamProg
  , TypedExprLam
  , getTypedDefType
  , defBodyToLam
  , liftTypedLamProg
  , mapLamProgType
) where

import Data.Fix
import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.Compile.Expr (PrimOp, PrimCon)
import Hschain.Utxo.Lang.Expr (Loc, VarName(..))

import qualified Type.Check.HM as H

-- | Programms annotated with types
type TypedLamProg = AnnLamProg (H.Type () Name) TypedName

-- | Typed definitions of functions
type TypedDef = AnnComb (H.Type () Name) TypedName

-- | Typed expressions
type TypedExprLam = AnnExprLam (H.Type () Name) TypedName

type TypedName = Typed (H.Type () Name) Name

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
newtype AnnLamProg ann bind = AnnLamProg { unAnnLamProg :: [AnnComb ann bind] }

-- | Extended lambda calculus programm
newtype LamProg = LamProg { unLamProg :: [Comb Name] }

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
  | EPrimOp !Loc !(PrimOp (H.Type () Name))
  -- ^ primitive operations
  | EAp !Loc a a
  -- ^ application
  | ELet !Loc [(bind, a)] a
  -- ^ let bindings
  | ELam !Loc [bind] a
  -- ^ lambda abstraction
  | EIf !Loc a a a
  -- ^ if expressions
  | ECase !Loc a [CaseAlt bind a]
  -- ^ case alternatives
  | EConstr !Loc (PrimCon (H.Type () Name))
  -- ^ constructor with tag id, also we should provide the type
  -- of constructor as a function for a type-checker
  | EAssertType !Loc a !(H.Type () Name)
  -- ^ Explicit type annotations
  | EBottom Loc
  -- ^ Value of any type that means failed programm.
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Case alternatives
data CaseAlt bind a = CaseAlt
  { caseAlt'loc   :: !Loc
  -- ^ source code location of the expression
  , caseAlt'tag   :: !(PrimCon (H.Type () Name))
  -- ^ integer tag of the constructor
  -- (integer substitution for the name of constructor)
  , caseAlt'args  :: [Name]
  -- ^ arguments of the pattern matching
  , caseAlt'rhs   :: a
  -- ^ right-hand side of the case-alternative
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Primitive values with locations
data PrimLoc = PrimLoc
  { primLoc'loc   :: !Loc   -- ^ location
  , primLoc'value :: !Prim  -- ^ primitive value
  } deriving (Show, Eq)

instance H.HasLoc TypedExprLam where
  type Loc TypedExprLam = Loc
  getLoc (Fix x) = H.getLoc $ ann'value x

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
    EConstr loc _       -> loc
    EAssertType loc _ _ -> loc
    EBottom loc         -> loc


-- | Reads  type signature of typed def
getTypedDefType :: TypedDef -> H.Type () Name
getTypedDefType Def{..} = foldr (H.arrowT ()) res args
  where
    args = fmap typed'type def'args
    res  = ann'note $ unFix def'body

defBodyToLam :: TypedDef -> TypedExprLam
defBodyToLam def@Def{..}
  | null def'args = def'body
  | otherwise     = Fix $ Ann
      { ann'note  = getTypedDefType def
      , ann'value = ELam (varName'loc def'name) def'args def'body
      }

liftTypedLamProg :: Monad m => (TypedExprLam -> m TypedExprLam) -> TypedLamProg -> m TypedLamProg
liftTypedLamProg f (AnnLamProg combs) =  fmap AnnLamProg $ mapM liftComb combs
  where
    liftComb def = do
      body <- f $ def'body def
      return $ def { def'body = body }

mapLamProgType :: (H.Type () Name -> H.Type () Name) -> LamProg -> LamProg
mapLamProgType f (LamProg combs) = LamProg $ fmap (fmap mapExpr) combs
  where
    mapExpr = foldFix $ \case
      EPrimOp loc op       -> Fix $ EPrimOp loc (fmap f op)
      EConstr loc con      -> Fix $ EConstr loc (fmap f con)
      EAssertType loc a ty -> Fix $ EAssertType loc a (f ty)
      ECase loc a alts     -> Fix $ ECase loc a (fmap mapAlt alts)
      other                -> Fix other

    mapAlt alt = alt { caseAlt'tag = fmap f $ caseAlt'tag alt }

----------------------------------------------------------------
-- instances

$(deriveEq1   ''ExprLamF)
$(deriveShow1 ''ExprLamF)
$(deriveEq1   ''CaseAlt)
$(deriveShow1 ''CaseAlt)
$(deriveEq1   ''Ann)
$(deriveOrd1  ''Ann)
$(deriveShow1 ''Ann)

deriving stock instance (Show bind, Show ann) => Show (AnnLamProg ann bind)
deriving stock instance Show LamProg



