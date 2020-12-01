{-# Language TypeFamilyDependencies #-}
-- | Main class for the library that defines common types and primitives for the language.
module Language.HM.Lang(
  -- * Lang
    Lang(..)
  , TypeOf
  , TermOf
  , TyTermOf
  , ContextOf
  , SubstOf
  , ErrorOf
  , Context(..)
  , insertCtx
  , lookupCtx
  -- * Context
) where

import Data.Map.Strict (Map)

import Language.HM.Term
import Language.HM.Subst
import Language.HM.Type
import Language.HM.TypeError
import Language.HM.TyTerm

import qualified Data.Map.Strict as M

-- | Main class to define inference API
class
  ( IsVar (Var q)
  , Show (Src q)
  , Eq (Src q)
  ) => Lang q where
  type Var q = r | r -> q
  type Src q
  type Prim q

  getPrimType :: Prim q -> TypeOf q

type TypeOf q = Type (Src q) (Var q)
type TermOf q = Term (Prim q) (Src q) (Var q)
type TyTermOf q = TyTerm (Prim q) (Src q) (Var q)
type ErrorOf q = TypeError (Src q) (Var q)
type ContextOf q = Context (Src q) (Var q)
type SubstOf q = Subst (Src q) (Var q)

-- | Context holds map of proven signatures for free variables in the expression.
newtype Context loc v = Context { unContext :: Map v (Signature loc v) }
  deriving (Show, Eq, Semigroup, Monoid)

instance CanApply Context where
  apply subst = Context . fmap (apply subst) . unContext

insertCtx :: Ord v => v -> Signature loc v ->  Context loc v -> Context loc v
insertCtx v sign (Context ctx) = Context $ M.insert v sign ctx

lookupCtx :: Ord v => v -> Context loc v -> Maybe (Signature loc v)
lookupCtx v (Context ctx) = M.lookup v ctx


