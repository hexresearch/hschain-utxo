-- | This module contains types for structured type errors.
module Language.HM.TypeError where

import Language.HM.Type

-- | Type errors.
data TypeError loc var
  = OccursErr  loc (Type loc var)
  | UnifyErr   loc (Type loc var) (Type loc var)  -- ^ Unification error
  | SubtypeErr loc (Type loc var) (Type loc var)  -- ^ Subtype error (happens on explicit type assertions)
  | NotInScopeErr loc var                         -- ^ Missing signature in context for free-variable.
  | EmptyCaseExpr loc                             -- ^ no case alternatives in the case expression
  deriving (Show, Eq)

