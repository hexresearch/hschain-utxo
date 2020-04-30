--------------------------------------------------------------------------------

-- | This module contains types for structured type errors.
module Language.HM.TypeError where

--------------------------------------------------------------------------------

import Language.HM.Type

--------------------------------------------------------------------------------

-- | Type errors.
data TypeError loc var
  = OccursErr  loc (Type loc var)
  | UnifyErr   loc (Type loc var) (Type loc var)
  | SubtypeErr loc (Type loc var) (Type loc var)
  | NotInScopeErr loc var
  deriving (Show, Eq)

