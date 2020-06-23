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
  deriving (Show, Eq, Functor)

instance LocFunctor TypeError where
  mapLoc f = \case
    OccursErr loc ty     -> OccursErr (f loc) (mapLoc f ty)
    UnifyErr loc tA tB   -> UnifyErr (f loc) (mapLoc f tA) (mapLoc f tB)
    SubtypeErr loc tA tB -> SubtypeErr (f loc) (mapLoc f tA) (mapLoc f tB)
    NotInScopeErr loc v  -> NotInScopeErr (f loc) v
    EmptyCaseExpr loc    -> EmptyCaseExpr (f loc)

