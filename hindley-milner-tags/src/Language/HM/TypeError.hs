--------------------------------------------------------------------------------

-- | This module contains types for structured type errors.
module Language.HM.TypeError where

--------------------------------------------------------------------------------

import Language.HM.Type

--------------------------------------------------------------------------------

-- | Type errors.
data TypeError v where
  OccursErr     :: IsVar v => Loc v -> v -> Type v -> TypeError v
  UnifyErr      :: IsVar v => Loc v -> Type v -> Type v -> TypeError v
  SubtypeErr    :: IsVar v => Loc v -> Type v -> Type v -> TypeError v
  NotInScopeErr :: IsVar v => Loc v -> v -> TypeError v

deriving instance (IsVar v, Show v, Show (Loc v)) => Show (TypeError v)
