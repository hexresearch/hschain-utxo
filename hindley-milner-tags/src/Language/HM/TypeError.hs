--------------------------------------------------------------------------------

-- | This module contains types for structured type errors.
module Language.HM.TypeError where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Language.HM.Type

--------------------------------------------------------------------------------

-- | Type errors.
data TypeError src
    = OccursErr src Text (Type src)
    | UnifyErr src (Type src) (Type src)
    | NotInScopeErr src Text
    deriving (Eq, Show)

--------------------------------------------------------------------------------
