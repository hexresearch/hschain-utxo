--------------------------------------------------------------------------------

-- | This module contains types for structured type errors.
module Language.HM.TypeError where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Language.HM.Type

--------------------------------------------------------------------------------

-- | Type errors.
data TypeError src
    = OccursErr (Maybe src) Text (Type src)
    | UnifyErr (Maybe src) (Type src) (Type src)
    | NotInScopeErr (Maybe src) Text
    deriving (Eq, Show)

--------------------------------------------------------------------------------
