--------------------------------------------------------------------------------

-- | This module contains types for structured type errors.
module Language.HM.TypeError where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Language.HM.Type

--------------------------------------------------------------------------------

-- | Type errors.
data TypeError src
    = OccursErr src Text (Tau src)
    | UnifyErr src (Tau src) (Tau src)
    | NotInScopeErr src Text
    deriving (Eq, Show)

--------------------------------------------------------------------------------
