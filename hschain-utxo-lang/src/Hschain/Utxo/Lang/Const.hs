-- | Module for special constants that are used in the library
-- It should not depend on any other module in the library.
module Hschain.Utxo.Lang.Const(
  -- * Environment
    getHeight
  , getSelf
  , getInputs
  , getOutputs
  , getArgs
  -- * Boxes
  , getBoxArgs
  , getBoxName
  , getBoxScript
  , getBoxValue
) where

import Data.Text (Text)

-- TODO: define all names for primitive functions in this module.
-- right now we use it only for
--
-- * read environment functions

-- names for primitive functions


---------------------------------------------------------------
-- names for functions that read environment

getHeight, getSelf, getInputs, getOutputs :: Text

getHeight  = "getHeight"
getSelf    = "getSelf"
getInputs  = "getInputs"
getOutputs = "getOutputs"

getArgs :: Text -> Text
getArgs typeName = mconcat ["get", typeName, "Args"]

-------------------------------------------------------------------
-- boxes

getBoxArgs :: Text -> Text
getBoxArgs typeName = mconcat ["getBox", typeName, "Args"]

getBoxName, getBoxScript, getBoxValue :: Text

getBoxName   = "getBoxName"
getBoxScript = "getBoxScript"
getBoxValue  = "getBoxValue"

