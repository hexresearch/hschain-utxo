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
  , getBoxId
  , getBoxScript
  , getBoxValue
  -- * List functions
  , listAt
  , map
  , filter
  , foldr
  , foldl
  , length
  , appendList
  -- * Bytes functions
  , appendBytes
  , serialiseBytes
  , deserialiseBytes
  , sha256
) where

import Prelude hiding (map, filter, foldr, foldl, length)
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

getBoxId, getBoxScript, getBoxValue :: Text

getBoxId     = "getBoxId"
getBoxScript = "getBoxScript"
getBoxValue  = "getBoxValue"

-------------------------------------------------------------------
-- list functions

listAt, map, filter, foldr, foldl, length, appendList :: Text

listAt = "listAt"
map    = "map"
filter = "filter"
foldr  = "foldr"
foldl  = "foldl"
length = "length"
appendList = "++"

-------------------------------------------------------------------
-- bytes functions

appendBytes :: Text
appendBytes = "appendBytes"

serialiseBytes :: Text -> Text
serialiseBytes typeName = "serialise" <> typeName

deserialiseBytes :: Text -> Text
deserialiseBytes typeName = "deserialise" <> typeName

sha256 :: Text
sha256 = "sha256"

