-- | Module for special constants that are used in the library
-- It should not depend on any other module in the library.
module Hschain.Utxo.Lang.Const(
    main
  -- * Environment
  , getHeight
  , getSelf
  , getInputs
  , getOutputs
  , getDataInputs
  , getArgs
  -- * Boxes
  , getBoxArgs
  , getBoxId
  , getBoxScript
  , getBoxValue
  , getBoxPostHeight
  , sigmaOr
  , sigmaAnd
  , sigmaGreater
  , sigmaLess
  , sigmaGreaterEquals
  , sigmaLessEquals
  , sigmaEquals
  , sigmaNonEquals
  -- * List functions
  , listAt
  , map
  , filter
  , foldr
  , foldl
  , length
  , appendList
  , andSigma
  , orSigma
  -- * Text functions
  , appendText
  , lengthText
  -- * Bytes functions
  , appendBytes
  , lengthBytes
  , serialiseBytes
  , deserialiseBytes
  , sha256
  -- * bitcoin style signatures
  , checkSig
  , checkMultiSig
  -- * Evaluation constants
  , evalReductionLimit
) where

import Prelude hiding (map, filter, foldr, foldl, length)
import Data.String
import Data.Text (Text)

-- TODO: define all names for primitive functions in this module.
-- right now we use it only for
--
-- * read environment functions

-- names for primitive functions


---------------------------------------------------------------
-- names for functions that read environment

getHeight, getSelf, getInputs, getOutputs, getDataInputs :: Text

getHeight  = "getHeight"
getSelf    = "getSelf"
getInputs  = "getInputs"
getOutputs = "getOutputs"
getDataInputs = "getDataInputs"

getArgs :: Text -> Text
getArgs typeName = mconcat ["get", typeName, "Args"]

-------------------------------------------------------------------
-- boxes

getBoxArgs :: Text -> Text
getBoxArgs typeName = mconcat ["getBox", typeName, "Args"]

getBoxId, getBoxScript, getBoxValue, getBoxPostHeight :: Text

getBoxId         = "getBoxId"
getBoxScript     = "getBoxScript"
getBoxValue      = "getBoxValue"
getBoxPostHeight = "getBoxPostHeight"
-------------------------------------------------------------------
-- list functions

listAt, map, filter, foldr, foldl, length, appendList, andSigma, orSigma :: IsString a => a

listAt = "listAt"
map    = "map"
filter = "filter"
foldr  = "foldr"
foldl  = "foldl"
length = "length"
appendList = "++"
andSigma = "andSigma"
orSigma = "orSigma"

sigmaOr, sigmaAnd, sigmaGreater, sigmaLess, sigmaGreaterEquals, sigmaLessEquals, sigmaEquals, sigmaNonEquals :: IsString a => a

sigmaOr = "||*"
sigmaAnd = "&&*"
sigmaGreater = ">*"
sigmaLess = "<*"
sigmaLessEquals = "<=*"
sigmaGreaterEquals = ">=*"
sigmaEquals = "==*"
sigmaNonEquals = "/=*"

-------------------------------------------------------------------
-- text functions

appendText :: Text
appendText = "<>"

lengthText :: Text
lengthText = "lengthText"

-------------------------------------------------------------------
-- bytes functions

appendBytes :: Text
appendBytes = "appendBytes"

lengthBytes :: Text
lengthBytes = "lengthBytes"

serialiseBytes :: Text -> Text
serialiseBytes typeName = "serialise" <> typeName

deserialiseBytes :: Text -> Text
deserialiseBytes typeName = "deserialise" <> typeName

sha256 :: Text
sha256 = "sha256"

main :: Text
main = "main"

----------------------------------------------------------------
-- Bitcoin like signatures

checkSig, checkMultiSig :: Text

checkSig = "checkSig"
checkMultiSig = "checkMultiSig"

---------------------------------------------------------
-- Evaluation constants

evalReductionLimit :: Int
evalReductionLimit = 10000



