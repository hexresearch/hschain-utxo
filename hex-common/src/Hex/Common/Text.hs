module Hex.Common.Text(
    showt
  , readRational
  , readInt
  , readPositiveInt
  , readDouble
  , ppText
  , ppDoc
  , pPrint
  , readMayt
) where

import Data.Text (Text)
import Text.Show.Pretty
import Safe

import qualified Data.Text      as T
import qualified Data.Text.Read as T

showt :: Show a => a -> Text
showt = T.pack . show

readRational :: Fractional a => Text -> Maybe a
readRational = runReader T.rational

readInt :: Integral a => Text -> Maybe a
readInt = runReader (T.signed T.decimal)

readPositiveInt :: Integral a => Text -> Maybe a
readPositiveInt = runReader T.decimal

readDouble :: Text -> Maybe Double
readDouble = runReader T.double

runReader :: T.Reader a -> Text -> Maybe a
runReader reader txt = case reader $ T.strip txt of
  Right (res, remainder) -> if T.null remainder
                              then Just res
                              else Nothing
  _                      -> Nothing

ppText :: Show a => a -> Text
ppText = T.pack . ppShow

-- | Please use specific reader functions if you can.
-- This function can introduce a leak.
readMayt :: Read a => Text -> Maybe a
readMayt = readMay . T.unpack
