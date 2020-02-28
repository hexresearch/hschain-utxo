module Hex.Common.Serialise(
    serialiseToText
  , serialiseFromText
  , serialiseToJSON
  , serialiseFromJSON
) where

import Codec.Serialise

import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)

import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE

serialiseToText :: Serialise a => a -> Text
serialiseToText = TE.decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet . LB.toStrict . serialise

serialiseFromText :: Serialise a => Text -> Maybe a
serialiseFromText =
  (either (const Nothing) Just . deserialiseOrFail . LB.fromStrict <=< Base58.decodeBase58 Base58.bitcoinAlphabet) .
  TE.encodeUtf8

serialiseToJSON :: Serialise a => a -> Value
serialiseToJSON = toJSON . TE.decodeUtf8 . Base64.encode . LB.toStrict . serialise

serialiseFromJSON :: Serialise a => Value -> Parser a
serialiseFromJSON =
      (toParser . deserialiseOrFail . LB.fromStrict)
  <=< (toParser . Base64.decode . TE.encodeUtf8)
  <=< parseJSON
  where
    toParser = either (const mzero) pure

