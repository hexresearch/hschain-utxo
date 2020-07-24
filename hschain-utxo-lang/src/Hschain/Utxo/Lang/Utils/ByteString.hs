-- | Haskell functions that copy behavior of hschain-utxo primitives
-- for ByteStrings
module Hschain.Utxo.Lang.Utils.ByteString(
    getSha256
  , serialiseInt
  , serialiseText
  , serialiseBool
  , serialiseByteString

  , deserialiseInt
  , deserialiseBool
  , deserialiseText
  , deserialiseByteString
) where

import Codec.Serialise

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)

import HSChain.Crypto     (Hash(..), hashBlob)
import HSChain.Crypto.SHA (SHA256)

import qualified Data.ByteString.Lazy as LB

getSha256 :: ByteString -> ByteString
getSha256 bs = let Hash h = hashBlob @SHA256 bs in h

serialiseInt :: Int64 -> ByteString
serialiseInt = genSerialise

serialiseText :: Text -> ByteString
serialiseText = genSerialise

serialiseBool :: Bool -> ByteString
serialiseBool = genSerialise

serialiseByteString :: ByteString -> ByteString
serialiseByteString = genSerialise

deserialiseInt :: ByteString -> Int64
deserialiseInt = genDeserialise

deserialiseText :: ByteString -> Text
deserialiseText = genDeserialise

deserialiseBool :: ByteString -> Bool
deserialiseBool = genDeserialise

deserialiseByteString :: ByteString -> ByteString
deserialiseByteString = genDeserialise

genSerialise :: Serialise a => a -> ByteString
genSerialise = LB.toStrict . serialise

genDeserialise :: Serialise a => ByteString -> a
genDeserialise = deserialise . LB.fromStrict

