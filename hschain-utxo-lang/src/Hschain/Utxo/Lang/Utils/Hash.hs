module Hschain.Utxo.Lang.Utils.Hash(
    getSha256
) where

import Data.ByteString (ByteString)

import HSChain.Crypto     (Hash(..), hashBlob)
import HSChain.Crypto.SHA (SHA256)

getSha256 :: ByteString -> ByteString
getSha256 bs = let Hash h = hashBlob @SHA256 bs in h
