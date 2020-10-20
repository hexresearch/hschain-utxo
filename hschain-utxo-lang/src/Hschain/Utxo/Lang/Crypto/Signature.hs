-- | Signatures based on Ed25519
module Hschain.Utxo.Lang.Crypto.Signature(
    Signature
  , signMessage
  , verifyMessage
) where

import Control.Applicative
import Crypto.Error

import Data.ByteString (ByteString)
import Data.Maybe

import Hschain.Utxo.Lang.Sigma (PublicKey, Secret, SigMessage, getPublicKey)

import HSChain.Crypto (ByteRepr(..), CryptoHashable)

import qualified Crypto.PubKey.Ed25519 as Ed

import qualified Data.ByteArray as BA

newtype Signature = Signature ByteString
  deriving newtype (ByteRepr, CryptoHashable)

signMessage :: Secret -> SigMessage -> Maybe Signature
signMessage privKey msg = maybeCryptoError $
  liftA2 (\priv pub -> Signature $ BA.convert $ Ed.sign priv pub (encodeToBS msg)) (fromPrivKey privKey) (fromPubKey pubKey)
  where
    pubKey = getPublicKey privKey

verifyMessage :: PublicKey -> Signature -> SigMessage -> Bool
verifyMessage pubKey signature msg = fromMaybe False $ maybeCryptoError $
  liftA2 (\pub sig -> Ed.verify pub (encodeToBS msg) sig) (fromPubKey pubKey) (fromSignature signature)

fromPrivKey :: Secret -> CryptoFailable Ed.SecretKey
fromPrivKey = Ed.secretKey . encodeToBS

fromPubKey :: PublicKey -> CryptoFailable Ed.PublicKey
fromPubKey  = Ed.publicKey . encodeToBS

fromSignature :: Signature -> CryptoFailable Ed.Signature
fromSignature = Ed.signature . encodeToBS

