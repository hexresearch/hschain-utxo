-- | Module defines main types for sigma-expressions.
module Hschain.Utxo.Lang.Sigma.Types where

import GHC.Generics

import HSChain.Crypto
import Hschain.Utxo.Lang.Sigma.EllipticCurve

import qualified Codec.Serialise as CBOR

-- | Pair of keys.
data KeyPair a = KeyPair
  { getSecretKey :: PrivKey a
  , getPublicKey :: PublicKey a
  } deriving (Generic)

instance (CBOR.Serialise (ECPoint a), CBOR.Serialise (ECScalar a)) => CBOR.Serialise (KeyPair a)

-- | Generate key-pair.
generateKeyPair :: EC a => IO (KeyPair a)
generateKeyPair = do
  s <- generatePrivKey
  return $ KeyPair s (publicKey s)

getCommitment :: EC a => Response a -> Challenge a -> PublicKey a -> Commitment a
getCommitment z ch pk = publicKey z ^+^ negateP (fromChallenge ch .*^ pk)
