-- | Module defines main types for sigma-expressions.
module Hschain.Utxo.Lang.Sigma.Types
where

import Control.DeepSeq (NFData)

import Data.Coerce

import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Sigma.EllipticCurve

import qualified Codec.Serialise as CBOR

-- | Private key.
newtype Secret    a = Secret    { unSecret    :: ECScalar a
  } deriving (Generic)

-- | Public key.
newtype PublicKey a = PublicKey { unPublicKey :: ECPoint  a
  } deriving (Generic)

-- | Pair of keys.
data KeyPair a = KeyPair
  { secretKey :: Secret a
  , publicKey :: PublicKey a
  }

type Commitment a = ECPoint a
type Response a   = ECScalar a

deriving stock   instance Show (ECPoint a) => Show (PublicKey a)
deriving stock   instance Eq   (ECPoint a) => Eq   (PublicKey a)
deriving stock   instance Ord  (ECPoint a) => Ord  (PublicKey a)
deriving newtype instance (CBOR.Serialise (ECScalar a)) => CBOR.Serialise (Secret a)
deriving newtype instance (CBOR.Serialise (ECPoint a)) => CBOR.Serialise (PublicKey a)
deriving newtype instance NFData (ECPoint a) => NFData (PublicKey a)

-- | Generate new private key.
generateSecretKey :: EC a => IO (Secret a)
generateSecretKey = coerce generateScalar

-- | Convert private key to public.
getPublicKey :: EC a => Secret a -> PublicKey a
getPublicKey = coerce fromGenerator

-- | Generate key-pair.
generateKeyPair :: EC a => IO (KeyPair a)
generateKeyPair = do
  s <- generateSecretKey
  return $ KeyPair s (getPublicKey s)

deriving instance Show (Secret    Ed25519)
deriving instance Eq   (Secret    Ed25519)
deriving instance Ord  (Secret    Ed25519)


