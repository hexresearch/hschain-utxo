-- |
--
-- Noninteractive proof that tuple is indeed Diffie-Hellman (DH) tuple.
-- DH tuple is one of the form
--
-- > (g, h=g^x, u=g^y, v=g^xy)
--
-- where @g@ is group generators and @x@,@y@ unknown constant Prover
-- demonstrates that he knows @y@ without disclosing it. Note that he
-- doesn't need to know @x@.
--
-- == Noninteractive protocol
--
-- 1. Prover chooses random @r : ℤ/q@ and keeps it secret. Then he
--    sends to verifier following commitments:
--
-- > t0 = g^r
-- > t1 = h^r = g^rx
--
-- 2. Verifier chooses random challenge @c : ℤ/q@, such @c@ is
--    generated using hash of @t0@,@t1@ and message.
--
-- 3. Prover computes @z = r + cy@ and sends it to verifier.
--
-- 4. Verifier accepts proof if
--
-- > g^z = t0·u^c = g^r·g^cy
-- > h^z = t1·v^c = g^rx·g^cxy
module Hschain.Utxo.Lang.Sigma.DTuple(
    DTuple(..)
  , ProofDTuple(..)
  , verifyProofDTuple
  , simulateProofDTuple
  , getCommitmentDTuple
) where

import Control.DeepSeq (NFData)

import Data.Either.Extra (eitherToMaybe)
import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)

import HSChain.Crypto.Classes (ByteRepr(..), defaultToJSON, defaultParseJSON)
import HSChain.Crypto.Classes.Hash

import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.Types

import qualified Data.ByteString.Lazy as LB
import qualified Codec.Serialise as CBOR

----------------------------------------------------------------
-- DH-tuple
----------------------------------------------------------------

-- | Diffie-Hellmann tuple.
data DTuple a = DTuple
  { dtuple'g    :: ECPoint a  -- ^ group generator @g@
  , dtuple'g_x  :: ECPoint a  -- ^ @g^x@
  , dtuple'g_y  :: ECPoint a  -- ^ @g^y@
  , dtuple'g_xy :: ECPoint a  -- ^ @g^xy@
  } deriving (Generic)

-- | Proof of knowledge of @y@ in Diffie-Hellman tuple
data ProofDTuple a = ProofDTuple
  { proofDTuple'public      :: DTuple a                      -- ^ public input for the algorithm
  , proofDTuple'commitmentA :: (Commitment a, Commitment a)  -- ^ commitments
  , proofDTuple'responseZ   :: Response a                    -- ^ response
  , proofDTuple'challengeE  :: Challenge a                   -- ^ challenge
  } deriving (Generic)

-- | Check that proof does validate provided DTuple
verifyProofDTuple :: EC a => ProofDTuple a -> Bool
-- NOTE: Not constant time
verifyProofDTuple ProofDTuple{..}
  =  (z .*^ g == t0 ^+^ (c .*^ u))
  && (z .*^ h == t1 ^+^ (c .*^ v))
  where
    DTuple g h u v = proofDTuple'public
    (t0, t1) = proofDTuple'commitmentA       -- Prover's commitments t0,t1
    c = fromChallenge proofDTuple'challengeE -- Challenge c
    z = proofDTuple'responseZ                -- Response z


getCommitmentDTuple :: EC a => Response a -> Challenge a -> DTuple a -> (Commitment a, Commitment a)
getCommitmentDTuple z ch DTuple{..} =
  ( getCommitment z ch (PublicKey dtuple'g_y)
  , getCommitment z ch (PublicKey dtuple'g_xy)
  )

-- | Simulate proof of posession of discrete logarithm for given
-- challenge
simulateProofDTuple :: EC a => DTuple a -> Challenge a -> IO (ProofDTuple a)
simulateProofDTuple dt e = do
  z <- generateScalar
  return ProofDTuple
    { proofDTuple'public      = dt
    , proofDTuple'commitmentA = getCommitmentDTuple z e dt
    , proofDTuple'responseZ   = z
    , proofDTuple'challengeE  = e
    }

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

deriving instance Show (ECPoint a) => Show (DTuple a)
deriving instance Eq (ECPoint a)   => Eq (DTuple a)
deriving instance Ord (ECPoint a)  => Ord (DTuple a)
deriving instance NFData (ECPoint a)  => NFData (DTuple a)
instance (CBOR.Serialise (ECPoint a)) => CBOR.Serialise (DTuple a)

instance CryptoHashable (ECPoint a) => CryptoHashable (DTuple a) where
  hashStep = genericHashStep hashDomain

instance ByteRepr (ECPoint a) => ByteRepr (DTuple a) where
  decodeFromBS bs = fromTuple =<< (eitherToMaybe $ CBOR.deserialiseOrFail $ LB.fromStrict bs)
    where
      fromTuple (genA, genB, pubA, pubB) =
        DTuple <$> decodeFromBS genA <*> decodeFromBS genB <*> decodeFromBS pubA <*> decodeFromBS pubB

  encodeToBS = LB.toStrict . CBOR.serialise . toTuple
    where
      toTuple DTuple{..} =
        ( encodeToBS dtuple'g
        , encodeToBS dtuple'g_x
        , encodeToBS dtuple'g_y
        , encodeToBS dtuple'g_xy
        )

instance ByteRepr (ECPoint a) => ToJSON (DTuple a) where
  toJSON = defaultToJSON

instance ByteRepr (ECPoint a) => FromJSON (DTuple a) where
  parseJSON = defaultParseJSON "DTuple"

deriving instance (Show (ECPoint a), Show (Response a), Show (Challenge a)) => Show (ProofDTuple a)
deriving instance (Eq (ECPoint a), Eq (Response a), Eq (Challenge a)) => Eq (ProofDTuple a)
instance (CBOR.Serialise (ECPoint a), CBOR.Serialise (Response a), CBOR.Serialise (Challenge a)) => CBOR.Serialise (ProofDTuple a)