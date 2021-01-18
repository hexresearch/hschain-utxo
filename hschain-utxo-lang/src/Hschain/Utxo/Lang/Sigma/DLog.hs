-- |
-- Proof of possession of discrete logarithm. This is non-interactive
-- proof that spender know scalar (private key) @x@ such that @g^x =
-- k@ where k is point on curve (public key), and @g@ is generator of
-- curve.
--
-- This is derived from Schnorr signatures.
module Hschain.Utxo.Lang.Sigma.DLog(
    ProofDLog(..)
  , verifyProofDLog
  , simulateProofDLog
  ) where

import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.Types

import qualified Codec.Serialise as CBOR

-- | Proof of possession of discrete logarithm (private key) of given
-- point on curve (public key). Spender proves knowledge of scalar @x@
-- such that
--
-- > u == g ^ x
--
-- where @u@ is public key. It is based on zero-knowledge sigma
-- protocols (Schnorr signatures).
data ProofDLog a = ProofDLog
  { proofDLog'public      :: PublicKey a
    -- ^ Possession private key corresponding to this public key is proven
  , proofDLog'commitmentA :: Commitment a
  , proofDLog'responseZ   :: Response   a
  , proofDLog'challengeE  :: Challenge  a
  } deriving (Generic)

deriving instance ( Show (ECPoint   a)
                  , Show (ECScalar  a)
                  , Show (Challenge a)
                  ) => Show (ProofDLog a)

deriving instance ( Eq (ECPoint   a)
                  , Eq (ECScalar  a)
                  , Eq (Challenge a)
                  ) => Eq (ProofDLog a)

instance ( CBOR.Serialise (ECPoint   a)
         , CBOR.Serialise (ECScalar  a)
         , CBOR.Serialise (Challenge a)
         ) => CBOR.Serialise (ProofDLog a)

-- | Simulate proof of posession of discrete logarithm for given
-- challenge
simulateProofDLog :: EC a => PublicKey a -> Challenge a -> IO (ProofDLog a)
simulateProofDLog pubK e = do
  z <- generateScalar
  return ProofDLog
    { proofDLog'public      = pubK
    , proofDLog'commitmentA = getCommitment z e pubK
    , proofDLog'responseZ   = z
    , proofDLog'challengeE  = e
    }

verifyProofDLog :: (EC a) => ProofDLog a -> Bool
verifyProofDLog ProofDLog{..}
  = fromGenerator proofDLog'responseZ == (proofDLog'commitmentA ^+^ (fromChallenge proofDLog'challengeE .*^ unPublicKey proofDLog'public))

