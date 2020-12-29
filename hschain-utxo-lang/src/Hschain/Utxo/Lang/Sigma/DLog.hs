module Hschain.Utxo.Lang.Sigma.DLog(
    DLog(..)
  , ProofDLog(..)
  , newDLog
  , verifyProofDLog
  , simulateProofDLog
  , getCommitmentDLog
) where

import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.Types

import qualified Codec.Serialise as CBOR

data ProofDLog a = ProofDLog
  { proofDLog'public      :: DLog a
  , proofDLog'commitmentA :: Commitment a
  , proofDLog'responseZ   :: Response a
  , proofDLog'challengeE  :: Challenge a
  } deriving (Generic)

data DLog a = DLog
  { dlog'publicKey :: PublicKey a
  } deriving (Generic)

newDLog :: EC a => Secret a -> DLog a
newDLog secret = DLog $ getPublicKey secret

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


deriving instance Show (ECPoint a) => Show (DLog a)
deriving instance Eq (ECPoint a)   => Eq (DLog a)
instance (CBOR.Serialise (ECPoint a)) => CBOR.Serialise (DLog a)

-- | Simulate proof of posession of discrete logarithm for given
-- challenge
simulateProofDLog :: EC a => DLog a -> Challenge a -> IO (ProofDLog a)
simulateProofDLog dl e = do
  z <- generateScalar
  return ProofDLog
    { proofDLog'public      = dl
    , proofDLog'commitmentA = getCommitment z e (dlog'publicKey dl)
    , proofDLog'responseZ   = z
    , proofDLog'challengeE  = e
    }

getCommitmentDLog :: EC a => Response a -> Challenge a -> DLog a -> Commitment a
getCommitmentDLog z ch (DLog pk) = getCommitment z ch pk

verifyProofDLog :: (EC a) => ProofDLog a -> Bool
verifyProofDLog ProofDLog{..}
  = fromGenerator proofDLog'responseZ == (proofDLog'commitmentA ^+^ (fromChallenge proofDLog'challengeE .*^ unPublicKey (dlog'publicKey proofDLog'public)))

