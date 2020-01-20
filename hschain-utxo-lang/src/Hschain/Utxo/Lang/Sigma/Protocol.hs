module Hschain.Utxo.Lang.Sigma.Protocol
where

import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.Types

import qualified Codec.Serialise as CBOR

-- | Expression that should be proven
data SigmaE k a
  = Leaf k a
    -- ^ Proof of possession of discrete logarithm of point at
    --   elliptic curve
  | AND k [SigmaE k a]
    -- ^ AND connective
  | OR  k [SigmaE k a]
  deriving (Functor, Foldable, Traversable, Show)

sexprAnn :: SigmaE k a -> k
sexprAnn = \case
  Leaf k _ -> k
  AND     k _ -> k
  OR      k _ -> k

-- | Set of known keys
newtype Env a = Env [KeyPair a]
--
-- | Proof of knowledge of discrete logarithm
data ProofDL a = ProofDL
  { publicK     :: PublicKey a
  , commitmentA :: Commitment a
  , responseZ   :: Response a
  , challengeE  :: Challenge a
  } deriving (Generic)

deriving instance ( Show (ECPoint   a)
                  , Show (ECScalar  a)
                  , Show (Challenge a)
                  ) => Show (ProofDL a)

deriving instance ( Eq (ECPoint   a)
                  , Eq (ECScalar  a)
                  , Eq (Challenge a)
                  ) => Eq (ProofDL a)

instance ( CBOR.Serialise (ECPoint   a)
         , CBOR.Serialise (ECScalar  a)
         , CBOR.Serialise (Challenge a)
         ) => CBOR.Serialise (ProofDL a)


-- Simulate proof of posession of discrete logarithm for given
-- challenge
simulateProofDL :: EC a => PublicKey a -> Challenge a -> IO (ProofDL a)
simulateProofDL pk e = do
  z <- generateScalar
  return ProofDL
    { publicK     = pk
    , commitmentA =  getCommitment z e pk
    , responseZ   = z
    , challengeE  = e
    }

getCommitment :: EC a => Response a -> Challenge a -> PublicKey a -> Commitment a
getCommitment z ch pk = fromGenerator z ^+^ negateP (fromChallenge ch .*^ unPublicKey pk)

verifyProofDL :: (EC a, Eq (ECPoint a)) => ProofDL a -> Bool
verifyProofDL ProofDL{..}
  = fromGenerator responseZ == (commitmentA ^+^ (fromChallenge challengeE .*^ unPublicKey publicK))


