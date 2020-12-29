-- | Types and functions for sigma-protocol.
module Hschain.Utxo.Lang.Sigma.Protocol(
    SigmaE(..)
  , sexprAnn
  , Env(..)
  , AtomicProof(..)
  , ProofInput(..)
  , simulateAtomicProof
  , verifyAtomicProof
  , responseZ
  , getProofInput
) where

import Data.Aeson   (FromJSON,ToJSON)
import GHC.Generics (Generic)

import HSChain.Crypto.Classes (ByteRepr(..))

import Hschain.Utxo.Lang.Sigma.DLog
import Hschain.Utxo.Lang.Sigma.DTuple
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
    -- ^ OR connective
  deriving (Functor, Foldable, Traversable, Show, Eq)

sexprAnn :: SigmaE k a -> k
sexprAnn = \case
  Leaf k _ -> k
  AND  k _ -> k
  OR   k _ -> k

-- | Set of known keys
newtype Env a = Env { unEnv :: [KeyPair a] }

data ProofInput a
  = InputDLog (DLog a)
  | InputDTuple (DTuple a)
  deriving (Generic)

deriving instance ( Show (ECPoint a)
                  , Show (ProofDTuple a)
                  , Show (ProofDLog   a)
                  ) => Show (ProofInput a)

deriving instance ( Eq (ECPoint a)
                  , Eq (ProofDTuple a)
                  , Eq (ProofDLog   a)
                  ) => Eq (ProofInput a)

instance ( CBOR.Serialise (ECPoint a)
         , CBOR.Serialise (ProofDTuple a)
         , CBOR.Serialise (ProofDLog   a)
         ) => CBOR.Serialise (ProofInput a)

deriving newtype instance (ByteRepr (ECPoint a)) => ToJSON (ProofInput a)
deriving newtype instance (ByteRepr (ECPoint a)) => FromJSON (ProofInput a)

data AtomicProof a
  = ProofDL (ProofDLog a)
  | ProofDT (ProofDTuple a)
  deriving (Generic)

getProofInput :: AtomicProof a -> ProofInput a
getProofInput = \case
  ProofDL ProofDLog{..}   -> InputDLog   proofDLog'public
  ProofDT ProofDTuple{..} -> InputDTuple proofDTuple'public

responseZ :: AtomicProof a -> Response a
responseZ = \case
  ProofDL ProofDLog{..}   -> proofDLog'responseZ
  ProofDT ProofDTuple{..} -> proofDTuple'responseZ

deriving instance ( Show (ProofDTuple a)
                  , Show (ProofDLog   a)
                  ) => Show (AtomicProof a)

deriving instance ( Eq (ProofDTuple a)
                  , Eq (ProofDLog   a)
                  ) => Eq (AtomicProof a)

instance ( CBOR.Serialise (ProofDTuple a)
         , CBOR.Serialise (ProofDLog a)
         ) => CBOR.Serialise (AtomicProof a)


-- | Simulate proof of posession of discrete logarithm for given
-- challenge
simulateAtomicProof :: EC a => ProofInput a -> Challenge a -> IO (AtomicProof a)
simulateAtomicProof inp e = case inp of
  InputDLog dlog     -> fmap ProofDL $ simulateProofDLog dlog e
  InputDTuple dtuple -> fmap ProofDT $ simulateProofDTuple dtuple e

verifyAtomicProof :: (EC a) => AtomicProof a -> Bool
verifyAtomicProof = \case
  ProofDL dlog   -> verifyProofDLog dlog
  ProofDT dtuple -> verifyProofDTuple dtuple

