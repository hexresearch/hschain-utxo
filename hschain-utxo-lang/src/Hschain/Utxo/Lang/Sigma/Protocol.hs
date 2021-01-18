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

import Data.Data
import Control.DeepSeq (NFData)
import Data.Aeson   (FromJSON(..), ToJSON(..))
import Data.Either.Extra (eitherToMaybe)
import GHC.Generics (Generic)

import HSChain.Crypto.Classes (ByteRepr(..), defaultToJSON, defaultParseJSON)
import HSChain.Crypto.Classes.Hash

import Hschain.Utxo.Lang.Sigma.DLog
import Hschain.Utxo.Lang.Sigma.DTuple
import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.Types

import qualified Data.ByteString.Lazy as LB
import qualified Codec.Serialise as CBOR
import qualified Language.Haskell.TH.Syntax as TH

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

-- | Public proof data (inputs for proof algorithms)
data ProofInput a
  = InputDLog   (PublicKey a)      -- ^ proof of discrte log
  | InputDTuple (DTuple a)  -- ^ proof of Diffie-Hellman tuple
  deriving (Generic)

instance ByteRepr (ECPoint a) => ByteRepr (ProofInput a) where
  decodeFromBS bs = fromEither =<< (eitherToMaybe $ CBOR.deserialiseOrFail $ LB.fromStrict bs)
    where
      fromEither = \case
        Left lbs  -> fmap InputDLog   $ decodeFromBS lbs
        Right rbs -> fmap InputDTuple $ decodeFromBS rbs

  encodeToBS = LB.toStrict . CBOR.serialise . toEither
    where
      toEither = \case
        InputDLog dlog     -> Left  $ encodeToBS dlog
        InputDTuple dtuple -> Right $ encodeToBS dtuple

deriving instance ( Show (ECPoint a)
                  , Show (Response a)
                  , Show (Challenge   a)
                  ) => Show (ProofInput a)

deriving instance ( Eq (ECPoint a)
                  , Eq (Response a)
                  , Eq (Challenge   a)
                  ) => Eq (ProofInput a)

deriving instance ( Ord (ECPoint a)
                  , Ord (Response a)
                  , Ord (Challenge a)
                  ) => Ord (ProofInput a)

deriving instance ( NFData (ECPoint a)
                  ) => NFData (ProofInput a)

instance ( CBOR.Serialise (ECPoint a)
         , CBOR.Serialise (ProofDTuple a)
         , CBOR.Serialise (ProofDLog   a)
         ) => CBOR.Serialise (ProofInput a)

instance (ByteRepr (ECPoint a)) => ToJSON (ProofInput a) where
  toJSON = defaultToJSON

instance (ByteRepr (ECPoint a)) => FromJSON (ProofInput a) where
  parseJSON = defaultParseJSON "ProofInput"

instance CryptoHashable (ECPoint a) => CryptoHashable (ProofInput a) where
  hashStep = genericHashStep hashDomain

instance Typeable a => Data (ProofInput a) where
  gfoldl _ _ _ = error       "ProofInput.gfoldl"
  toConstr _   = error       "ProofInput.toConstr"
  gunfold _ _  = error       "ProofInput.gunfold"
  dataTypeOf _ = mkNoRepType "Hschain.Utxo.Lang.Sigma.Types.ProofInput"

instance ByteRepr (ECPoint a) => TH.Lift (ProofInput a) where
  lift pk = [| let Just k = decodeFromBS bs in k |]
    where
      bs = encodeToBS pk

-- | Proof of one of two methods of key verification
data AtomicProof a
  = ProofDL (ProofDLog a)    -- ^ proof of knowledge of discrete log
  | ProofDT (ProofDTuple a)  -- ^ proof of knowledge of Diffie-Helman tuple
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

