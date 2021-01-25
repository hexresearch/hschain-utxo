-- | Types and functions for sigma-protocol.
module Hschain.Utxo.Lang.Sigma.Protocol(
    SigmaE(..)
  , sexprAnn
  , Env(..)
  , AtomicProof(..)
  , ProofInput(..)
  , leafPublicKey
  , simulateAtomicProof
  , verifyAtomicProof
  , responseZ
  , getProofInput
) where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Data
import Data.Aeson   (FromJSON(..), ToJSON(..))
import Data.Either.Extra (eitherToMaybe)
import Data.Bifunctor
import GHC.Generics (Generic)

import HSChain.Crypto.Classes (defaultToJSON, defaultParseJSON)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto

import Hschain.Utxo.Lang.Sigma.DLog
import Hschain.Utxo.Lang.Sigma.DTuple
import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.Types

import qualified Data.ByteString.Lazy as LB
import qualified Codec.Serialise as CBOR
import qualified Language.Haskell.TH.Syntax as TH

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Sigma expression which is tree of conjunctions and disjunctions
--   of primitive predicates: 1) possession of private key, 2) that we
--   know one private key of DH tuple
data SigmaE k a
  = Leaf k a
    -- ^ Primitive predicate
  | AND k [SigmaE k a]
    -- ^ AND connective
  | OR  k [SigmaE k a]
    -- ^ OR connective
  deriving stock    (Functor, Foldable, Traversable, Show, Eq, Generic)
  deriving anyclass (Serialise)

sexprAnn :: SigmaE k a -> k
sexprAnn = \case
  Leaf k _ -> k
  AND  k _ -> k
  OR   k _ -> k

-- | Public proof data (inputs for proof algorithms)
data ProofInput a
  = InputDLog   (PublicKey a)      -- ^ proof of discrte log
  | InputDTuple (DTuple a)  -- ^ proof of Diffie-Hellman tuple
  deriving (Generic)

leafPublicKey :: ProofInput a -> PublicKey a
leafPublicKey (InputDLog   pk) = pk
leafPublicKey (InputDTuple dh) = dtuple'g_y dh


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


-- | Set of known keys
newtype Env a = Env { unEnv :: [KeyPair a] }



----------------------------------------------------------------
-- Instance
----------------------------------------------------------------0

instance Bifunctor SigmaE where
  first f = go where
    go = \case
      Leaf k a  -> Leaf (f k) a
      AND  k es -> AND  (f k) (go <$> es)
      OR   k es -> OR   (f k) (go <$> es)
  second = fmap


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

deriving stock instance (CryptoAsymmetric a) => Show (ProofInput a)
deriving stock instance (EC a)               => Eq   (ProofInput a)
deriving stock instance (EC a)               => Ord  (ProofInput a)
instance (NFData (PublicKey a)) => NFData         (ProofInput a)
instance (CryptoAsymmetric a)   => CBOR.Serialise (ProofInput a)
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

deriving instance (CryptoAsymmetric a, Show (Challenge a)) => Show (AtomicProof a)
deriving instance (EC a) => Eq (AtomicProof a)
instance (EC a) => CBOR.Serialise (AtomicProof a)
