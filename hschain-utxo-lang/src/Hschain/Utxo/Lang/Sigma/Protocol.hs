{-# LANGUAGE DeriveLift #-}
-- | Types and functions for sigma-protocol.
module Hschain.Utxo.Lang.Sigma.Protocol(
    -- * Types
    -- ** Σ-expressions AST
    SigmaE(..)
  , sexprAnn
  , mapSigmaE
  , traverseSigmaE
  , Proof(..)
  , ProvenTree(..)
    -- ** Leaves
  , ProofInput(..)
  , CommitmentData(..)
  , CommitedProof(..)
  , PartialProof(..)
  , AtomicProof(..)
    -- ** Possession of private key
    -- $dlog
  , ProofDLog(..)
  , verifyProofDLog
  , simulateProofDLog
    -- ** Diffie-Hellman
    -- $dh-tuple
  , DTuple(..)
  , ProofDTuple(..)
  , verifyProofDTuple
  , simulateProofDTuple
  , getCommitmentDTuple
    -- ** OTHER
  , Env(..)
  , isProvable
  , computeCommitments
  , simulateAtomicProof
  , verifyAtomicProof
  , responseZ
  -- * Classes
  , HasPK(..)
  , HasCommitment(..)
  , HasProofInput(..)
  , HasCommitmentData(..)
  ) where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Data
import Data.Aeson   (FromJSON(..), ToJSON(..))
import Data.Maybe
import Data.Functor.Identity
import Data.Functor.Classes
import Data.Bifunctor
import GHC.Generics (Generic)

import HSChain.Crypto.Classes.Hash
import HSChain.Crypto

import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.Types

import qualified Language.Haskell.TH.Syntax as TH

----------------------------------------------------------------
-- AST
----------------------------------------------------------------

-- | AST for Σ-expressions. This form has annotations for each
--   constructor and polymorphic in types of leaves. It's used for
--   creation of complete proofs, see 'Proof'.
data SigmaE k a
  = Leaf k a
    -- ^ Primitive predicate
  | AND k [SigmaE k a]
    -- ^ AND connective
  | OR  k [SigmaE k a]
    -- ^ OR connective
  deriving stock    (Functor,Foldable,Traversable,Show,Eq,Ord,Generic,Data)
  deriving anyclass (Serialise,NFData,ToJSON,FromJSON)

-- | Get constructor's annotation
sexprAnn :: SigmaE k a -> k
sexprAnn = \case
  Leaf k _ -> k
  AND  k _ -> k
  OR   k _ -> k

-- | Analog of 'traverse' which uses constructor annotation as well.
traverseSigmaE :: Applicative f => (k -> a -> f b) -> SigmaE k a -> f (SigmaE k b)
traverseSigmaE f = go
  where
    go = \case
      Leaf k a  -> Leaf k <$> f k a
      AND  k es -> AND  k <$> traverse go es
      OR   k es -> OR   k <$> traverse go es

mapSigmaE :: (k -> a -> b) -> SigmaE k a -> SigmaE k b
mapSigmaE f = runIdentity . traverseSigmaE (\k a -> Identity $ f k a)


instance Bifunctor SigmaE where
  first f = go where
    go = \case
      Leaf k a  -> Leaf (f k) a
      AND  k es -> AND  (f k) (go <$> es)
      OR   k es -> OR   (f k) (go <$> es)
  second = fmap

instance Eq k =>  Eq1 (SigmaE k) where
  liftEq = liftEq2 (==)
instance Eq2 SigmaE where
  liftEq2 eqK eqA = (===) where
    Leaf k a  === Leaf l b  = eqK k l && eqA a b
    AND  k as === AND  l bs = eqK k l && liftEq (===) as bs
    OR   k as === OR   l bs = eqK k l && liftEq (===) as bs
    _         === _         = False


-- | Proof to reconstruct all challenges from the root challenge.
data Proof a = Proof
  { proof'rootChallenge :: Challenge a   -- ^ root chalenge
  , proof'tree          :: ProvenTree a  -- ^ expression to prove
  } deriving (Generic)

-- | Proof tree. It has same shape as original sigma expression.
data ProvenTree a
  = ProvenLeaf (ECScalar a) (ProofInput a)
  -- ^ Primitive proof. It contains prover's response @z@ and
  --   statement to prove
  | ProvenOr (ProvenTree a) [(Challenge a, ProvenTree a)]
  -- ^ OR of subexpressions. First node gets challenge which is
  --   computed as xor of parent challenge and challenges of rest of
  --   the nodes.
  | ProvenAnd [ProvenTree a]
  -- ^ AND of subexpressions
  deriving (Generic)


instance (EC a) => Serialise (Proof a)
instance (EC a) => FromJSON  (Proof a)
instance (EC a) => ToJSON    (Proof a)

instance ( CryptoHashable (Challenge a)
         , CryptoAsymmetric a
         ) => CryptoHashable (Proof a) where
  hashStep = genericHashStep hashDomain

deriving stock instance (EC a) => Show (Proof a)
deriving stock instance (EC a) => Eq   (Proof a)
deriving anyclass instance (NFData (ECPoint a), NFData (ECScalar a), NFData (Challenge a)) => NFData (Proof a)

instance (EC a) => Serialise (ProvenTree a)
instance (EC a) => FromJSON  (ProvenTree a)
instance (EC a) => ToJSON    (ProvenTree a)

instance ( CryptoHashable (Challenge a)
         , CryptoAsymmetric a
         ) => CryptoHashable (ProvenTree a) where
  hashStep = genericHashStep hashDomain

deriving stock   instance (EC a) => Show (ProvenTree a)
deriving stock   instance (EC a) => Eq   (ProvenTree a)
deriving anyclass instance (NFData (ECPoint a), NFData (ECScalar a), NFData (Challenge a)) => NFData (ProvenTree a)


----------------------------------------------------------------
-- Leaves of expression tree
----------------------------------------------------------------

-- | Statement being proven.
data ProofInput a
  = InputDLog   (PublicKey a)
    -- ^ Proof of possession of discrete logarithm (private key) of
    --   a given point on a curve (public key)
  | InputDTuple (DTuple a)
    -- ^ Proof of possession of one secret key used to generate a
    --   Diffie-Hellman tuple.
  deriving (Generic)

-- | Prover's commitment.
data CommitmentData a
  = CommitmentDL (Commitment a)
  | CommitmentDT (Commitment a, Commitment a)
  deriving (Generic)

-- | Primitive statement and prover's commitment. Different statements
--   require different proof.
data CommitedProof a
  = CommitedDLog   (PublicKey a) (Commitment a)
  | CommitedDTuple (DTuple a)    (Commitment a, Commitment a)
  deriving stock (Generic)

-- | Partial proof of Σ-expression leaf. It stores prover's secret @r@
--   and is used when expression is constructed locally.
data PartialProof a = PartialProof
  { pproofInput :: CommitedProof a -- ^ Statement being proven
  , pproofR     :: ECScalar      a -- ^ Prover's secret @r@
  }
  deriving (Generic)

-- | Proof of one of two methods of key verification
data AtomicProof a
  = ProofDL (ProofDLog   a)  -- ^ proof of knowledge of discrete log
  | ProofDT (ProofDTuple a)  -- ^ proof of knowledge of Diffie-Helman tuple
  deriving (Generic)



----------------------------------------------------------------
-- API
----------------------------------------------------------------

computeCommitments :: EC a => ECScalar a -> ProofInput a -> CommitedProof a
computeCommitments r = \case
  InputDLog   pk -> CommitedDLog   pk (publicKey r)
  InputDTuple dh -> CommitedDTuple dh ( r .*^ dtuple'g   dh
                                      , r .*^ dtuple'g_x dh
                                      )

responseZ :: AtomicProof a -> Response a
responseZ = \case
  ProofDL ProofDLog{..}   -> proofDLog'responseZ
  ProofDT ProofDTuple{..} -> proofDTuple'responseZ

-- | Simulate proof of posession of discrete logarithm for given
-- challenge
simulateAtomicProof :: EC a => ProofInput a -> Challenge a -> IO (AtomicProof a)
simulateAtomicProof inp e = case inp of
  InputDLog   dlog   -> ProofDL <$> simulateProofDLog dlog e
  InputDTuple dtuple -> ProofDT <$> simulateProofDTuple dtuple e

verifyAtomicProof :: (EC a) => AtomicProof a -> Bool
verifyAtomicProof = \case
  ProofDL dlog   -> verifyProofDLog dlog
  ProofDT dtuple -> verifyProofDTuple dtuple



-- | Set of known keys
newtype Env a = Env { lookupSecret :: PublicKey a -> Maybe (PrivKey a) }

isProvable :: Env a -> PublicKey a -> Bool
isProvable env = isJust . lookupSecret env

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

deriving stock instance (EC a)  => Show      (ProofInput a)
deriving stock instance (EC a)  => Eq        (ProofInput a)
deriving stock instance (EC a)  => Ord       (ProofInput a)
instance (NFData (PublicKey a)) => NFData    (ProofInput a)
instance (CryptoAsymmetric a)   => Serialise (ProofInput a)
instance (CryptoAsymmetric a)   => ToJSON    (ProofInput a)
instance (CryptoAsymmetric a)   => FromJSON  (ProofInput a)

instance (CryptoAsymmetric a) => CryptoHashable (ProofInput a) where
  hashStep = genericHashStep hashDomain

instance Typeable a => Data (ProofInput a) where
  gfoldl _ _ _ = error       "ProofInput.gfoldl"
  toConstr _   = error       "ProofInput.toConstr"
  gunfold _ _  = error       "ProofInput.gunfold"
  dataTypeOf _ = mkNoRepType "Hschain.Utxo.Lang.Sigma.Types.ProofInput"

deriving instance TH.Lift (PublicKey a) => TH.Lift (ProofInput a)

deriving stock instance (EC a)  => Show      (CommitmentData a)
deriving stock instance (EC a)  => Eq        (CommitmentData a)
instance (NFData (PublicKey a)) => NFData    (CommitmentData a)
instance (CryptoAsymmetric a)   => Serialise (CommitmentData a)
instance (CryptoAsymmetric a)   => ToJSON    (CommitmentData a)
instance (CryptoAsymmetric a)   => FromJSON  (CommitmentData a)

deriving stock instance (EC a)  => Show      (CommitedProof a)
deriving stock instance (EC a)  => Eq        (CommitedProof a)
instance (NFData (PublicKey a)) => NFData    (CommitedProof a)
instance (CryptoAsymmetric a)   => Serialise (CommitedProof a)
instance (CryptoAsymmetric a)   => ToJSON    (CommitedProof a)
instance (CryptoAsymmetric a)   => FromJSON  (CommitedProof a)

deriving stock instance (EC a)  => Show (AtomicProof a)
deriving stock instance (EC a)  => Eq   (AtomicProof a)
instance (NFData (PublicKey a), NFData (Challenge a), NFData (PrivKey a)) => NFData (AtomicProof a)
instance (EC a) => Serialise (AtomicProof a)
instance (EC a) => ToJSON    (AtomicProof a)
instance (EC a) => FromJSON  (AtomicProof a)

deriving stock instance (EC a)  => Show (PartialProof a)
deriving stock instance (EC a)  => Eq   (PartialProof a)
instance (NFData (PublicKey a), NFData (PrivKey a)) => NFData (PartialProof a)
instance (EC a) => Serialise (PartialProof a)
instance (EC a) => ToJSON    (PartialProof a)
instance (EC a) => FromJSON  (PartialProof a)



----------------------------------------------------------------
-- Discrete log
----------------------------------------------------------------

-- $dlog
--
-- Proof of possession of discrete logarithm. This is non-interactive
-- proof that spender know scalar (private key) @x@ such that @g^x =
-- k@ where k is point on curve (public key), and @g@ is generator of
-- curve.
--
-- This is derived from Schnorr signatures. Verification algorithm
-- proceeds as follows:
--
-- 1. Prover chooses random @r : ℤ/q@, keeps it secret and sends
--    commitment @a = g^r@ to verifier
--
-- 2. Verifier chooses random challenge $c : ℤ/q@, such @c@ is
--    generated using hash of commitment and message.
--
-- 3. Prover send @z = r + c·x : ℤ/q@ and sends it to verifier. Proof is
--    accepted if @g^z = a·k^c@.

-- | Proof of possession of discrete logarithm (private key) of given
--   point on curve (public key).
data ProofDLog a = ProofDLog
  { proofDLog'public      :: PublicKey a
    -- ^ Public key
  , proofDLog'commitmentA :: Commitment a
    -- ^ Commitment of prover
  , proofDLog'challengeE  :: Challenge  a
    -- ^ Challenge that generated noninteractively
  , proofDLog'responseZ   :: Response   a
    -- ^ Response of prover
  } deriving (Generic)

-- | Simulate proof of posession of discrete logarithm for given
--   challenge. It's possible to generate valid proof for any given
--   pair of challenge @c@ and response @z@ by computing:
--
--   > a = g^zk^{-c}
simulateProofDLog :: EC a => PublicKey a -> Challenge a -> IO (ProofDLog a)
simulateProofDLog pubK e = do
  z <- generatePrivKey
  return ProofDLog
    { proofDLog'public      = pubK
    , proofDLog'commitmentA = getCommitment z e pubK
    , proofDLog'responseZ   = z
    , proofDLog'challengeE  = e
    }

verifyProofDLog :: (EC a) => ProofDLog a -> Bool
verifyProofDLog ProofDLog{..}
  =  publicKey proofDLog'responseZ
  == (proofDLog'commitmentA ^+^ (fromChallenge proofDLog'challengeE .*^ proofDLog'public))


deriving instance (EC a) => Show (ProofDLog a)
deriving instance (EC a) => Eq   (ProofDLog a)
instance (NFData (PublicKey a), NFData (Challenge a), NFData (PrivKey a)) => NFData (ProofDLog a)
instance (EC a) => Serialise (ProofDLog a)
instance (EC a) => FromJSON  (ProofDLog a)
instance (EC a) => ToJSON    (ProofDLog a)

----------------------------------------------------------------
-- Diffie-Hellman tuples
----------------------------------------------------------------

-- $dh-tuple
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
  ( toCommitment z ch dtuple'g   dtuple'g_y
  , toCommitment z ch dtuple'g_x dtuple'g_xy
  )

toCommitment :: EC a => Response a -> Challenge a -> ECPoint a -> PublicKey a -> Commitment a
toCommitment z ch gen pk = (z .*^ gen)  ^+^ negateP (fromChallenge ch .*^ pk)

-- | Simulate proof of posession of discrete logarithm for given
-- challenge
simulateProofDTuple :: EC a => DTuple a -> Challenge a -> IO (ProofDTuple a)
simulateProofDTuple dt e = do
  z <- generatePrivKey
  return ProofDTuple
    { proofDTuple'public      = dt
    , proofDTuple'commitmentA = getCommitmentDTuple z e dt
    , proofDTuple'responseZ   = z
    , proofDTuple'challengeE  = e
    }


deriving instance (EC a) => Show (DTuple a)
deriving instance (EC a) => Eq   (DTuple a)
deriving instance (EC a) => Ord   (DTuple a)
deriving instance NFData (ECPoint a)  => NFData (DTuple a)
instance (CryptoAsymmetric a) => Serialise (DTuple a)
instance (CryptoAsymmetric a) => ToJSON    (DTuple a)
instance (CryptoAsymmetric a) => FromJSON  (DTuple a)

instance CryptoHashable (ECPoint a) => CryptoHashable (DTuple a) where
  hashStep = genericHashStep hashDomain


deriving instance (EC a) => Show (ProofDTuple a)
deriving instance (EC a) => Eq   (ProofDTuple a)
instance (NFData (PublicKey a), NFData (Challenge a), NFData (PrivKey a)) => NFData (ProofDTuple a)
instance (EC a) => Serialise (ProofDTuple a)
instance (EC a) => FromJSON  (ProofDTuple a)
instance (EC a) => ToJSON    (ProofDTuple a)

deriving instance TH.Lift (PublicKey a) => TH.Lift (DTuple a)


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

class HasPK f where
  getPK :: f a -> PublicKey a

instance HasPK PublicKey    where getPK = id
instance HasPK DTuple       where getPK = dtuple'g_y
instance HasPK ProofDLog    where getPK = proofDLog'public
instance HasPK ProofDTuple  where getPK = getPK . proofDTuple'public
instance HasPK PartialProof where getPK = getPK . pproofInput
instance HasPK ProofInput where
  getPK = \case InputDLog   p -> p
                InputDTuple p -> getPK p
instance HasPK AtomicProof where
  getPK = \case ProofDL p -> getPK p
                ProofDT p -> getPK p

instance HasPK CommitedProof where
  getPK = \case CommitedDLog   k _ -> k
                CommitedDTuple p _ -> getPK p


class HasProofInput f where
  toProofInput :: f a -> ProofInput a

instance HasProofInput ProofInput where toProofInput = id
instance HasProofInput CommitedProof where
  toProofInput = \case CommitedDLog   p _ -> InputDLog   p
                       CommitedDTuple p _ -> InputDTuple p
instance HasProofInput PartialProof where
  toProofInput = toProofInput . pproofInput
instance HasProofInput AtomicProof where
  toProofInput = \case
    ProofDL ProofDLog{..}   -> InputDLog   proofDLog'public
    ProofDT ProofDTuple{..} -> InputDTuple proofDTuple'public


class HasCommitment f where
  toCommitedProof :: f a -> CommitedProof a

instance HasCommitment CommitedProof where
  toCommitedProof = id
instance HasCommitment AtomicProof where
  toCommitedProof = \case
    ProofDL ProofDLog{..}   -> CommitedDLog   proofDLog'public   proofDLog'commitmentA
    ProofDT ProofDTuple{..} -> CommitedDTuple proofDTuple'public proofDTuple'commitmentA
instance HasCommitment PartialProof where
  toCommitedProof = pproofInput


class HasCommitmentData f where
  toCommitedData :: f a -> CommitmentData a

instance HasCommitmentData CommitmentData where
  toCommitedData = id
instance HasCommitmentData CommitedProof where
  toCommitedData = \case
    CommitedDLog   _ a -> CommitmentDL a
    CommitedDTuple _ t -> CommitmentDT t
instance HasCommitmentData AtomicProof where
  toCommitedData = \case
    ProofDL ProofDLog{..}   -> CommitmentDL proofDLog'commitmentA
    ProofDT ProofDTuple{..} -> CommitmentDT proofDTuple'commitmentA
instance HasCommitmentData PartialProof where
  toCommitedData = toCommitedData . pproofInput
