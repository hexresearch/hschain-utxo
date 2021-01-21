-- |
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
module Hschain.Utxo.Lang.Sigma.DLog(
    ProofDLog(..)
  , verifyProofDLog
  , simulateProofDLog
  ) where

import GHC.Generics (Generic)

import HSChain.Crypto
import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.Types

import qualified Codec.Serialise as CBOR

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

