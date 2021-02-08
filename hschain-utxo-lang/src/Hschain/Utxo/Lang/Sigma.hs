-- | It defines types and functions for Sigma-expressions.
-- Sigma-expressions are used to sign scripts without providing
-- the information on who signed the script.
--
-- It is the way to prove ownership of the private-key.
module Hschain.Utxo.Lang.Sigma(
    CryptoAlg
  , KeyPair
  , PublicKey
  , ECPoint
  , Secret
  , ProofEnv
  , ProofInput
  , Proof
  , AtomicProof
  , SigMessage(..)
  , Sigma
  , Sigma.SigmaE(..)
  , sigmaPk
  , dlogSigma
  , dtupleSigma
  , dlogInput
  , dtupleInput
  , newProof
  , Sigma.verifyProof
  , Sigma.verifyProofExpr
  , newSecret
  , newKeyPair
  , getSecretKey
  , getPublicKey
  , toPublicKey
  , getKeyPair
  , toProofEnv
  , eliminateSigmaBool
    -- * Distributed proof
    -- ** Main prover
  , Partial
  , DuringCommitment
  , DuringChallenge
  , Sigma.mainStartProof
  , Sigma.mainProcessCommitment
  , Sigma.mainAdvanceToChallenge
  , Sigma.mainProcessChallengeResponse
  , Sigma.mainAdvanceToProof
    -- ** Subordinate prover
  , SubordinateProof
  , Sigma.proverGenerateCommitment
  , Sigma.proverProcessChallenge
  ) where

import Control.DeepSeq (NFData)
import Codec.Serialise

import Data.Aeson
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable
import Data.Either
import Data.Text (Text)

import GHC.Generics (Generic)

import HSChain.Crypto.Classes      (ViaBase58(..), ByteRepr(..))
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA          (SHA256)
import qualified HSChain.Crypto as Crypto
import qualified Hschain.Utxo.Lang.Sigma.Interpreter           as Sigma
import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve         as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Protocol              as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Types                 as Sigma


-- | Cryptographic algorithm that we use.
type CryptoAlg = Sigma.Ed25519

-- | Pair of public and private keys.
type KeyPair    = Sigma.KeyPair    CryptoAlg

-- | Public key.
type PublicKey  = Crypto.PublicKey  CryptoAlg

-- | Private key.
type Secret     = Crypto.PrivKey    CryptoAlg

-- | Environment to prove the ownership. It is a list of key-pairs
-- that are owned by the prover.
type ProofEnv   = Sigma.Env        CryptoAlg

type ECPoint    = Sigma.ECPoint    CryptoAlg

-- | Proof of the ownership.
type ProofInput  = Sigma.ProofInput   CryptoAlg
type Proof       = Sigma.Proof        CryptoAlg
type AtomicProof = Sigma.AtomicProof  CryptoAlg

-- | Message for signature it is computed based on Tx.
newtype SigMessage = SigMessage (Hash SHA256)
  deriving newtype  (Show, Eq, Ord, NFData, ByteRepr, CryptoHashable)
  deriving stock    (Generic)
  deriving anyclass (Serialise)
  deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via (ViaBase58 "SigMessage" ByteString)

type Partial f = Sigma.Partial f CryptoAlg
type DuringCommitment = Sigma.DuringCommitment CryptoAlg
type DuringChallenge  = Sigma.DuringChallenge  CryptoAlg
type SubordinateProof = Sigma.SubordinateProof CryptoAlg

-- | Generate new private key.
newSecret :: IO Secret
newSecret = Crypto.generatePrivKey

-- | Convert private key to public key.
toPublicKey :: Secret -> PublicKey
toPublicKey = Crypto.publicKey

-- | Creates key-pair for given private key.
getKeyPair :: Secret -> KeyPair
getKeyPair secret = Sigma.KeyPair secret (toPublicKey secret)

newKeyPair :: IO KeyPair
newKeyPair = fmap getKeyPair newSecret

getSecretKey :: KeyPair -> Secret
getSecretKey = Sigma.getSecretKey

getPublicKey :: KeyPair -> PublicKey
getPublicKey = Sigma.getPublicKey

-- | Proof environment is a listavailable key-pairs.
toProofEnv :: [KeyPair] -> ProofEnv
toProofEnv keys = Sigma.Env $ \k ->
  case [ sk | Sigma.KeyPair sk pk <- keys, k == pk ] of
    []   -> Nothing
    sk:_ -> Just sk

-- | Creates proof for sigma expression with given collection of key-pairs (@ProofEnv@).
-- The last argument message is a serialised content of transaction.
-- It's message to be signed.
--
-- For the message use getTxBytes from TX that has no proof.
newProof :: ProofEnv -> Sigma.SigmaE () ProofInput -> SigMessage -> IO (Either Text Proof)
newProof env expr message = Sigma.createProof env expr $ encodeToBS message

type Sigma k = Sigma.SigmaE () (Either Bool k)

sigmaPk :: k -> Sigma k
sigmaPk k = Sigma.Leaf () (Right k)

dlogSigma :: PublicKey -> Sigma.SigmaE () ProofInput
dlogSigma k = Sigma.Leaf () $ dlogInput k

dtupleSigma :: ECPoint -> PublicKey -> PublicKey -> Sigma.SigmaE () ProofInput
dtupleSigma genB keyA keyB = Sigma.Leaf () $ dtupleInput genB keyA keyB

dlogInput :: PublicKey -> ProofInput
dlogInput = Sigma.InputDLog

dtupleInput :: ECPoint -> PublicKey -> PublicKey -> ProofInput
dtupleInput genB keyA keyB =
  Sigma.InputDTuple $ Sigma.DTuple Sigma.groupGenerator genB keyA keyB


-- | Tries to remove all boolean constants.
-- returns Left boolean if it's not possible
-- to eliminate boolean constants.
eliminateSigmaBool :: Sigma.SigmaE () (Either Bool a) -> Either Bool (Sigma.SigmaE () a)
eliminateSigmaBool = go
  where
    go = \case
      Sigma.Leaf _ (Left  b) -> Left b
      Sigma.Leaf _ (Right a) -> Right $ Sigma.Leaf () a
      Sigma.AND _ as
        | and bools -> case sigmas of
           []       -> Left True
           [sigma]  -> Right sigma
           (s:ss)   -> Right $ Sigma.AND () (s :| ss)
        | otherwise -> Left False
        where
          (bools, sigmas) = partitionEithers $ eliminateSigmaBool <$> toList as
      Sigma.OR () as
        | or bools  -> Left True
        | otherwise -> case sigmas of
            []      -> Left False
            [sigma] -> Right sigma
            (s:ss)  -> Right $ Sigma.OR () (s :| ss)
        where
          (bools, sigmas) = partitionEithers $ eliminateSigmaBool <$> toList as
