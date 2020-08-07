-- | It defines types and functions for Sigma-expressions.
-- Sigma-expressions are used to sign scripts without providing
-- the information on who signed the script.
--
-- It is the way to prove ownership of the private-key.
module Hschain.Utxo.Lang.Sigma(
    CryptoAlg
  , KeyPair
  , PublicKey
  , Secret
  , ProofEnv
  , Proof
  , SignMessage(..)
  , Sigma
  , SigmaF(..)
  , newProof
  , verifyProof
  , publicKeyFromText
  , publicKeyToText
  , emptyProofEnv
  , proofEnvFromKeys
  , newSecret
  , getPublicKey
  , getKeyPair
  , toProofEnv
  , equalSigmaExpr
  , equalSigmaProof
  , eliminateSigmaBool
  ) where

import Hex.Common.Serialise

import Control.DeepSeq (NFData)
import Codec.Serialise

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Either
import Data.Fix
import Data.Functor.Classes (Eq1(..))
import Data.Text (Text)

import GHC.Generics

import Text.Show.Deriving

import HSChain.Crypto.Classes      (ViaBase58(..))
import HSChain.Crypto.Classes.Hash (CryptoHashable(..), genericHashStep)
import qualified Hschain.Utxo.Lang.Sigma.Interpreter           as Sigma
import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve         as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Protocol              as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Types                 as Sigma

newtype SignMessage = SignMessage { unSignMessage :: ByteString }
  deriving newtype  (Show, Eq, Ord, NFData)
  deriving stock    (Generic)
  deriving anyclass (Serialise)
  deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via (ViaBase58 "BoxId" ByteString)

-- | Cryptographic algorithm that we use.
type CryptoAlg = Sigma.Ed25519

-- | Pair of public and private keys.
type KeyPair    = Sigma.KeyPair    CryptoAlg

-- | Public key.
type PublicKey  = Sigma.PublicKey  CryptoAlg

-- | Private key.
type Secret     = Sigma.Secret     CryptoAlg

-- | Environment to prove the ownership. It is a list of key-pairs
-- that are owned by the prover.
type ProofEnv   = Sigma.Env        CryptoAlg

-- | Proof of the ownership.
type Proof      = Sigma.Proof      CryptoAlg
type ProofDL    = Sigma.ProofDL    CryptoAlg

-- | Generate new private key.
newSecret :: IO Secret
newSecret = Sigma.generateSecretKey

-- | Convert private key to public key.
getPublicKey :: Secret -> PublicKey
getPublicKey = Sigma.getPublicKey

-- | Creates key-pair for given private key.
getKeyPair :: Secret -> KeyPair
getKeyPair secret = Sigma.KeyPair secret (getPublicKey secret)

-- | Proof environment is a listavailable key-pairs.
toProofEnv :: [KeyPair] -> ProofEnv
toProofEnv ks = Sigma.Env ks

-- | Parse public key from text.
publicKeyFromText :: Text -> Maybe PublicKey
publicKeyFromText = serialiseFromText

-- | Convert public key to text.
publicKeyToText :: PublicKey -> Text
publicKeyToText = serialiseToText

instance Serialise a => ToJSON (Sigma a) where
  toJSON = serialiseToJSON

instance Serialise a => FromJSON (Sigma a) where
  parseJSON = serialiseFromJSON

-- | Creates proof for sigma expression with given collection of key-pairs (@ProofEnv@).
-- The last argument message is a serialised content of transaction.
-- It's message to be signed.
--
-- For the message use getTxBytes from TX that has no proof.
newProof :: ProofEnv -> Sigma PublicKey -> SignMessage -> IO (Either Text Proof)
newProof env expr (SignMessage message) =
  case toSigmaExpr expr of
    Right sigma -> Sigma.newProof env sigma message
    Left  _     -> return catchBoolean
  where
    catchBoolean = Left "Expression is constant boolean. It is not  a sigma-expression"

-- | Verify the proof.
--
-- > verifyProof proof message
--
-- For the message use getTxBytes from TX.
verifyProof :: Proof -> SignMessage -> Bool
verifyProof proof (SignMessage msg) = Sigma.verifyProof proof msg

type Sigma k = Fix (SigmaF k)

deriving anyclass instance NFData k => NFData (Sigma k)

-- | Sigma-expression
data SigmaF k a =
    SigmaPk k      -- ownership of the key (contains public key)
  | SigmaAnd [a]   -- and-expression
  | SigmaOr  [a]   -- or-expression
  | SigmaBool Bool -- wraps boolean constants
  deriving (Functor, Foldable, Traversable, Show, Read, Eq, Ord, Generic, NFData)

instance Serialise k => Serialise (Sigma k)
instance (Serialise k, Serialise a) => Serialise (SigmaF k a)

instance (CryptoHashable k, CryptoHashable a) => CryptoHashable (SigmaF k a) where
  hashStep = genericHashStep Sigma.hashDomain

fromSigmaExpr :: Sigma.SigmaE () a -> Sigma a
fromSigmaExpr = \case
  Sigma.Leaf _ k -> Fix $ SigmaPk k
  Sigma.AND _ as -> Fix $ SigmaAnd $ fmap rec as
  Sigma.OR _ as  -> Fix $ SigmaOr  $ fmap rec as
  where
    rec  = fromSigmaExpr

-- | Tries to remove all boolean constants.
-- returns Left boolean if it's not possible
-- to eliminate boolean constants.
eliminateSigmaBool :: Sigma a -> Either Bool (Sigma a)
eliminateSigmaBool = cata $ \case
  SigmaBool b -> Left b
  SigmaPk pk  -> Right $ Fix $ SigmaPk pk
  SigmaAnd as ->
    let (bools, sigmas) = partitionEithers as
        boolRes = and bools
    in  if boolRes
          then
              case sigmas of
                []      -> Left True
                [sigma] -> Right sigma
                _       -> Right $ Fix $ SigmaAnd sigmas
          else Left False
  SigmaOr as ->
    let (bools, sigmas) = partitionEithers as
        boolRes = or bools
    in  if boolRes
          then Left True
          else
               case sigmas of
                 []      -> Left False
                 [sigma] -> Right sigma
                 _       -> Right $ Fix $ SigmaOr sigmas

toSigmaExpr :: Sigma a -> Either Bool (Sigma.SigmaE () a)
toSigmaExpr a = (maybe (Left False) Right . toPrimSigmaExpr) =<< eliminateSigmaBool a

toPrimSigmaExpr :: Sigma a -> Maybe (Sigma.SigmaE () a)
toPrimSigmaExpr = cata $ \case
  SigmaPk k    -> Just $ Sigma.Leaf () k
  SigmaAnd as  -> fmap (Sigma.AND ()) $ sequence as
  SigmaOr  as  -> fmap (Sigma.OR  ()) $ sequence as
  SigmaBool _  -> Nothing

-- | Empty proof environment. It holds no keys.
emptyProofEnv :: ProofEnv
emptyProofEnv = Sigma.Env []

-- | Wrapper to contruct proof environment from list of key-pairs.
proofEnvFromKeys :: [KeyPair] -> ProofEnv
proofEnvFromKeys = Sigma.Env

-- | Check if sigma expression is proven with given proof.
equalSigmaProof :: Sigma PublicKey -> Proof -> Bool
equalSigmaProof candidate proof =
  equalSigmaExpr
      candidate
      (fromSigmaExpr $ Sigma.completeProvenTree proof)

equalSigmaExpr :: Sigma PublicKey -> Sigma ProofDL -> Bool
equalSigmaExpr (Fix x) (Fix y) = case (x, y) of
  (SigmaPk pubKey, SigmaPk proof)  -> pubKey == Sigma.publicK proof
  (SigmaOr as, SigmaOr bs)         -> equalList as bs
  (SigmaAnd as, SigmaAnd bs)       -> equalList as bs
  _                                -> False
  where
    equalList = liftEq equalSigmaExpr

$(deriveShow1 ''SigmaF)
