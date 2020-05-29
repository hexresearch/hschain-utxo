{-# OPTIONS_GHC -Wno-orphans #-}
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
  , Sigma
  , SigmaExpr(..)
  , newProof
  , verifyProof
  , notSigma
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
  ) where

import Hex.Common.Serialise

import Control.Monad
import Control.DeepSeq (NFData)

import Codec.Serialise

import Data.Aeson
import Data.Either
import Data.Fix
import Data.Text (Text)

import GHC.Generics

import Text.Show.Deriving

import qualified Hschain.Utxo.Lang.Sigma.Interpreter           as Sigma
import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve         as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Protocol              as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Types                 as Sigma



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

instance FromJSON Proof where
  parseJSON = serialiseFromJSON

instance FromJSON PublicKey where
  parseJSON = (maybe mzero pure . publicKeyFromText) <=< parseJSON

instance ToJSON Proof where
  toJSON = serialiseToJSON

instance ToJSON PublicKey where
  toJSON = toJSON . publicKeyToText

instance ToJSON (Sigma Proof) where
  toJSON = serialiseToJSON

instance FromJSON (Sigma Proof) where
  parseJSON = serialiseFromJSON

-- | Creates proof for sigma expression with given collection of key-pairs (@ProofEnv@).
newProof :: ProofEnv -> Sigma PublicKey -> IO (Either Text Proof)
newProof env = Sigma.newProof env . toSigmaExpr

-- | Verify the proof.
verifyProof :: Proof -> Bool
verifyProof = Sigma.verifyProof

type Sigma k = Fix (SigmaExpr k)

deriving anyclass instance NFData k => NFData (Sigma k)

-- | Sigma-expression
data SigmaExpr k a =
    SigmaPk k      -- ownership of the key (contains public key)
  | SigmaAnd [a]   -- and-expression
  | SigmaOr  [a]   -- or-expression
  deriving (Functor, Foldable, Traversable, Show, Read, Eq, Ord, Generic, NFData)

instance Serialise k => Serialise (Sigma k)
instance (Serialise k, Serialise a) => Serialise (SigmaExpr k a)

-- | Not for sigma expressions.
--  Warning: assumption
-- We propose that absense of the key can not be proved.
-- not (pk owner) - evaluates to Fasle for any owner
notSigma :: forall k . Sigma k -> Either Bool (Sigma k)
notSigma = cata $ \case
      SigmaPk  _   -> Left False
      SigmaAnd as  -> orTag as
      SigmaOr  as  -> andTag as
  where
    orTag xs
      | or ls     = Left True
      | otherwise = Right $ Fix $ SigmaOr rs
      where
        (ls, rs) = partitionEithers xs

    andTag xs
      | not (and ls) = Left False
      | otherwise    = Right $ Fix $ SigmaAnd rs
      where
        (ls, rs) = partitionEithers xs

-- TODO: make human readable JSON instances for
-- usage with other languages, or provide tools to
-- work with haskell representation.

instance FromJSON (Sigma PublicKey) where
  parseJSON = serialiseFromJSON

instance ToJSON (Sigma PublicKey) where
  toJSON = serialiseToJSON

fromSigmaExpr :: Sigma.SigmaE () a -> Sigma a
fromSigmaExpr = \case
  Sigma.Leaf _ k -> Fix $ SigmaPk k
  Sigma.AND _ as -> Fix $ SigmaAnd $ fmap rec as
  Sigma.OR _ as  -> Fix $ SigmaOr  $ fmap rec as
  where
    rec  = fromSigmaExpr

toSigmaExpr :: Sigma a -> Sigma.SigmaE () a
toSigmaExpr = cata $ \case
  SigmaPk k    -> Sigma.Leaf () k
  SigmaAnd as  -> Sigma.AND () as
  SigmaOr  as  -> Sigma.OR  () as

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
    equalList xs ys = case (xs, ys) of
      ([], []) -> True
      (a:as, b:bs) -> if equalSigmaExpr a b
                        then equalList as bs
                        else False
      _ -> False

$(deriveShow1 ''SigmaExpr)
