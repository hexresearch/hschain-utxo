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
  , serialiseFromJSON
  , serialiseToJSON
  , serialiseFromText
  , serialiseToText
  ) where

import Control.Monad
import Control.DeepSeq (NFData)

import Codec.Serialise

import Data.Aeson
import Data.Aeson.Types
import Data.Either
import Data.Fix
import Data.Functor.Classes
import Data.Set (Set)
import Data.Text (Text)

import GHC.Generics

import Text.Show.Deriving

import Safe

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Base64 as Base64
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import qualified Hschain.Utxo.Lang.Sigma.Interpreter           as Sigma
import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve         as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Protocol              as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Types                 as Sigma


import qualified Data.ByteString.Base58 as Base58

type CryptoAlg = Sigma.Ed25519

type KeyPair    = Sigma.KeyPair    CryptoAlg
type PublicKey  = Sigma.PublicKey  CryptoAlg
type Secret     = Sigma.Secret     CryptoAlg

type ProofEnv   = Sigma.Env        CryptoAlg
type Proof      = Sigma.Proof      CryptoAlg
type ProofDL    = Sigma.ProofDL    CryptoAlg

newSecret :: IO Secret
newSecret = Sigma.generateSecretKey

getPublicKey :: Secret -> PublicKey
getPublicKey = Sigma.getPublicKey

getKeyPair :: Secret -> KeyPair
getKeyPair secret = Sigma.KeyPair secret (getPublicKey secret)

toProofEnv :: [KeyPair] -> ProofEnv
toProofEnv ks = Sigma.Env ks

publicKeyFromText :: Text -> Maybe PublicKey
publicKeyFromText = serialiseFromText

publicKeyToText :: PublicKey -> Text
publicKeyToText = serialiseToText

serialiseToText :: Serialise a => a -> Text
serialiseToText = TE.decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet . LB.toStrict . serialise

serialiseFromText :: Serialise a => Text -> Maybe a
serialiseFromText =
  (either (const Nothing) Just . deserialiseOrFail . LB.fromStrict <=< Base58.decodeBase58 Base58.bitcoinAlphabet) .
  TE.encodeUtf8

serialiseToJSON :: Serialise a => a -> Value
serialiseToJSON = toJSON . TE.decodeUtf8 . Base64.encode . LB.toStrict . serialise

serialiseFromJSON :: Serialise a => Value -> Parser a
serialiseFromJSON =
      (toParser . deserialiseOrFail . LB.fromStrict)
  <=< (toParser . Base64.decode . TE.encodeUtf8)
  <=< parseJSON
  where
    toParser = either (const mzero) pure

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

newProof :: ProofEnv -> Sigma PublicKey -> IO (Either Text Proof)
newProof env = Sigma.newProof env . toSigmaExpr

verifyProof :: Proof -> Bool
verifyProof = Sigma.verifyProof

type Sigma k = Fix (SigmaExpr k)

deriving anyclass instance NFData k => NFData (Sigma k)

data SigmaExpr k a =
    SigmaPk k
  | SigmaAnd [a]
  | SigmaOr  [a]
  deriving (Functor, Foldable, Traversable, Show, Read, Eq, Ord, Generic, NFData)

instance Serialise k => Serialise (Sigma k)
instance (Serialise k, Serialise a) => Serialise (SigmaExpr k a)

-- | Not for sigma expressions.
--  Warning: assumption
-- We propose that absense of the key can not be proved.
-- not (pk owner) - evaluates to Fasle for any owner
notSigma :: forall k . Sigma k -> Either Bool (Sigma k)
notSigma = cata $ \case
      SigmaPk  key -> Left False
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

emptyProofEnv :: ProofEnv
emptyProofEnv = Sigma.Env []

proofEnvFromKeys :: [KeyPair] -> ProofEnv
proofEnvFromKeys = Sigma.Env

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
