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
  , newSecret
  , getPublicKey
  , getKeyPair
  , toProofEnv
  , equalSigmaExpr
  ) where

import Control.Monad

import Codec.Serialise

import Data.Aeson
import Data.Aeson.Types

import Data.Fix
import Data.Functor.Classes
import Data.Set (Set)
import Data.Text (Text)

import GHC.Generics

import Text.Show.Deriving

import Safe

import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import qualified Hschain.Utxo.Lang.Sigma.Expr as Sigma

import qualified Data.ByteString.Base58 as Base58

type CryptoAlg = Sigma.Ed25519

type KeyPair    = Sigma.KeyPair    CryptoAlg
type PublicKey  = Sigma.PublicKey  CryptoAlg
type Secret     = Sigma.Secret     CryptoAlg

type ProofEnv   = Sigma.Env        CryptoAlg
type Proof      = Sigma.ProofDL    CryptoAlg

newSecret :: IO Secret
newSecret = Sigma.generateSecretKey

getPublicKey :: Secret -> PublicKey
getPublicKey = Sigma.getPublicKey

getKeyPair :: Secret -> KeyPair
getKeyPair secret = Sigma.KeyPair secret (getPublicKey secret)

toProofEnv :: [KeyPair] -> ProofEnv
toProofEnv ks = Sigma.Env ks

publicKeyFromText :: Text -> Maybe PublicKey
publicKeyFromText =
  (either (const Nothing) Just . deserialiseOrFail . LB.fromStrict <=< Base58.decodeBase58 Base58.bitcoinAlphabet) .
  TE.encodeUtf8

publicKeyToText :: PublicKey -> Text
publicKeyToText = TE.decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet . LB.toStrict . serialise

serialiseToJSON :: Serialise a => a -> Value
serialiseToJSON = toJSON . TE.decodeUtf8With TE.lenientDecode . LB.toStrict . serialise

serialiseFromJSON :: Serialise a => Value -> Parser a
serialiseFromJSON = (either (const mzero) pure . deserialiseOrFail . LB.fromStrict . TE.encodeUtf8) <=< parseJSON

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

newProof :: ProofEnv -> Sigma PublicKey -> IO (Sigma Proof)
newProof env = fmap fromSigmaExpr . Sigma.newProof env . toSigmaExpr

verifyProof :: Sigma Proof -> Bool
verifyProof = Sigma.verifyProof . toSigmaExpr

type Sigma k = Fix (SigmaExpr k)

data SigmaExpr k a =
    SigmaPk k
  | SigmaAnd a a -- todo: in low-level impl And Or contain list of values. is it better for performance
                 -- to use lists here too instead of pair of arguments
  | SigmaOr  a a
  deriving (Functor, Foldable, Traversable, Show, Read, Eq, Ord, Generic)

instance Serialise k => Serialise (Sigma k)
instance (Serialise k, Serialise a) => Serialise (SigmaExpr k a)

-- | Not for sigma expressions.
--  Warning: assumption
-- We propose that absense of the key can not be proved.
-- not (pk owner) - evaluates to Fasle for any owner
notSigma :: forall k . Sigma k -> Either Bool (Sigma k)
notSigma = cata $ \case
      SigmaPk  key -> Left False
      SigmaAnd a b -> orTag a b
      SigmaOr  a b -> andTag a b
  where
    orTag x y = case (x, y) of
      (Left a, b)        -> if a then Left True else b
      (a, Left b)        -> if b then Left True else a
      (Right a, Right b) -> Right $ Fix $ SigmaOr a b

    andTag x y = case (x, y) of
      (Left a, b)        -> if a then b else Left False
      (a, Left b)        -> if b then a else Left False
      (Right a, Right b) -> Right $ Fix $ SigmaAnd a b

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
  Sigma.AND _ as -> foldBy SigmaAnd $ fmap rec as
  Sigma.OR _ as  -> foldBy SigmaOr  $ fmap rec as
  where
    rec  = fromSigmaExpr

    foldBy cons as = L.foldl1 (\a b -> Fix $ cons a b) as

toSigmaExpr :: Sigma a -> Sigma.SigmaE () a
toSigmaExpr = cata $ \case
  SigmaPk k    -> Sigma.Leaf () k
  SigmaAnd a b -> Sigma.AND () [a, b]
  SigmaOr  a b -> Sigma.OR  () [a, b]

emptyProofEnv :: ProofEnv
emptyProofEnv = Sigma.Env []

equalSigmaExpr :: Sigma PublicKey -> Sigma Proof -> Bool
equalSigmaExpr (Fix x) (Fix y) = case (x, y) of
  (SigmaPk pubKey, SigmaPk proof)  -> pubKey == Sigma.publicK proof
  (SigmaOr a1 b1, SigmaOr a2 b2)   -> equalSigmaExpr a1 a2 && equalSigmaExpr b1 b2
  (SigmaAnd a1 b1, SigmaAnd a2 b2) -> equalSigmaExpr a1 a2 && equalSigmaExpr b1 b2
  _                                -> False

$(deriveShow1 ''SigmaExpr)

