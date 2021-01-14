-- | Module defines main types and functions for working with elliptic curves.
module Hschain.Utxo.Lang.Sigma.EllipticCurve(
    EC(..)
  , Ed25519
  , hashDomain
  ) where

import Control.DeepSeq (NFData)

import Crypto.Error
import Data.Aeson (FromJSON(..),ToJSON(..),FromJSONKey(..),ToJSONKey(..))
import Data.Bits
import Data.Coerce
import Data.Function (on)
import Data.Maybe (fromJust)
import HSChain.Crypto.Classes
import GHC.Generics

import qualified Codec.Serialise          as CBOR
import qualified Crypto.ECC.Edwards25519  as Ed
import qualified Crypto.Hash.Algorithms   as Hash
import qualified Crypto.Hash              as Hash
import qualified Crypto.Random.Types      as RND
import qualified Data.ByteArray           as BA
import qualified Data.ByteString          as BS
import HSChain.Crypto.Classes.Hash

infixl 6 .+.
infixl 7 .*.

-- | Operations with elliptic curve
class ( ByteRepr (ECPoint   a)
      , ByteRepr (ECScalar  a)
      , ByteRepr (Challenge a)
      , Eq (ECScalar a)
      , Eq (Challenge a)
      ) => EC a where
  data ECPoint   a
  data ECScalar  a
  data Challenge a
  -- Challenge part
  generateChallenge :: IO (Challenge a)
  randomOracle      :: BS.ByteString -> Challenge a
  xorChallenge      :: Challenge a -> Challenge a -> Challenge a

  generateScalar    :: IO (ECScalar a)
  -- ^ generates random scalar

  fromGenerator     :: ECScalar  a -> ECPoint  a
  -- ^ multiplies generator of the group N times (first argument)

  fromChallenge     :: Challenge a -> ECScalar a

  groupGenerator    :: ECPoint a
  -- ^ generator of the group

  (.+.)   :: ECScalar a -> ECScalar a -> ECScalar a
  (.*.)   :: ECScalar a -> ECScalar a -> ECScalar a

  (.*^)   :: ECScalar a -> ECPoint  a -> ECPoint  a
  -- Group operations
  (^+^)   :: ECPoint  a -> ECPoint  a -> ECPoint  a
  negateP :: ECPoint a -> ECPoint a

instance ByteRepr (ECPoint a) => FromJSON (ECPoint a) where
  parseJSON = defaultParseJSON "ECPoint"
instance ByteRepr (ECPoint a) => ToJSON (ECPoint a) where
  toJSON = defaultToJSON
instance ByteRepr (ECPoint a) => FromJSONKey (ECPoint a) where
  fromJSONKey = defaultFromJsonKey "ECPoint"
instance ByteRepr (ECPoint a) => ToJSONKey   (ECPoint a) where
  toJSONKey = defaultToJsonKey
instance ByteRepr (ECPoint a) => CBOR.Serialise (ECPoint a) where
  encode = defaultCborEncode
  decode = defaultCborDecode "ECPoint"

instance ByteRepr (ECScalar a) => FromJSON (ECScalar a) where
  parseJSON = defaultParseJSON "ECScalar"
instance ByteRepr (ECScalar a) => ToJSON (ECScalar a) where
  toJSON = defaultToJSON
instance ByteRepr (ECScalar a) => FromJSONKey (ECScalar a) where
  fromJSONKey = defaultFromJsonKey "ECScalar"
instance ByteRepr (ECScalar a) => ToJSONKey   (ECScalar a) where
  toJSONKey = defaultToJsonKey
instance ByteRepr (ECScalar a) => CBOR.Serialise (ECScalar a) where
  encode = defaultCborEncode
  decode = defaultCborDecode "ECScalar"

instance ByteRepr (Challenge a) => FromJSON (Challenge a) where
  parseJSON = defaultParseJSON "Challenge"
instance ByteRepr (Challenge a) => ToJSON (Challenge a) where
  toJSON = defaultToJSON
instance ByteRepr (Challenge a) => FromJSONKey (Challenge a) where
  fromJSONKey = defaultFromJsonKey "Challenge"
instance ByteRepr (Challenge a) => ToJSONKey   (Challenge a) where
  toJSONKey = defaultToJsonKey
instance ByteRepr (Challenge a) => CBOR.Serialise (Challenge a) where
  encode = defaultCborEncode
  decode = defaultCborDecode "Challenge"


-- | Algorithm tag.
data Ed25519

instance EC Ed25519 where
  newtype ECPoint   Ed25519 = ECPoint25519  Ed.Point
    deriving stock   (Show,Eq,Generic)
    deriving newtype (NFData)
  newtype ECScalar  Ed25519 = ECScalar25519 Ed.Scalar
    deriving stock   (Show,Eq,Generic)
    deriving newtype (NFData)
  newtype Challenge Ed25519 = ChallengeEd25519 BS.ByteString
    deriving stock   (Show,Eq,Ord,Generic)
    deriving newtype (NFData)
  generateChallenge = ChallengeEd25519 <$> RND.getRandomBytes 31
  randomOracle
    = ChallengeEd25519
    . BS.take 31
    . BA.convert
    . Hash.hash @_ @Hash.SHA256
  xorChallenge (ChallengeEd25519 a) (ChallengeEd25519 b)
    = ChallengeEd25519
    $ BS.pack
    $ BS.zipWith xor a b

  generateScalar    = coerce (Ed.scalarGenerate @IO)
  fromGenerator     = coerce Ed.toPoint
  -- FIXME: We need to maintain that challenge is less than group
  --        module, right?
  fromChallenge (ChallengeEd25519 bs) =
    case Ed.scalarDecodeLong $ BS.take 31 bs of
      CryptoPassed x -> ECScalar25519 x
      CryptoFailed e -> error (show e)
  (.+.)   = coerce Ed.scalarAdd
  (.*.)   = coerce Ed.scalarMul
  (^+^)   = coerce Ed.pointAdd
  (.*^)   = coerce Ed.pointMul
  negateP = coerce Ed.pointNegate

  groupGenerator = ECPoint25519 $ Ed.toPoint $ fromJust $ maybeCryptoError $ Ed.scalarDecodeLong $ (BA.pack [1] :: BA.Bytes)

instance Ord (ECPoint   Ed25519) where
  compare = coerce (compare `on` (Ed.pointEncode :: Ed.Point -> BS.ByteString))
instance Ord (ECScalar  Ed25519) where
  compare = coerce (compare `on` (Ed.scalarEncode :: Ed.Scalar -> BS.ByteString))


instance ByteRepr (ECPoint Ed25519) where
  decodeFromBS bs = case Ed.pointDecode bs of
    CryptoPassed p -> Just $ ECPoint25519 p
    CryptoFailed _ -> Nothing
  encodeToBS (ECPoint25519 p) = Ed.pointEncode p

instance ByteRepr (ECScalar Ed25519) where
  decodeFromBS bs = case Ed.scalarDecodeLong bs of
    CryptoPassed p -> Just $ ECScalar25519 p
    CryptoFailed _ -> Nothing
  encodeToBS (ECScalar25519 p) = Ed.scalarEncode p

instance ByteRepr (Challenge Ed25519) where
  decodeFromBS = Just . ChallengeEd25519
  encodeToBS   = coerce

instance CryptoHashable (Challenge Ed25519) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable (ECScalar Ed25519) where
  hashStep x
    =  hashStep (CryPrivateKey "Ed25519")
    <> hashStep (encodeToBS x)

instance CryptoHashable (ECPoint Ed25519) where
  hashStep x
    =  hashStep (CryPublicKey "Ed25519")
    <> hashStep (encodeToBS x)


hashDomain :: String
hashDomain = "hschain.utxo.sigma"
