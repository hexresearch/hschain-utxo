module Hschain.Utxo.Lang.Sigma.EllipticCurve(
    EC(..)
  , Ed25519
) where

import Crypto.Error

import Data.Bits
import Data.Coerce
import Data.Function (on)

import qualified Codec.Serialise          as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Crypto.ECC.Edwards25519  as Ed
import qualified Crypto.Hash.Algorithms   as Hash
import qualified Crypto.Hash              as Hash
import qualified Crypto.Random.Types      as RND
import qualified Data.ByteArray           as BA
import qualified Data.ByteString          as BS

-- | Operations with elliptic curve
class EC a where
  data ECPoint   a
  data ECScalar  a
  data Challenge a
  -- Challenge part
  generateChallenge :: IO (Challenge a)
  randomOracle      :: BS.ByteString -> Challenge a
  xorChallenge      :: Challenge a -> Challenge a -> Challenge a

  generateScalar    :: IO (ECScalar a)
  fromGenerator     :: ECScalar  a -> ECPoint  a
  fromChallenge     :: Challenge a -> ECScalar a
  (.+.)   :: ECScalar a -> ECScalar a -> ECScalar a
  (.*.)   :: ECScalar a -> ECScalar a -> ECScalar a

  (.*^)   :: ECScalar a -> ECPoint  a -> ECPoint  a
  -- Group operations
  (^+^)   :: ECPoint  a -> ECPoint  a -> ECPoint  a
  negateP :: ECPoint a -> ECPoint a

data Ed25519

instance EC Ed25519 where
  newtype ECPoint   Ed25519 = ECPoint25519  Ed.Point
  newtype ECScalar  Ed25519 = ECScalar25519 Ed.Scalar
  newtype Challenge Ed25519 = ChallengeEd25519 BS.ByteString
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

instance Ord Ed.Point where
  compare = compare `on` (Ed.pointEncode :: Ed.Point -> BS.ByteString)

instance Ord Ed.Scalar where
  compare = compare `on` (Ed.scalarEncode :: Ed.Scalar -> BS.ByteString)

deriving instance Show (ECPoint   Ed25519)
deriving instance Show (ECScalar  Ed25519)
deriving instance Show (Challenge Ed25519)

deriving instance Eq   (ECPoint   Ed25519)
deriving instance Eq   (ECScalar  Ed25519)
deriving instance Eq   (Challenge Ed25519)

deriving instance Ord  (ECPoint   Ed25519)
deriving instance Ord  (ECScalar  Ed25519)
deriving instance Ord  (Challenge Ed25519)

instance CBOR.Serialise (ECPoint Ed25519) where
  encode = CBOR.encode . id @BS.ByteString . Ed.pointEncode . coerce
  decode = decodeBy (fmap coerce . Ed.pointDecode) =<< CBOR.decode

instance CBOR.Serialise (ECScalar Ed25519) where
  encode = CBOR.encode . id @BS.ByteString . Ed.scalarEncode . coerce
  decode = decodeBy (fmap coerce . Ed.scalarDecodeLong) =<< CBOR.decode

decodeBy :: (BS.ByteString -> CryptoFailable a) -> BS.ByteString -> CBOR.Decoder s a
decodeBy decoder bs = case decoder bs of
  CryptoPassed a   -> return a
  CryptoFailed err -> fail $ show err

instance CBOR.Serialise (Challenge Ed25519) where
  encode (ChallengeEd25519 bs) = CBOR.encode bs
  decode = fmap ChallengeEd25519 CBOR.decode


