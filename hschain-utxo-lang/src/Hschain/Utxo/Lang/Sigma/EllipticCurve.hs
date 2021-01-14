-- | Module defines main types and functions for working with elliptic curves.
module Hschain.Utxo.Lang.Sigma.EllipticCurve(
    EC(..)
  , Ed25519
  , ECPoint
  , ECScalar
  , hashDomain
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class
import Crypto.Error
import Data.Aeson (FromJSON(..),ToJSON(..),FromJSONKey(..),ToJSONKey(..))
import Data.Data
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
import qualified Language.Haskell.TH.Syntax as TH
import Instances.TH.Lift ()
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash


infixl 6 .+.
infixl 7 .*.

type ECPoint  = PublicKey
type ECScalar = PrivKey

-- | Operations with elliptic curve
class ( CryptoAsymmetric a 
      , ByteRepr (Challenge a)
      , Eq (ECScalar a)
      , Eq (Challenge a)
      ) => EC a where
  data Challenge a
  -- Challenge part
  generateChallenge :: IO (Challenge a)
  randomOracle      :: BS.ByteString -> Challenge a
  xorChallenge      :: Challenge a -> Challenge a -> Challenge a
  -- ^ generates random scalar

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


----------------------------------------------------------------
-- Ed25519 elliptic curve
----------------------------------------------------------------

-- | Algorithm tag.
data Ed25519

newtype instance PublicKey Ed25519 = ECPoint25519  Ed.Point
  deriving stock   (Eq,Generic)
  deriving newtype (NFData)

newtype instance PrivKey Ed25519 = ECScalar25519 Ed.Scalar
  deriving stock   (Eq,Generic)
  deriving newtype (NFData)

instance CryptoAsymmetric Ed25519 where
  generatePrivKey = liftIO $ coerce (Ed.scalarGenerate @IO)
  publicKey = coerce Ed.toPoint
  asymmKeyAlgorithmName = CryptoName "ECC.Ed25519"

instance EC Ed25519 where
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

instance ByteReprSized (ECPoint Ed25519) where
  type ByteSize (ECPoint Ed25519) = 32

instance ByteRepr (ECScalar Ed25519) where
  decodeFromBS bs = case Ed.scalarDecodeLong bs of
    CryptoPassed p -> Just $ ECScalar25519 p
    CryptoFailed _ -> Nothing
  encodeToBS (ECScalar25519 p) = Ed.scalarEncode p

instance ByteReprSized (ECScalar Ed25519) where
  -- FIXME: Questionable
  type ByteSize (ECScalar Ed25519) = 32

instance ByteRepr (Challenge Ed25519) where
  decodeFromBS = Just . ChallengeEd25519
  encodeToBS   = coerce

instance CryptoHashable (Challenge Ed25519) where
  hashStep = genericHashStep hashDomain


hashDomain :: String
hashDomain = "hschain.utxo.sigma"

instance Data (PublicKey Ed25519) where
  gfoldl _ _ _ = error       "PublicKey.gfoldl"
  toConstr _   = error       "PublicKey.toConstr"
  gunfold _ _  = error       "PublicKey.gunfold"
  dataTypeOf _ = mkNoRepType "Hschain.Utxo.Lang.Sigma.Types.PublicKey"

instance TH.Lift (PublicKey Ed25519) where
  lift pk = [| let Just k = decodeFromBS bs in k |]
    where
      bs = encodeToBS pk
