-- | Defines basic types for blockchain.
module Hschain.Utxo.Lang.Types where

import Hex.Common.Aeson
import Hex.Common.Serialise
import Control.DeepSeq (NFData)
import Control.Monad

import Codec.Serialise

import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.ByteString (ByteString)
import Data.Int
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Generics

import HSChain.Crypto.Classes (ViaBase58(..), encodeBase58)
import HSChain.Crypto.Classes.Hash (CryptoHashable(..), genericHashStep)
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Sigma.EllipticCurve (hashDomain)
import Hschain.Utxo.Lang.Utils.ByteString

import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as LB

-- | User identifier.
newtype UserId = UserId { unUserId :: Text }
  deriving newtype  (Show, Eq, ToJSON, FromJSON)
  deriving stock    (Generic)

-- | Hash of transaction.
newtype TxHash = TxHash ByteString
  deriving newtype  (Show, Eq, Ord, Serialise)
  deriving stock    (Generic)

instance ToJSON TxHash where
  toJSON = serialiseToJSON

instance FromJSON TxHash where
  parseJSON = serialiseFromJSON

instance ToJSONKey TxHash where
  toJSONKey = ToJSONKeyText serialiseToText (text . serialiseToText)

instance FromJSONKey TxHash where
  fromJSONKey = FromJSONKeyTextParser  (maybe mzero return . serialiseFromText)

-- | Type for transactions.
--
-- Transaction spends values from the input boxes to create output boxes.
-- The result is boolean AND fold over all scripts in the input boxes.
-- The sum of the input values should be equal to the sum of the output values.
--
-- The proof can be missing (Nothing) if we not commiting transaction but
-- send API-query to know the sigma-expression that is going to be result
-- of the calculation of the input scripts within the context of the current blockchain state.
data Tx = Tx
  { tx'inputs  :: !(Vector BoxId)   -- ^ List of identifiers of input boxes in blockchain
  , tx'outputs :: !(Vector Box)     -- ^ List of outputs
  , tx'proof   :: !(Maybe Proof)    -- ^ Proof of the resulting sigma expression
  , tx'args    :: !Args             -- ^ Arguments for the scripts
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)

-- | This is used for hashing the TX, to get it's id and
-- for serialization to get message to be signed for verification.
data TxContent = TxContent
  { txContent'inputs  :: !(Vector BoxId)
  , txContent'outputs :: !(Vector Box)
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)

getTxContent :: Tx -> TxContent
getTxContent Tx{..} = TxContent
  { txContent'inputs  = tx'inputs
  , txContent'outputs = tx'outputs
  }

getTxBytes :: Tx -> ByteString
getTxBytes = getTxContentBytes . getTxContent

getTxContentBytes :: TxContent -> ByteString
getTxContentBytes = LB.toStrict . CBOR.serialise

-- | Tx with substituted inputs and environment.
--  This type is the same as Tx only it contains Boxes for inputs instead
-- of identifiers. Boxes are read from the current blockchain state.
data TxArg = TxArg
  { txArg'inputs   :: !(Vector Box)
  , txArg'outputs  :: !(Vector Box)
  , txArg'proof    :: !(Maybe Proof)
  , txArg'args     :: !Args
  , txArg'env      :: !Env
  , txArg'txBytes  :: !ByteString -- ^ serialised content of TX (it's used to verify the proof)
  }
  deriving (Show, Eq)

-- | Blockchain environment variables.
data Env = Env
  { env'height   :: !Int64    -- ^ blockchain height
  } deriving (Show, Eq)

-- | Transaction environment. All values that user can read
-- from the script
data TxEnv = TxEnv
  { txEnv'height   :: !Int64
  , txEnv'self     :: !Box
  , txEnv'inputs   :: !(Vector Box)
  , txEnv'outputs  :: !(Vector Box)
  , txEnv'args     :: !Args
  }

txPreservesValue :: TxArg -> Bool
txPreservesValue tx@TxArg{..}
  | isStartEpoch tx = True
  | otherwise       = toSum txArg'inputs == toSum txArg'outputs
  where
    toSum xs = getSum $ foldMap (Sum . box'value) xs

isStartEpoch :: TxArg -> Bool
isStartEpoch TxArg{..} = env'height txArg'env == 0
{-
instance FromJSON Script where
  parseJSON = fmap (\(ViaBase58 s :: ViaBase58 "Script" ByteString) -> Script s) . parseJSON

instance ToJSONKey Script where
  toJSONKey = contramapToJSONKeyFunction (ViaBase58 . unScript) toJSONKey

instance FromJSON Args where
  parseJSON = withObject "Args" $ \o -> do
    args'ints  <- o .: "ints"
    args'bools <- o .: "bools"
    args'texts <- o .: "texts"
    bytes      <- o .: "bytes"
    return Args{ args'bytes = coerce (bytes :: Vector (ViaBase58 "" ByteString))
               , ..
               }
instance ToJSON Args where
  toJSON Args{..} = object
    [ "ints"  .= args'ints
    , "bools" .= args'bools
    , "texts" .= args'texts
    , "bytes" .= (coerce args'bytes :: Vector (ViaBase58 "" ByteString))
    ]
-}
instance ToJSON TxArg where
  toJSON TxArg{..} = object
    [ "inputs"   .= txArg'inputs
    , "outputs"  .= txArg'outputs
    , "proof"    .= txArg'proof
    , "args"     .= txArg'args
    , "env"      .= txArg'env
    , "txBytes"  .= ViaBase58 txArg'txBytes
    ]

instance FromJSON TxArg where
  parseJSON = withObject "TxArgs" $ \obj -> do
    txArg'inputs  <- obj .: "inputs"
    txArg'outputs <- obj .: "outputs"
    txArg'proof   <- obj .: "proof"
    txArg'args    <- obj .: "args"
    txArg'env     <- obj .: "env"
    bytes         <- obj .: "txBytes"
    return TxArg { txArg'txBytes = (\(ViaBase58 s :: ViaBase58 "ByteString" ByteString) -> s) bytes
                 , ..
                 }

--------------------------------------------

-- | Claculate the hash of the script.
hashScript :: Script -> ByteString
hashScript = getSha256 . unScript

scriptToText :: Script -> Text
scriptToText = encodeBase58 . unScript

--------------------------------------------
-- JSON instnaces

$(deriveJSON dropPrefixOptions ''Tx)
$(deriveJSON dropPrefixOptions ''Env)

instance CryptoHashable Tx where
  hashStep = genericHashStep hashDomain

