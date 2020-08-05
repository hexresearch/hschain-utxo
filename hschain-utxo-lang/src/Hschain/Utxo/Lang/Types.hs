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
  { tx'inputs  :: !(Vector BoxInputRef)   -- ^ List of inputs
  , tx'outputs :: !(Vector Box)           -- ^ List of outputs
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)

-- | Pre-TX is used to query the sigma expressions that TX-inputs are going to be evaluated to
-- without proving them
data PreTx = PreTx
  { preTx'inputs  :: !(Vector PreBoxInputRef) -- ^ List of inputs
  , preTx'outputs :: !(Vector Box)
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)

-- | Input is an unspent Box that exists in blockchain.
-- To spend the input we need to provide right arguments and proof
-- of reulting sigma expression.
data BoxInputRef = BoxInputRef
  { boxInputRef'id    :: BoxId
  , boxInputRef'args  :: Args
  , boxInputRef'proof :: Proof
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)

data PreBoxInputRef = PreBoxInputRef
  { preBoxInputRef'id   :: BoxId
  , preBoxInputRef'args :: Args
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
  { txContent'inputs  = fmap boxInputRef'id tx'inputs
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
  { txArg'inputs   :: !(Vector BoxInput)
  , txArg'outputs  :: !(Vector Box)
  , txArg'env      :: !Env
  , txArg'txBytes  :: !ByteString -- ^ serialised content of TX (it's used to verify the proof)
  }
  deriving (Show, Eq)

data BoxInput = BoxInput
  { boxInput'box   :: !Box
  , boxInput'args  :: !Args
  , boxInput'proof :: !Proof
  }
  deriving (Show, Eq, Generic)

-- | Blockchain environment variables.
data Env = Env
  { env'height   :: !Int64    -- ^ blockchain height
  } deriving (Show, Eq)

-- | Input environment contains all data that have to be used
-- during execution of the script.
data InputEnv = InputEnv
  { inputEnv'height  :: !Int64
  , inputEnv'self    :: !Box
  , inputEnv'inputs  :: !(Vector Box)
  , inputEnv'outputs :: !(Vector Box)
  , inputEnv'args    :: !Args
  }

txPreservesValue :: TxArg -> Bool
txPreservesValue tx@TxArg{..}
  | isStartEpoch tx = True
  | otherwise       = toSum (fmap boxInput'box txArg'inputs) == toSum txArg'outputs
  where
    toSum xs = getSum $ foldMap (Sum . box'value) xs

isStartEpoch :: TxArg -> Bool
isStartEpoch TxArg{..} = env'height txArg'env == 0

instance ToJSON TxArg where
  toJSON TxArg{..} = object
    [ "inputs"   .= txArg'inputs
    , "outputs"  .= txArg'outputs
    , "env"      .= txArg'env
    , "txBytes"  .= ViaBase58 txArg'txBytes
    ]

instance FromJSON TxArg where
  parseJSON = withObject "TxArgs" $ \obj -> do
    txArg'inputs  <- obj .: "inputs"
    txArg'outputs <- obj .: "outputs"
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
$(deriveJSON dropPrefixOptions ''BoxInput)
$(deriveJSON dropPrefixOptions ''BoxInputRef)
$(deriveJSON dropPrefixOptions ''PreBoxInputRef)

instance CryptoHashable Tx where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxInput where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxInputRef where
  hashStep = genericHashStep hashDomain

