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
import qualified Data.Vector as V

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

-- | Input is an unspent Box that exists in blockchain.
-- To spend the input we need to provide right arguments and proof
-- of reulting sigma expression.
data BoxInputRef = BoxInputRef
  { boxInputRef'id    :: BoxId
  , boxInputRef'args  :: Args
  , boxInputRef'proof :: Maybe Proof
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)

appendProofs :: Vector (Maybe Proof) -> Tx -> Tx
appendProofs proofs Tx{..} = Tx
  { tx'inputs  = V.zipWith appendProofToBox proofs tx'inputs
  , tx'outputs = tx'outputs
  }

appendProofToBox :: Maybe Proof -> BoxInputRef -> BoxInputRef
appendProofToBox proof BoxInputRef{..} = BoxInputRef
  { boxInputRef'id    = boxInputRef'id
  , boxInputRef'args  = boxInputRef'args
  , boxInputRef'proof = proof
  }

-- | This is used for hashing the TX, to get it's id and
-- for serialization to get message to be signed for verification.
data TxContent a = TxContent
  { txContent'inputs  :: !(Vector a)
  , txContent'outputs :: !(Vector BoxContent)
  }
  deriving stock    (Show, Eq, Ord, Generic, Functor)
  deriving anyclass (Serialise, NFData)

getTxContent :: Tx -> TxContent BoxInputRef
getTxContent Tx{..} = TxContent
  { txContent'inputs  = tx'inputs
  , txContent'outputs = fmap toBoxContent tx'outputs
  }

toBoxContent :: Box -> BoxContent
toBoxContent Box{..} = BoxContent
  { boxContent'value  = box'value
  , boxContent'script = box'script
  , boxContent'args   = box'args
  }

getTxBytes :: Tx -> ByteString
getTxBytes = getTxContentBytes . getTxContent

getTxContentBytes :: Serialise a => TxContent a -> ByteString
getTxContentBytes = LB.toStrict . CBOR.serialise

getTxId :: Serialise a => TxContent a -> TxId
getTxId = TxId . getSha256 . LB.toStrict . serialise

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
  , boxInput'proof :: !(Maybe Proof)
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

splitInputs :: TxArg -> Vector (Maybe Proof, InputEnv)
splitInputs tx = fmap (\input -> (boxInput'proof input, getInputEnv tx input)) $ txArg'inputs tx

getInputEnv :: TxArg -> BoxInput -> InputEnv
getInputEnv TxArg{..} input = InputEnv
  { inputEnv'self    = boxInput'box input
  , inputEnv'height  = env'height txArg'env
  , inputEnv'inputs  = fmap boxInput'box txArg'inputs
  , inputEnv'outputs = txArg'outputs
  , inputEnv'args    = boxInput'args input
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
---------------------------------------------------------------------
-- smartconstructors to create boxes and transactions

-- | Creates TX and assigns properly all box identifiers.
-- It does not creates the proofs.
newTx :: TxContent BoxInputRef -> Tx
newTx tx = Tx
  { tx'inputs  = txContent'inputs tx
  , tx'outputs = makeOutputs txId $ txContent'outputs tx
  }
  where
    txId = getTxId tx

makeOutputs :: TxId -> Vector BoxContent -> Vector Box
makeOutputs txId outputs = V.imap toBox outputs
  where
    toBox outputIndex box@BoxContent{..} = Box
      { box'id     = boxId
      , box'value  = boxContent'value
      , box'script = boxContent'script
      , box'args   = boxContent'args
      }
      where
        boxId = getBoxToHashId $ BoxToHash
            { boxToHash'origin  = BoxOrigin
                { boxOrigin'outputIndex = fromIntegral outputIndex
                , boxOrigin'txId        = txId
                }
            , boxToHash'content = box
            }

makeInputs :: ProofEnv -> ByteString -> Vector ExpectedBox -> IO (Vector BoxInputRef)
makeInputs proofEnv message expectedInputs = mapM toInput expectedInputs
  where
    toInput ExpectedBox{..} = do
      mProof <- mapM (\sigma -> newProof proofEnv sigma message) expectedBox'sigma
      return $ expectedBox'input { boxInputRef'proof = either (const Nothing) Just =<< mProof }

-- | Expectation of the result of the box.
data ExpectedBox = ExpectedBox
  { expectedBox'sigma   :: Maybe (Sigma PublicKey)
  , expectedBox'input   :: BoxInputRef
  }

-- | If we now the expected sigma expressions for the inputs
-- we can create transaction with all proofs supplied.
--
-- Otherwise we can create TX with empty proof and query the expected results of sigma-expressions
-- over API.
--
-- Note: If it can not produce the proof (user don't have corresponding private key)
-- it produces @Nothing@ in the @boxInputRef'proof@.
newProofTx :: ProofEnv -> TxContent ExpectedBox -> IO Tx
newProofTx proofEnv tx = do
  inputs <- makeInputs proofEnv message $ txContent'inputs tx
  return $ Tx
    { tx'inputs  = inputs
    , tx'outputs = makeOutputs txId $ txContent'outputs tx
    }
  where
    txId    = getTxId txContent
    message = getTxContentBytes txContent
    txContent = fmap expectedBox'input tx

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

instance CryptoHashable Tx where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxInput where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxInputRef where
  hashStep = genericHashStep hashDomain

