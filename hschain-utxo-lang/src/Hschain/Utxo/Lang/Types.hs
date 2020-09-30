-- | Defines basic types for blockchain.
module Hschain.Utxo.Lang.Types
  ( Tx
  , GTx(..)
  , TxHash(..)
  , TxArg(..)
  , BoxInput(..)
  , BoxInputRef(..)
  , ExpectedBox
  , Env(..)
  , Money
  , InputEnv(..)
  , TxId(..)
  , Script(..)
  , Args(..)
  , ArgType(..)
  , Box(..)
  , BoxId(..)
  , PreBox(..)
  , BoxOrigin(..)
  , computeBoxId
  , argTypes
    -- * Functions
  , newTx
  , newProofTx
  , newProofTxOrFail
  , hashScript
  , splitInputs
  , txPreservesValue
  , computeTxId
  , validateOutputBoxIds
    -- * Helperes
  , singleOwnerInput
  ) where

import Hex.Common.Aeson
import Hex.Common.Text
import Control.DeepSeq (NFData)
import Control.Monad.Except

import Codec.Serialise
import Data.Aeson      ((.=),(.:),object,withObject)
import Data.ByteString (ByteString)
import Data.Bifunctor
import Data.Coerce
import Data.Fix
import Data.Int
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))
import GHC.Generics

import HSChain.Crypto.Classes      (ViaBase58(..), ByteRepr, decodeBase58, encodeBase58)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA          (SHA256)
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Sigma.EllipticCurve (hashDomain)
import Hschain.Utxo.Lang.Utils.ByteString

import qualified Data.Vector as V

-- | Type synonym for money values
type Money = Int64

-- | Argument for script in the transaction
--
-- It's Key-Value map from argument-names to primitive constant values.
data Args = Args
  { args'ints  :: Vector Int64
  , args'bools :: Vector Bool
  , args'texts :: Vector Text
  , args'bytes :: Vector ByteString
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid Args

-- | Types that we can store as arguments in transactions.
-- We store lists of them.
data ArgType = IntArg | TextArg | BoolArg | BytesArg
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise)

argTypes :: [ArgType]
argTypes = [IntArg, TextArg, BoolArg, BytesArg]


-- | Identifier of TX. We can derive it from the PreTx.
--  It equals to hash of serialised PreTx
newtype TxId = TxId { unTxId :: Hash SHA256 }
  deriving newtype  (Show, Eq, Ord, NFData, ByteRepr, CryptoHashable)
  deriving stock    (Generic)
  deriving anyclass (Serialise)
  deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via (ViaBase58 "TxId" ByteString)

-- | Identifier of the box. Box holds value protected by the script.
-- It equals to the hash of Box-content.
newtype BoxId = BoxId { unBoxId :: Hash SHA256 }
  deriving newtype  (Show, Eq, Ord, NFData, ByteRepr, CryptoHashable)
  deriving stock    (Generic)
  deriving anyclass (Serialise)
  deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via (ViaBase58 "BoxId" ByteString)

instance ToText BoxId where
  toText (BoxId bs) = encodeBase58 bs

instance FromText BoxId where
  fromText txt = fmap BoxId $ decodeBase58 txt

-- | Type for script that goes over the wire.
newtype Script = Script { unScript :: ByteString }
  deriving newtype  (Show, Eq, Ord, NFData)
  deriving stock    (Generic)
  deriving anyclass (Serialise)
  deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via (ViaBase58 "Script" ByteString)

-- | Box holds the value protected by the script.
-- We use boxes as inputs for transaction and create new output boxes
-- when script is correct.
data Box = Box
  { box'id     :: !BoxId    -- ^ box identifier
  , box'value  :: !Money    -- ^ Value of the box
  , box'script :: !Script   -- ^ Protecting script
  , box'args   :: !Args     -- ^ arguments for the script
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)


-- | PreBox holds all meaningfull data of the Box.
-- we use it to get Hashes for transaction and Box itself.
-- Comparing to Box it omits identifier that is generated from PreBox
-- and origin that can be derived from TX identifier (hash of @getTxBytes tx@).
data PreBox = PreBox
  { preBox'value  :: !Money    -- ^ Value of the box
  , preBox'script :: !Script   -- ^ Protecting script
  , preBox'args   :: !Args     -- ^ arguments for the script
  }
  deriving (Show, Eq, Ord, Generic, Serialise, NFData)

computeBoxId :: BoxOrigin -> BoxId
computeBoxId BoxOrigin{..}
  = BoxId . hashBuilder
  $ hashStep boxOrigin'txId
 <> hashStep boxOrigin'outputIndex

-- | Data encodes the source of the Box when it was produced.
data BoxOrigin = BoxOrigin
  { boxOrigin'txId        :: !TxId   -- ^ identifier of TX that produced the Box
  , boxOrigin'outputIndex :: !Int64  -- ^ index in the vector of outputs for the box
  }
  deriving (Show, Eq, Ord, Generic, Serialise, NFData)


-- | Hash of transaction.
newtype TxHash = TxHash ByteString
  deriving newtype  (Show, Eq, Ord, Serialise, ByteRepr)
  deriving stock    (Generic)
  deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
       via ViaBase58 "TxHash" TxHash

-- | Type for transaction. It spends values from the input boxes and
--   create output boxes.
--
--   Each input references another box and contains proof for complete
--   transaction or sigma expression that should be proven when we
--   assemble transaction. Proof will be missing if spend script
--   evaluated to boolean.
data GTx i o = Tx
  { tx'inputs  :: !(Vector (BoxInputRef i))
    -- ^ List of inputs
  , tx'outputs :: !(Vector o)
    -- ^ List of outputs
  }
  deriving stock    (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Serialise, NFData)

-- | Transaction which is part of block and which are exchanged between clients
type Tx = GTx Proof Box

instance Bifunctor GTx where
  first f Tx{..} = Tx { tx'inputs = (fmap . fmap) f tx'inputs
                      , ..
                      }
  second = fmap

data TxSignMessage = TxSignMessage { unTxSignMessage :: ByteString }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)

-- | Input is an unspent Box that exists in blockchain.
-- To spend the input we need to provide right arguments and proof
-- of reulting sigma expression.
data BoxInputRef a = BoxInputRef
  { boxInputRef'id    :: BoxId
  , boxInputRef'args  :: Args
  , boxInputRef'proof :: Maybe a
  }
  deriving stock    (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Serialise, NFData)


getPreTx :: Tx -> GTx Proof PreBox
getPreTx = fmap toPreBox

toPreBox :: Box -> PreBox
toPreBox Box{..} = PreBox
  { preBox'value  = box'value
  , preBox'script = box'script
  , preBox'args   = box'args
  }

computeTxId :: Tx -> TxId
computeTxId = computePreTxId . getPreTx

computePreTxId :: GTx a PreBox -> TxId
computePreTxId Tx{..}
  = TxId . hashBuilder
  $ hashStep (UserType hashDomain "Tx")
 <> hashStepFoldableWith stepIn  tx'inputs
 <> hashStepFoldableWith stepOut tx'outputs
  where
    stepIn BoxInputRef{..}  = hashStep boxInputRef'id
                           <> hashStep boxInputRef'args
    stepOut = hashStep


-- | Tx with substituted inputs and environment.
--  This type is the same as Tx only it contains Boxes for inputs instead
-- of identifiers. Boxes are read from the current blockchain state.
data TxArg = TxArg
  { txArg'inputs   :: !(Vector BoxInput)
  , txArg'outputs  :: !(Vector Box)
  , txArg'env      :: !Env
  , txArg'txBytes  :: !TxId
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
  deriving (Show, Eq)

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

---------------------------------------------------------------------
-- smartconstructors to create boxes and transactions

-- | Creates TX and assigns properly all box identifiers.
-- It does not create the proofs.
newTx :: GTx Proof PreBox -> Tx
newTx tx = tx { tx'outputs = makeOutputs txId $ tx'outputs tx
              }
  where
    txId = computePreTxId tx

makeOutputs :: TxId -> Vector PreBox -> Vector Box
makeOutputs txId outputs = V.imap toBox outputs
  where
    toBox outputIndex PreBox{..} = Box
      { box'id     = boxId
      , box'value  = preBox'value
      , box'script = preBox'script
      , box'args   = preBox'args
      }
      where
        boxId = computeBoxId BoxOrigin
                { boxOrigin'outputIndex = fromIntegral outputIndex
                , boxOrigin'txId        = txId
                }

makeInputs
  :: ProofEnv
  -> TxId
  -> Vector (BoxInputRef (Sigma PublicKey))
  -> IO (Vector (BoxInputRef Proof))
makeInputs proofEnv message
  = traverse toInput
  where
    toInput BoxInputRef{..} = do
      mProof <- mapM (\sigma -> newProof proofEnv sigma message) boxInputRef'proof
      return BoxInputRef{ boxInputRef'proof = either (const Nothing) Just =<< mProof
                        , ..
                        }

makeInputsOrFail
  :: ProofEnv
  -> TxId
  -> Vector (BoxInputRef (Sigma PublicKey))
  -> IO (Either Text (Vector (BoxInputRef Proof)))
makeInputsOrFail proofEnv message
  = runExceptT . (traverse . traverse) toInput
  where
    toInput sigma = ExceptT $ newProof proofEnv sigma message


-- | Expectation of the result of the box. We use it when we know to
-- what sigma expression input box script is going to be executed.
-- Then we can generate proofs with function @newProofTx@.
type ExpectedBox = BoxInputRef (Sigma PublicKey)

-- | If we know the expected sigma expressions for the inputs
-- we can create transaction with all proofs supplied.
--
-- Otherwise we can create TX with empty proof and query the expected results of sigma-expressions
-- over API.
--
-- Note: If it can not produce the proof (user don't have corresponding private key)
-- it produces @Nothing@ in the @boxInputRef'proof@.
newProofTx :: MonadIO io => ProofEnv -> GTx (Sigma PublicKey) PreBox -> io Tx
newProofTx proofEnv tx = liftIO $ do
  inputs <- makeInputs proofEnv txId $ tx'inputs tx
  return $ Tx
    { tx'inputs  = inputs
    , tx'outputs = makeOutputs txId $ tx'outputs tx
    }
  where
    txId  = computePreTxId tx

-- | If we now the expected sigma expressions for the inputs
-- we can create transaction with all proofs supplied.
-- Whole function fails if any of the proof can not be produced
--
-- Otherwise we can create TX with empty proof and query the expected results of sigma-expressions
-- over API.
newProofTxOrFail :: MonadIO io => ProofEnv -> GTx (Sigma PublicKey) PreBox -> io (Either Text Tx)
newProofTxOrFail proofEnv tx = liftIO $ do
  eInputs <- makeInputsOrFail proofEnv txId $ tx'inputs tx
  return $ fmap (\inputs -> Tx
    { tx'inputs  = inputs
    , tx'outputs = makeOutputs txId $ tx'outputs tx
    }) eInputs
  where
    txId  = computePreTxId tx

--------------------------------------------
-- box ids validation

-- | Checks that all output boxes have correct identifiers that are based on hashes.
validateOutputBoxIds :: Tx -> Bool
validateOutputBoxIds tx = and $ V.imap checkBoxId $ tx'outputs tx
  where
    txId = computeTxId tx

    checkBoxId n Box{..} = box'id == getId n

    getId n = computeBoxId BoxOrigin
              { boxOrigin'outputIndex = fromIntegral n
              , boxOrigin'txId        = txId
              }

-- | Claculate the hash of the script.
hashScript :: Script -> ByteString
hashScript = getSha256 . unScript


--------------------------------------------
-- useful utils

singleOwnerSigma :: PublicKey -> Sigma PublicKey
singleOwnerSigma pubKey = Fix $ SigmaPk pubKey

singleOwnerInput :: BoxId -> PublicKey -> Vector ExpectedBox
singleOwnerInput boxId pubKey = return $ BoxInputRef
  { boxInputRef'id    = boxId
  , boxInputRef'args  = mempty
  , boxInputRef'proof = Just $ singleOwnerSigma pubKey
  }


--------------------------------------------
-- JSON instnaces

$(deriveJSON dropPrefixOptions ''GTx)
$(deriveJSON dropPrefixOptions ''TxArg)
$(deriveJSON dropPrefixOptions ''Env)
$(deriveJSON dropPrefixOptions ''BoxInput)
$(deriveJSON dropPrefixOptions ''BoxInputRef)
$(deriveJSON dropPrefixOptions ''Box)

instance CryptoHashable Tx where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxInput where
  hashStep = genericHashStep hashDomain

instance CryptoHashable a => CryptoHashable (BoxInputRef a) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Script where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Box where
  hashStep = genericHashStep hashDomain

instance CryptoHashable PreBox where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Args where
  hashStep = genericHashStep hashDomain

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
