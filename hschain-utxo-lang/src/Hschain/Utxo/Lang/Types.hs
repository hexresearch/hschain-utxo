-- | Defines basic types for blockchain.
module Hschain.Utxo.Lang.Types
  ( -- * Transaction types
    Tx
  , GTx(..)
  , TxId(..)
  , Box(..)
  , BoxId(..)
  , BoxInputRef(..)
  , Args(..)
  , ArgType(..)
  , Script(..)
  , Money
  , computeBoxId
  , argTypes
    -- * Blockchain state manipulation
  , InputEnv(..)
  , TxArg(..)
  , buildTxArg
  , BoxInput(..)
  , BoxOutput(..)
  , SigMask(..)
  , signAll
  , ExpectedBox
  , Env(..)
  , TxHash(..)
  , PostBox(..)
    -- * Functions
  , newProofTx
  , newProofTxOrFail
  , hashScript
  , getInputEnv
  , txPreservesValue
  , computeTxId
  , SigMessage(..)
  , getSigMessageTx
    -- * Helperes
  , singleOwnerInput
    -- * Lenses
  , txArg'envL, txArg'idL, txArg'inputsL, txArg'outputsL
  , boxInput'argsL, boxInput'boxL, boxInput'idL, boxInput'proofL
  , boxInput'sigMaskL, boxInput'sigMsgL, boxInput'sigsL
  , box'argsL, box'scriptL, box'valueL
  ) where

import Hex.Common.Aeson
import Hex.Common.Text
import Hex.Common.Lens (makeLensesWithL)
import Control.DeepSeq (NFData)
import Control.Monad.Except

import Codec.Serialise
import Data.Aeson      ((.=),(.:),object,withObject)
import Data.ByteString (ByteString)
import Data.Bifunctor
import Data.Coerce
import Data.Fix
import Data.Int
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))
import GHC.Generics

import HSChain.Crypto.Classes      (ViaBase58(..), ByteRepr, decodeBase58, encodeBase58)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA          (SHA256)
import Hschain.Utxo.Lang.Crypto.Signature
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Sigma.EllipticCurve (hashDomain)
import Hschain.Utxo.Lang.Utils.ByteString

import qualified Data.List as L
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
  deriving newtype  (Show, Eq, Ord, NFData, ByteRepr)
  deriving stock    (Generic)
  deriving anyclass (Serialise)
  deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via (ViaBase58 "Script" ByteString)

-- | Box holds the value protected by the script.
-- We use boxes as inputs for transaction and create new output boxes
-- when script is correct.
data Box = Box
  { box'value  :: !Money    -- ^ Value of the box
  , box'script :: !Script   -- ^ Protecting script
  , box'args   :: !Args     -- ^ arguments for the script
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)

-- | Box and relevant information that is aquired at TX-post time.
data PostBox = PostBox
  { postBox'content :: Box    -- ^ approved box
  , postBox'height  :: Int64  -- ^ Height of block at which box was commited to blockchain
  } deriving (Show, Eq, Generic, Serialise)

computeBoxId :: TxId -> Int64 -> BoxId
computeBoxId txId i
  = BoxId . hashBuilder
  $ hashStep txId
 <> hashStep i

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
  deriving Monoid via GenericSemigroupMonoid (GTx i o)

-- | Transaction which is part of block and which are exchanged between clients
type Tx    = GTx Proof Box

data TxSizes = TxSizes
  { txSizes'inputs  :: !Int
  , txSizes'outputs :: !Int
  } deriving (Show, Eq)

instance Bifunctor GTx where
  first f tx = tx { tx'inputs = (fmap . fmap) f (tx'inputs tx) }
  second = fmap


instance Semigroup (GTx ins outs) where
  (<>) a b = Tx
    { tx'inputs  = appendInputs (tx'inputs a) (tx'inputs b)
    , tx'outputs = tx'outputs a <> tx'outputs b
    }
    where
      appendInputs aIns bIns = aIns <> fmap (shiftRefMask bSizes) bIns

      aSizes = getTxSizes a
      bSizes = getTxSizes b

      shiftRefMask sizes ref@BoxInputRef{..} = ref
        { boxInputRef'sigMask = shiftMask sizes $ boxInputRef'sigMask
        }

      shiftMask sizes x = appendSigMask (aSizes, prefix) (sizes, x)
        where
          prefix = SigMask
            { sigMask'inputs  = V.replicate (txSizes'inputs  aSizes) False
            , sigMask'outputs = V.replicate (txSizes'outputs aSizes) False
            }

getTxSizes :: GTx ins outs -> TxSizes
getTxSizes Tx{..} = TxSizes
  { txSizes'inputs  = V.length tx'inputs
  , txSizes'outputs = V.length tx'outputs
  }

-- | Input is an unspent Box that exists in blockchain.
-- To spend the input we need to provide right arguments and proof
-- of reulting sigma expression.
data BoxInputRef a = BoxInputRef
  { boxInputRef'id       :: BoxId
  -- ^ identifier of the box to spend
  , boxInputRef'args     :: Args
  -- ^ arguments for box script
  , boxInputRef'proof    :: Maybe a
  -- ^ proof for the script
  , boxInputRef'sigs     :: Vector Signature
  -- ^ signatures for the script. We have to exclude this field on computing TxId and on computing SigMessage
  , boxInputRef'sigMask  :: SigMask
  -- ^ mask of TX which defines the filter of inputs and outputs that we sign
  }
  deriving stock    (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Serialise, NFData)

-- | Signature mask. It defines what inputs and outputs
-- are included in the message to sign.
--
-- Empty SigMask means sign all inputs and outputs.
data SigMask = SigMask
  { sigMask'inputs   :: Vector Bool
  , sigMask'outputs  :: Vector Bool
  } -- ^ Specify what inputs and outputs to sign
  | SigAll
  -- ^ Signs whole transaction (all inputs and outputs)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)

appendSigMask :: (TxSizes, SigMask) -> (TxSizes, SigMask) -> SigMask
appendSigMask (aSizes, a) (bSizes , b) = case (a, b) of
  (SigAll, SigAll)      -> SigAll
  (SigAll, SigMask{..}) -> appendSigMask (aSizes, signAll aSizes) (bSizes, b)
  (SigMask{..}, SigAll) -> appendSigMask (aSizes, a) (bSizes, signAll bSizes)
  (SigMask aIns aOuts, SigMask bIns bOuts) -> SigMask (aIns <> bIns) (aOuts <> bOuts)

signAll :: TxSizes -> SigMask
signAll TxSizes{..} =
  SigMask
    { sigMask'inputs  = V.replicate txSizes'inputs  True
    , sigMask'outputs = V.replicate txSizes'outputs True
    }

computeTxId :: GTx a Box -> TxId
computeTxId Tx{..}
  = TxId . hashBuilder
  $ hashStep (UserType hashDomain "Tx")
 <> hashStepFoldableWith stepIn  tx'inputs
 <> hashStepFoldableWith stepOut tx'outputs
  where
    stepIn BoxInputRef{..}  = hashStep boxInputRef'id
                           <> hashStep boxInputRef'args
                           <> hashStep boxInputRef'sigMask
    stepOut = hashStep

getSigMessageTx :: SigMask -> GTx a Box -> SigMessage
getSigMessageTx mask Tx{..}
   = SigMessage . hashBuilder
   $ hashStep (UserType hashDomain "Tx")
  <> hashStepFoldableWith stepIn  (filterIns  tx'inputs)
  <> hashStepFoldableWith stepOut (filterOuts tx'outputs)
  where
    (filterIns, filterOuts) = case mask of
      SigMask{..} -> (filterMask sigMask'inputs, filterMask sigMask'outputs)
      SigAll      -> (id, id)

    stepIn BoxInputRef{..}  = hashStep boxInputRef'id
                           <> hashStep boxInputRef'args
                           <> hashStep boxInputRef'sigMask
    stepOut = hashStep

filterMask :: Vector Bool -> Vector a -> Vector a
filterMask mask v = fmap snd $ V.filter fst $ V.zip mask v

-- | Tx with substituted inputs and environment.
--  This type is the same as Tx only it contains Boxes for inputs instead
-- of identifiers. Boxes are read from the current blockchain state.
data TxArg = TxArg
  { txArg'inputs       :: !(Vector BoxInput)
  , txArg'outputs      :: !(Vector BoxOutput)
  , txArg'env          :: !Env
  , txArg'id           :: !TxId
  }
  deriving (Show, Eq)

-- | Information relevant to proof of the spend script
data BoxInput = BoxInput
  { boxInput'box     :: !PostBox
  , boxInput'id      :: !BoxId
  , boxInput'args    :: !Args
  , boxInput'proof   :: !(Maybe Proof)
  , boxInput'sigs    :: !(Vector Signature)
  , boxInput'sigMask :: !SigMask
  , boxInput'sigMsg  :: !SigMessage
  }
  deriving (Show, Eq, Generic)

-- | Information relevant to script for outputs
data BoxOutput = BoxOutput
  { boxOutput'box    :: !PostBox
  , boxOutput'id     :: !BoxId
  }
  deriving (Show, Eq, Generic)

-- | Substitute box references in transaction by boxes
buildTxArg
  :: Monad m
  => (BoxId -> m PostBox)      -- ^ Function to look up box by its IDs
  -> Env                       -- ^ Environment
  -> Tx
  -> m TxArg
buildTxArg lookupBox env tx@Tx{..} = do
  inputs <- forM tx'inputs $ \BoxInputRef{..} -> do
    box <- lookupBox boxInputRef'id
    pure BoxInput { boxInput'id      = boxInputRef'id
                  , boxInput'box     = box
                  , boxInput'args    = boxInputRef'args
                  , boxInput'proof   = boxInputRef'proof
                  , boxInput'sigs    = boxInputRef'sigs
                  , boxInput'sigMask = boxInputRef'sigMask
                  , boxInput'sigMsg  = getSigMessageTx boxInputRef'sigMask tx
                  }
  pure TxArg { txArg'inputs   = inputs
             , txArg'outputs  = V.imap (\i b -> let boxId = computeBoxId txId (fromIntegral i)
                                                in  BoxOutput (PostBox b height) boxId
                                       ) tx'outputs
             , txArg'env      = env
             , txArg'id       = txId
             }
  where
    txId = computeTxId tx
    height = env'height env

-- | Blockchain environment variables.
data Env = Env
  { env'height   :: !Int64    -- ^ blockchain height
  } deriving (Show, Eq)

-- | Input environment contains all data that have to be used
-- during execution of the script.
data InputEnv = InputEnv
  { inputEnv'height  :: !Int64
  , inputEnv'self    :: !BoxInput
  , inputEnv'inputs  :: !(Vector BoxInput)
  , inputEnv'outputs :: !(Vector BoxOutput)
  , inputEnv'args    :: !Args
  , inputEnv'sigs    :: !(Vector Signature)
  , inputEnv'sigMsg  :: !SigMessage
  }
  deriving (Show, Eq)

getInputEnv :: TxArg -> BoxInput -> InputEnv
getInputEnv TxArg{..} input = InputEnv
  { inputEnv'self    = input
  , inputEnv'height  = env'height txArg'env
  , inputEnv'inputs  = txArg'inputs
  , inputEnv'outputs = txArg'outputs
  , inputEnv'args    = boxInput'args input
  , inputEnv'sigs    = boxInput'sigs input
  , inputEnv'sigMsg  = boxInput'sigMsg input
  }

txPreservesValue :: TxArg -> Bool
txPreservesValue tx@TxArg{..}
  | isStartEpoch tx = True
  | otherwise       = sumWith (box'value . postBox'content . boxInput'box) txArg'inputs == sumWith (box'value . postBox'content . boxOutput'box) txArg'outputs
  where
    sumWith f = L.foldl' (\acc x -> acc + f x) 0

isStartEpoch :: TxArg -> Bool
isStartEpoch TxArg{..} = env'height txArg'env == 0

---------------------------------------------------------------------
-- smartconstructors to create boxes and transactions

makeInput
  :: GTx (Sigma PublicKey) Box
  -> ProofEnv
  -> BoxInputRef (Sigma PublicKey)
  -> IO (BoxInputRef Proof)
makeInput tx proofEnv BoxInputRef{..} = do
  let message = getSigMessageTx boxInputRef'sigMask tx
  mProof <- mapM (\sigma -> newProof proofEnv sigma message) boxInputRef'proof
  return BoxInputRef{ boxInputRef'proof = either (const Nothing) Just =<< mProof
                    , ..
                    }

makeInputOrFail
  :: GTx (Sigma PublicKey) Box
  -> ProofEnv
  -> BoxInputRef (Sigma PublicKey)
  -> ExceptT Text IO (BoxInputRef Proof)
makeInputOrFail tx proofEnv ref@BoxInputRef{..}
  = traverse toInput ref
  where
    toInput sigma = ExceptT $ newProof proofEnv sigma message

    message = getSigMessageTx boxInputRef'sigMask tx


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
newProofTx :: MonadIO io => ProofEnv -> GTx (Sigma PublicKey) Box -> io Tx
newProofTx proofEnv tx = liftIO $ do
  inputs <- traverse (makeInput tx proofEnv) $ tx'inputs tx
  return $ Tx
    { tx'inputs  = inputs
    , tx'outputs = tx'outputs tx
    }

-- | If we now the expected sigma expressions for the inputs
-- we can create transaction with all proofs supplied.
-- Whole function fails if any of the proof can not be produced
--
-- Otherwise we can create TX with empty proof and query the expected results of sigma-expressions
-- over API.
newProofTxOrFail :: MonadIO io => ProofEnv -> GTx (Sigma PublicKey) Box -> io (Either Text Tx)
newProofTxOrFail proofEnv tx = liftIO $ do
  eInputs <- runExceptT $ traverse (makeInputOrFail tx proofEnv) $ tx'inputs tx
  return $ fmap (\inputs -> Tx
    { tx'inputs  = inputs
    , tx'outputs = tx'outputs tx
    }) eInputs

--------------------------------------------
-- box ids validation

-- | Claculate the hash of the script.
hashScript :: Script -> ByteString
hashScript = getSha256 . unScript


--------------------------------------------
-- useful utils

singleOwnerSigma :: PublicKey -> Sigma PublicKey
singleOwnerSigma pubKey = Fix $ SigmaPk pubKey

singleOwnerInput :: BoxId -> PublicKey -> Vector ExpectedBox
singleOwnerInput boxId pubKey = return $ BoxInputRef
  { boxInputRef'id      = boxId
  , boxInputRef'args    = mempty
  , boxInputRef'proof   = Just $ singleOwnerSigma pubKey
  , boxInputRef'sigs    = mempty
  , boxInputRef'sigMask = SigAll
  }


--------------------------------------------
-- JSON instnaces

$(deriveJSON dropPrefixOptions ''GTx)
$(deriveJSON dropPrefixOptions ''TxArg)
$(deriveJSON dropPrefixOptions ''Env)
$(deriveJSON dropPrefixOptions ''PostBox)
$(deriveJSON dropPrefixOptions ''BoxInput)
$(deriveJSON dropPrefixOptions ''BoxOutput)
$(deriveJSON dropPrefixOptions ''BoxInputRef)
$(deriveJSON dropPrefixOptions ''Box)
$(deriveJSON dropPrefixOptions ''SigMask)

instance CryptoHashable Tx where
  hashStep = genericHashStep hashDomain

instance CryptoHashable PostBox where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxInput where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxOutput where
  hashStep = genericHashStep hashDomain

instance CryptoHashable a => CryptoHashable (BoxInputRef a) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Script where
  hashStep = genericHashStep hashDomain

instance CryptoHashable SigMask where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Box where
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

$(makeLensesWithL ''TxArg)
$(makeLensesWithL ''BoxInput)
$(makeLensesWithL ''Box)
