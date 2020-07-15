-- | Defines basic types for blockchain.
module Hschain.Utxo.Lang.Types where

import Hex.Common.Aeson
import Hex.Common.Serialise
import Hex.Common.Text
import Control.DeepSeq (NFData)
import Control.Monad

import Codec.Serialise

import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Generics

import HSChain.Crypto.Classes.Hash (CryptoHashable(..), genericHashStep)
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Sigma.EllipticCurve (hashDomain)
import Hschain.Utxo.Lang.Parser.Hask

import qualified Crypto.Hash as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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


-- | Tx with substituted inputs and environment.
--  This type is the same as Tx only it contains Boxes for inputs instead
-- of identifiers. Boxes are read from the current blockchain state.
data TxArg = TxArg
  { txArg'inputs  :: !(Vector Box)
  , txArg'outputs :: !(Vector Box)
  , txArg'proof   :: !(Maybe Proof)
  , txArg'args    :: !Args
  , txArg'env     :: !Env
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

--------------------------------------------

-- | Parses script from text.
parseScript :: Text -> Either Text (Expr Bool)
parseScript txt =
  case parseExp (Just "<parseSrcipt>") $ T.unpack txt of
    ParseOk expr        -> Right $ Expr expr
    ParseFailed loc msg -> Left $ mconcat ["Parse failed at ", showt loc, " with ", T.pack msg]

-- | Convert script to boolean expression.
fromScript :: Script -> Either Text (Expr Bool)
fromScript (Script txt) = parseScript txt

-- | Convert boolean expression to script.
toScript :: Expr SigmaBool -> Script
toScript (Expr expr) = Script $ T.pack $ prettyExp expr

encodeScript :: ExecCtx -> Lang -> Text
encodeScript = undefined

decodeScript :: Text -> Either Text (ExecCtx, Lang)
decodeScript = undefined

-- | Claculate the hash of the script.
hashScript ::  C.HashAlgorithm a => a -> Script -> Text
hashScript algo = hash . unScript
  where
    hash :: Text -> Text
    hash txt = showt $ C.hashWith algo $ T.encodeUtf8 txt

scriptToText :: Script -> Text
scriptToText = unScript

--------------------------------------------
-- JSON instnaces

$(deriveJSON dropPrefixOptions ''Tx)
$(deriveJSON dropPrefixOptions ''TxArg)
$(deriveJSON dropPrefixOptions ''Env)

instance CryptoHashable Tx where
  hashStep = genericHashStep hashDomain

