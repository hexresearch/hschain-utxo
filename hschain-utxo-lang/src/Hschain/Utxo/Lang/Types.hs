module Hschain.Utxo.Lang.Types where

import Hex.Common.Aeson
import Hex.Common.Text
import Control.Monad

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import Safe

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Parser.Hask

import qualified Crypto.Hash as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype UserId = UserId { unUserId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype TxHash = TxHash Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data Tx = Tx
  { tx'inputs  :: !(Vector BoxId)
  , tx'outputs :: !(Vector Box)
  , tx'proof   :: !(Maybe Proof)
  , tx'args    :: !Args
  }
  deriving (Show, Eq)

-- | Tx with substituted inputs and environment.
data TxArg = TxArg
  { txArg'inputs  :: !(Vector Box)
  , txArg'outputs :: !(Vector Box)
  , txArg'proof   :: !(Maybe Proof)
  , txArg'args    :: !Args
  , txArg'env     :: !Env
  }
  deriving (Show, Eq)

data Env = Env
  { env'height   :: !Integer }
  deriving (Show, Eq)


--------------------------------------------

parseScript :: Text -> Maybe (Expr Bool)
parseScript txt =
  case parseExp $ T.unpack txt of
    ParseOk expr    -> Just $ Expr expr
    ParseFailed _ _ -> Nothing

fromScript :: Script -> Maybe (Expr Bool)
fromScript (Script txt) = parseScript txt

toScript :: Expr Bool -> Script
toScript (Expr expr) = Script $ T.pack $ prettyExp expr

hashScript ::  C.HashAlgorithm a => a -> Script -> Text
hashScript algo = hash . unScript
  where
    hash :: Text -> Text
    hash txt = showt $ C.hashWith algo $ T.encodeUtf8 txt

--------------------------------------------
-- JSON instnaces

$(deriveJSON dropPrefixOptions ''Tx)
$(deriveJSON dropPrefixOptions ''TxArg)
$(deriveJSON dropPrefixOptions ''Box)
$(deriveJSON dropPrefixOptions ''Env)

