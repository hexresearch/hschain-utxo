-- | Network specification for hschain node
module Hschain.Utxo.Blockchain.Net(
    NodeSpec(..)
  , LogSpec(..)
) where

import Hex.Common.Aeson

import Data.Word
import Data.Text (Text)

import GHC.Generics

import HSChain.Crypto
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.P2P
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Logger (ScribeSpec)

----------------------------------------------------------------
 -- Generating node specification
 ----------------------------------------------------------------

-- | Specification of nodes
data NodeSpec = NodeSpec
  { nspec'privKey        :: Maybe (PrivValidator (Ed25519 :& SHA512))
    -- ^ Private key of validator
  , nspec'validators     :: [PublicKey (Ed25519 :& SHA512)]
    -- ^ Set of public keys of validator nodes
  , nspec'dbName         :: Maybe FilePath
    -- ^ Database name for the node
  , nspec'port           :: Word16
    -- ^ Port to listen on
  , nspec'seeds          :: [NetAddr]
    -- ^ Set of initial addresses
  , nspec'monitoringPort :: Maybe Int
  , nspec'logs           :: LogSpec
    -- ^ Loggers
  }
  deriving (Generic, Show)

data LogSpec = LogSpec
  { logSpec'files           :: ![ScribeSpec]
    -- ^ Log files to write to
  , logSpec'namespace       :: !Text
    -- ^ Logger namespace
  } deriving (Generic, Show)

$(deriveJSON dropPrefixOptions ''LogSpec)
$(deriveJSON dropPrefixOptions ''NodeSpec)

