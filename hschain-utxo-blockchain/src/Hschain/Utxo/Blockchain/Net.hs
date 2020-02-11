module Hschain.Utxo.Blockchain.Net(
  NodeSpec(..)
) where

import Hex.Common.Aeson

import GHC.Generics

import HSChain.Crypto
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.P2P
import HSChain.Blockchain.Internal.Engine.Types

----------------------------------------------------------------
 -- Generating node specification
 ----------------------------------------------------------------

-- | Specification of nodes
data NodeSpec = NodeSpec
  { nspec'privKey        :: Maybe (PrivValidator (Ed25519 :& SHA512))
    -- ^ Private key of validator
  , nspec'validators     :: [PublicKey (Ed25519 :& SHA512)]
    -- ^ Set of public keys of validator nodes
  , nspec'dbName         :: FilePath
    -- ^ Database name for the node
  , nspec'port           :: String
    -- ^ Port to listen on
  , nspec'seeds          :: [NetAddr]
    -- ^ Set of initial addresses
  , nspec'monitoringPort :: Maybe Int
  }
  deriving (Generic, Show)

$(deriveJSON dropPrefixOptions ''NodeSpec)

