module Hschain.Utxo.State.Blockchain.Config where

import Control.Exception   (Exception)
import Control.Monad.Catch (MonadCatch(..))
import GHC.Generics (Generic)

import qualified Data.Aeson as JSON

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Crypto
import HSChain.Crypto.Ed25519 (Ed25519)
import HSChain.Crypto.SHA     (SHA512)
import HSChain.Logger         (ScribeSpec)
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Store

data BoxChainConfig

instance DefaultConfig BoxChainConfig where
  defCfg = Configuration
    { cfgConsensus         = ConsensusCfg
      { timeoutNewHeight   = 500
      , timeoutProposal    = (500, 500)
      , timeoutPrevote     = (500, 500)
      , timeoutPrecommit   = (500, 500)
      , timeoutEmptyBlock  = 100
      , incomingQueueSize  = 7
      }
    , cfgNetwork               = NetworkCfg
      { gossipDelayVotes       = 25
      , gossipDelayBlocks      = 25
      , gossipDelayMempool     = 25
      , pexMinConnections      = 3
      , pexMaxConnections      = 20
      , pexMinKnownConnections = 3
      , pexMaxKnownConnections = 20
      , reconnectionRetries    = 12
      , reconnectionDelay      = 100
      }
    }

data NodeSpec = NodeSpec
  { nspecPrivKey    :: Maybe (PrivValidator (Ed25519 :& SHA512))
  , nspecDbName     :: Maybe FilePath
  , nspecLogFile    :: [ScribeSpec]
  }
  deriving (Generic,Show)

----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------

data RunningNode m a = RunningNode
  { rnodeState   :: BChStore m a
  , rnodeConn    :: Connection 'RO a
  , rnodeMempool :: Mempool m (Alg a) (TX a)
  }

hoistRunningNode
  :: (Functor n)
  => (forall x. m x -> n x) -> RunningNode m a -> RunningNode n a
hoistRunningNode fun RunningNode{..} = RunningNode
  { rnodeState   = hoistBChStore fun rnodeState
  , rnodeMempool = hoistMempool  fun rnodeMempool
  , ..
  }




