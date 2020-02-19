module Hschain.Utxo.Blockchain.Interpret(
    UtxoAlg
  , UtxoError(..)
  , BData(..)
  , initBoxChain
  , interpretSpec
--  , interpretSpecWithCallback
) where

import Data.Foldable

import Codec.Serialise      (Serialise, serialise)
import Control.Applicative
import Control.Concurrent.STM
import Control.DeepSeq      (NFData)
import Control.Exception    (Exception)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Parallel.Strategies
import Data.Fix
import Data.Fixed
import Data.Function (fix)
import Data.Either
import Data.Int
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Vector         as V
import qualified Crypto.ECC.Edwards25519  as Ed
import qualified Network.Socket as Net

import GHC.Generics (Generic)
import Prometheus

import System.FilePath
import System.Directory

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto hiding (PublicKey)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Monitoring
import HSChain.Run
import HSChain.P2P (generatePeerId)
import HSChain.P2P.Network (newNetworkTcp)
import HSChain.Store
import HSChain.Store.STM
import HSChain.Types
import HSChain.Types.Merkle.Types

import qualified HSChain.P2P.Types as P2PT

import Hschain.Utxo.Lang hiding (Height)
import Hschain.Utxo.State.React
import Hschain.Utxo.State.Types

import Hschain.Utxo.Blockchain.Logic
import Hschain.Utxo.Blockchain.Bchain
import Hschain.Utxo.Blockchain.Net

import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Interpreter as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Types as Sigma

------------------------------------------

{-
interpretSpec
   :: ( MonadDB m BData, MonadFork m, MonadMask m, MonadLogger m
      , MonadTrace m, MonadTMMonitoring m)
   => NodeSpec
   -> [Tx]
   -> (Bchain m -> m ())
   -> m ()
interpretSpec = interpretSpecWithCallback (const $ pure ())
-}

interpretSpec
   :: ( MonadDB m BData, MonadFork m, MonadMask m, MonadLogger m
      , MonadTrace m, MonadTMMonitoring m)
   => (Block BData -> m ())
   -> NodeSpec
   -> [Tx]
   -> m (Bchain m, [m ()])
interpretSpec callBackOnCommit nodeSpec genesisTx = do
  txWaitChan <- liftIO newBroadcastTChanIO
  conn     <- askConnectionRO
  store    <- newSTMBchStorage $ blockchainState genesis
  mempool  <- makeMempool store (ExceptT . return)
  nodeDesc <- getNodeDesc nodeSpec mempool store genesis txWaitChan callBackOnCommit
  acts <- runNode (defCfg :: Configuration BoxChainConfig) nodeDesc
  let bchain = Bchain
          { bchain'conn       = conn
          , bchain'mempool    = mempool
          , bchain'store      = store
          , bchain'waitForTx  = getTxWait txWaitChan conn mempool
          }
  initDB
  return (bchain, acts)
  -- runConcurrently (cont bchain : acts)
  where
    validatorSet = getValidatorSet nodeSpec
    genesisBlock = bchValue genesis
    genesis = initBoxChain validatorSet genesisTx

-- TODO
initDB :: Monad m => m ()
initDB = return ()

getNodeDesc :: (Monad m, MonadIO m)
  => NodeSpec
  -> Mempool m (Alg BData) Tx
  -> BChStore m BData
  -> Genesis BData
  -> TChan [Hash UtxoAlg]
  -> (Block BData -> m ())
  -> m (NodeDescription m BData)
getNodeDesc spec@NodeSpec{..} mempool store genesis txWaitChan callBackOnCommit = do
  net <- getNetwork spec
  return $ NodeDescription
    { nodeValidationKey = nspec'privKey
    , nodeGenesis       = genesis
    , nodeCallbacks     = mempty { appCommitCallback = getCommitCallback txWaitChan callBackOnCommit }
    , nodeRunner        = ExceptT . return
    , nodeStore         = AppStore { appBchState = store
                                   , appMempool  = mempool
                                   }
    , nodeNetwork       = net
    }

getNetwork :: (Monad m, MonadIO m)
  => NodeSpec -> m BlockchainNet
getNetwork NodeSpec{..} = do
  peerId <- generatePeerId
  let peerInfo = P2PT.PeerInfo peerId (read nspec'port) 0
  return $ BlockchainNet
    { bchNetwork          = newNetworkTcp peerInfo
    , bchInitialPeers     = nspec'seeds
    }

getTxWait :: (Monad m, MonadIO m)
  => TChan [Hash UtxoAlg]
  -> Connection 'RO BData
  -> Mempool m UtxoAlg Tx
  -> m (TxHash -> m Bool)
getTxWait txWaitChan conn mempool = do
  ch <- liftIO $ atomically $ dupTChan txWaitChan
  pure $ \(TxHash h0) -> fix $ \loop -> do
    let h = Hashed (Hash h0)
    hashes :: [Hash UtxoAlg] <- liftIO $ atomically $ readTChan ch
    case Hash h0 `elem` hashes of
      True  -> pure True
      False -> txInMempool mempool h >>= \case
        True  -> loop
        False -> pure False

getCommitCallback :: (Monad m, MonadIO m)
  => TChan [Hash UtxoAlg]
  -> (Block BData -> m ())
  -> Block BData
  -> m ()
getCommitCallback txWaitChan callBackOnCommit b = do
  liftIO $ atomically $ writeTChan txWaitChan $ fmap hash $ (\(BData txs) -> txs) $ merkleValue $ blockData b
  -- User callback
  callBackOnCommit b

getValidatorSet :: NodeSpec -> ValidatorSet UtxoAlg
getValidatorSet NodeSpec{..} =
  fromRight err $ makeValidatorSet [ Validator pk 1 | pk <- nspec'validators ]
  where
    err = error "Failed to get validator set"

getGenesisBlock :: NodeSpec -> [Tx] -> Block BData
getGenesisBlock nspec txs =
  makeGenesis (BData txs) (Hashed $ hash emptyBoxChain) validatorSet validatorSet
  where
    validatorSet = getValidatorSet nspec


-- | Genesis block has many field with predetermined content so this
--   is convenience function to create genesis block.
makeGenesis
  :: (Crypto (Alg a), CryptoHashable a)
  => a                          -- ^ Block data
  -> Hashed (Alg a) (BlockchainState a)
  -> ValidatorSet (Alg a)           -- ^ Set of validators for H=0
  -> ValidatorSet (Alg a)           -- ^ Set of validators for H=1
  -> Block a
makeGenesis dat stateHash valSet0 valSet1 = Block
  { blockHeight        = Height 0
  , blockPrevBlockID   = Nothing
  , blockValidators    = merkled valSet0
  , blockNewValidators = merkled valSet1
  , blockData          = merkled dat
  , blockPrevCommit    = Nothing
  , blockEvidence      = merkled []
  , blockStateHash     = stateHash
  }

-------------------------------------------

data BoxChainSpec = BoxChainSpec

initBoxChain :: ValidatorSet UtxoAlg -> [Tx] -> Genesis BData
initBoxChain valSet txs = BChEval
  { bchValue        = genesis
  , validatorSet    = merkled valSet
  , blockchainState = merkled state0
  }
  where
    genesis = makeGenesis (BData txs) (hashed state0) valSet valSet
    state0 = emptyBoxChain

-----------------------------------------------------------------
-- Prometheus metrics
{-
makeGaugeRegisteredUsers
  :: (MonadIO m, MonadMonitor n)
  => m (BoxChain -> n ())
makeGaugeRegisteredUsers = do
  g <- register
     $ gauge $ Info "huralchain_users_total" "Number of registered users"
  pure $ setGauge g
       . fromIntegral . Map.size . huralState'users
-}

data BoxChainConfig

instance DefaultConfig BoxChainConfig where
  defCfg = Configuration
    { cfgConsensus         = ConsensusCfg
      { timeoutNewHeight   = 500
      , timeoutProposal    = (500, 500)
      , timeoutPrevote     = (500, 500)
      , timeoutPrecommit   = (500, 500)
      , timeoutEmptyBlock  = 100
      , incomingQueueSize  = 10
      }
    , cfgNetwork               = NetworkCfg
      { gossipDelayVotes       = 25
      , gossipDelayBlocks      = 25
      , gossipDelayMempool     = 25
      , pexMinConnections      = 3
      , pexMaxConnections      = 12
      , pexMinKnownConnections = 3
      , pexMaxKnownConnections = 20
      , reconnectionRetries    = 100
      , reconnectionDelay      = 100
      }
    }

