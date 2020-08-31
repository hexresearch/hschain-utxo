module Hschain.Utxo.Blockchain.Interpret(
    UtxoAlg
  , UtxoError(..)
  , BData(..)
  , interpretSpec
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Default.Class
import Data.Function (fix)
import Data.IORef
import Data.Either
import qualified Data.Map.Strict as Map

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Crypto hiding (PublicKey)
import HSChain.Logger
import HSChain.Mempool
import HSChain.Monitoring
import HSChain.Run
import HSChain.Network.TCP (newNetworkTcp)
import HSChain.Store
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Internal.Types.Consensus

import Hschain.Utxo.Lang hiding (Height)

import Hschain.Utxo.Blockchain.Logic
import Hschain.Utxo.Blockchain.Bchain
import Hschain.Utxo.Blockchain.Net

------------------------------------------

interpretSpec
   :: ( MonadDB m, MonadCached BData m, MonadFork m, MonadMask m, MonadLogger m
      , MonadTMMonitoring m)
   => (Block BData -> m ())
   -> NodeSpec
   -> [Tx]
   -> m (Bchain m, [m ()])
interpretSpec callBackOnCommit nodeSpec@NodeSpec{..} genesisTx = do
  txWaitChan <- liftIO newBroadcastTChanIO
  conn       <- askConnectionRO
  (state,stRef,memThr) <- inMemoryView validatorSet
  let node = NodeDescription
        { nodeValidationKey = nspec'privKey
        , nodeGenesis       = genesis
        , nodeCallbacks     = mempty { appCommitCallback = \b -> do
                                         getCommitCallback txWaitChan b
                                         callBackOnCommit b
                                     }
        , nodeStateView     = state
        , nodeNetwork       = BlockchainNet
            { bchNetwork      = newNetworkTcp nspec'port
            , bchInitialPeers = nspec'seeds
            }
    }
  acts <- runNode (def :: Configuration BoxChainConfig) node
  cursor <- getMempoolCursor $ mempoolHandle $ stateMempool state
  let bchain = Bchain
          { bchain'conn          = conn
          , bchain'mempoolCursor = cursor
          , bchain'state         = liftIO $ readIORef stRef
          , bchain'waitForTx     = getTxWait txWaitChan $ stateMempool state
          }
  return (bchain, acts ++ memThr)
  where
    validatorSet = getValidatorSet nodeSpec
    genesis      = Genesis
      { genesisValSet = validatorSet
      , genesisBlock  = makeGenesis (BData genesisTx) validatorSet validatorSet
      }

getTxWait :: (Monad m, MonadIO m)
  => TChan [Hash UtxoAlg]
  -> Mempool m (Hashed UtxoAlg Tx) Tx
  -> m (TxHash -> m Bool)
getTxWait txWaitChan mempool = do
  ch <- liftIO $ atomically $ dupTChan txWaitChan
  pure $ \(TxHash h0) -> fix $ \loop -> do
    let h = Hashed (Hash h0)
    hashes :: [Hash UtxoAlg] <- liftIO $ atomically $ readTChan ch
    case Hash h0 `elem` hashes of
      True  -> pure True
      False -> txInMempool mempool h >>= \case
        True  -> loop
        False -> pure False
  where
    txInMempool Mempool{..} h = do
      MempoolState{..} <- liftIO getMempoolState
      return $ h `Map.member` mempRevMap
      

getCommitCallback :: (Monad m, MonadIO m)
  => TChan [Hash UtxoAlg]
  -> Block BData
  -> m ()
getCommitCallback txWaitChan b = do
  liftIO $ atomically $ writeTChan txWaitChan $ fmap hash $ (\(BData txs) -> txs) $ merkleValue $ blockData b


getValidatorSet :: NodeSpec -> ValidatorSet UtxoAlg
getValidatorSet NodeSpec{..} =
  fromRight err $ makeValidatorSet [ Validator pk 1 | pk <- nspec'validators ]
  where
    err = error "Failed to get validator set"


-- | Genesis block has many field with predetermined content so this
--   is convenience function to create genesis block.
makeGenesis
  :: (Crypto (Alg a), CryptoHashable a)
  => a                          -- ^ Block data
  -> ValidatorSet (Alg a)       -- ^ Set of validators for H=0
  -> ValidatorSet (Alg a)       -- ^ Set of validators for H=1
  -> Block a
makeGenesis dat valSet0 valSet1 = Block
  { blockHeight        = Height 0
  , blockPrevBlockID   = Nothing
  , blockValidators    = hashed valSet0
  , blockNewValidators = hashed valSet1
  , blockData          = merkled dat
  , blockPrevCommit    = Nothing
  , blockEvidence      = merkled []
  }

-------------------------------------------


-- | Config tag for our blockchain
data BoxChainConfig

-- Default settings
instance Default (ConsensusCfg BoxChainConfig) where
  def = ConsensusCfg
    { timeoutNewHeight   = 500
    , timeoutProposal    = (500, 500)
    , timeoutPrevote     = (500, 500)
    , timeoutPrecommit   = (500, 500)
    , timeoutEmptyBlock  = 100
    , incomingQueueSize  = 10
    }

instance Default (NetworkCfg BoxChainConfig) where
  def = NetworkCfg
    { gossipDelayVotes       = 25
    , gossipDelayBlocks      = 25
    , gossipDelayMempool     = 25
    , pexMinConnections      = 3
    , pexMaxConnections      = 12
    , pexMinKnownConnections = 3
    , pexMaxKnownConnections = 20
    , reconnectionRetries    = 100
    , reconnectionDelay      = 100
    , pexConnectionDelay     = 3000
    , pexAskPeersDelay       = 10000
    }


