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
import Data.Default.Class
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
import HSChain.Control.Class
import HSChain.Crypto hiding (PublicKey)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Logger
import HSChain.Monitoring
import HSChain.Run
import HSChain.Network.TCP (newNetworkTcp)
import HSChain.Store
import HSChain.Store.STM
import HSChain.Types
import HSChain.Types.Merkle.Types

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

interpretSpec
   :: ( MonadDB BData m, MonadFork m, MonadMask m, MonadLogger m
      , MonadTMMonitoring m)
   => (Block BData -> m ())
   -> NodeSpec
   -> [Tx]
   -> m (Bchain m, [m ()])
interpretSpec callBackOnCommit nodeSpec genesisTx = do
  txWaitChan <- liftIO newBroadcastTChanIO
  conn     <- askConnectionRO
  store    <- newSTMBchStorage $ blockchainState genesis
  mempool  <- makeMempool store (ExceptT . return)
  acts <- runNode (def :: Configuration BoxChainConfig)
        $ getNodeDesc nodeSpec mempool store genesis txWaitChan callBackOnCommit
  let bchain = Bchain
          { bchain'conn       = conn
          , bchain'mempool    = mempool
          , bchain'store      = store
          , bchain'waitForTx  = getTxWait txWaitChan conn mempool
          }
  return (bchain, acts)
  where
    validatorSet = getValidatorSet nodeSpec
    genesisBlock = bchValue genesis
    genesis = initBoxChain validatorSet genesisTx

getNodeDesc :: (Monad m, MonadIO m)
  => NodeSpec
  -> Mempool m (Alg BData) Tx
  -> BChStore m BData
  -> Genesis BData
  -> TChan [Hash UtxoAlg]
  -> (Block BData -> m ())
  -> NodeDescription m BData
getNodeDesc spec@NodeSpec{..} mempool store genesis txWaitChan callBackOnCommit =
  NodeDescription
    { nodeValidationKey = nspec'privKey
    , nodeGenesis       = genesis
    , nodeCallbacks     = mempty { appCommitCallback = getCommitCallback txWaitChan callBackOnCommit }
    , nodeRunner        = ExceptT . return
    , nodeStore         = AppStore { appBchState = store
                                   , appMempool  = mempool
                                   }
    , nodeNetwork       = BlockchainNet
        { bchNetwork      = newNetworkTcp nspec'port
        , bchInitialPeers = nspec'seeds
        }
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
  , blockValidators    = hashed valSet0
  , blockNewValidators = hashed valSet1
  , blockData          = merkled dat
  , blockPrevCommit    = Nothing
  , blockEvidence      = merkled []
  , blockStateHash     = stateHash
  }

-------------------------------------------

data BoxChainSpec = BoxChainSpec

initBoxChain :: ValidatorSet UtxoAlg -> [Tx] -> Genesis BData
initBoxChain valSet txs = BChEval
  { bchValue        = extractRealGenesisHash genesis
  , validatorSet    = merkled valSet
  , blockchainState = merkled state0
  }
  where
    genesis = getFakeHashGenesis
    state0 = emptyBoxChain

    getFakeHashGenesis = makeGenesis (BData txs) fakeHash valSet valSet
    fakeHash = hashed emptyBoxChain

    extractRealGenesisHash gen = gen { blockStateHash = merkleHashed st }
      where
        Right BChEval{blockchainState=st}
          = processBlock utxoLogic BChEval
            { bchValue        = gen
            , validatorSet    = merkled valSet
            , blockchainState = merkled state0
            }

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


