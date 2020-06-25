module Hschain.Utxo.App(
  runApp
) where

import Hex.Common.Aeson
import Hex.Common.Yaml

import Codec.Serialise

import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe

import Data.ByteString (ByteString)

import Data.Functor.Classes (Show1)

import qualified Data.Map.Strict as Map

import Data.Maybe

import Data.Text (Text)

import GHC.Generics

import Servant.API
import Servant.Server

import HSChain.Crypto.Classes
import HSChain.Crypto.SHA
import qualified HSChain.Crypto.Classes.Hash as Crypto
import qualified HSChain.PoW.Types as POWTypes
import qualified HSChain.PoW.Node as POWNode
import HSChain.Types.Merkle.Types

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import qualified Hschain.Utxo.State.Query as S
import Hschain.Utxo.State.Types

import Hschain.Utxo.API.Rest

-- ^A block proper. It does not contain nonce to solve PoW puzzle
-- but it contains all information about block.
data UTXOBlockProper f = UTXOBlockProper
  { ubpPrevious   :: !(Crypto.Hash SHA256)
  -- ^Previous block.
  , ubpData       :: !(MerkleNode f SHA256 [Tx])
  -- ^ List of key-value pairs
  , ubpTarget     :: !POWTypes.Target
  -- ^ Current difficulty of mining. It means a complicated thing
  -- right now.
  , ubpTime       :: !POWTypes.Time
  -- ^ Block creation time.
  }
  deriving stock (Generic)
deriving stock instance (Show1 f)    => Show (UTXOBlockProper f)
deriving stock instance (IsMerkle f) => Eq   (UTXOBlockProper f)
instance Serialise (UTXOBlockProper Identity)
instance Serialise (UTXOBlockProper Proxy)

instance (IsMerkle f) => Crypto.CryptoHashable (UTXOBlockProper f) where
  hashStep = Crypto.genericHashStep "block proper"

-- ^The block. Nonce (puzzle answer) is prepended to the header
-- as it is more secure - prevents selfish pool mining utilization.
-- When nonce is before block and answer is computed as a hash of nonce
-- and header, then each (pool) miner sees what is previous block
-- and can detect selfish mining and mining pool utilization for
-- currencies it does not support.
data UTXOBlock f = UTXOBlock
  { ubNonce       :: !ByteString
  , ubProper      :: !(UTXOBlockProper f)
  }
  deriving stock (Generic)
deriving stock instance (Show1 f)    => Show (UTXOBlock f)
deriving stock instance (IsMerkle f) => Eq   (UTXOBlock f)
instance Serialise (UTXOBlock Identity)
instance Serialise (UTXOBlock Proxy)

instance (IsMerkle f) => Crypto.CryptoHashable (UTXOBlock f) where
  hashStep = Crypto.genericHashStep "block proper"


-- |Run the PoW node.
runNode :: String -> IO ()
runNode cfgConfigPath =
  POWNode.runNode [cfgConfigPath] optMine genesisBlock utxoViewStep Map.empty (getBlockToMine optNodeName)
  where
    getBlockToMine = error "getBlockToMine"
    optNodeName = error "optnodename"
    optMine = True
    genesisBlock = undefined
    utxoViewStep :: POWTypes.Block UTXOBlock -> Map.Map Int String -> Maybe (Map.Map Int String)
    utxoViewStep b m
      | otherwise                               = error "utxo view step is not done"
      where
        txs = merkleValue $ ubpData $ ubProper $ POWTypes.blockData b

-- | Server implementation for 'UtxoAPI'
utxoServer :: ServerT UtxoAPI ServerM
utxoServer =
       postTxEndpoint
  :<|> getBoxBalanceEndpoint
  :<|> getTxSigmaEndpoint
  :<|> getEnvEndpoint
  :<|> getStateEndpoint

postTxEndpoint :: Tx -> ServerM PostTxResponse
postTxEndpoint tx = fmap PostTxResponse $ postTxWait tx

getBoxBalanceEndpoint :: BoxId -> ServerM (Maybe Money)
getBoxBalanceEndpoint boxId =
  --fmap (\bch -> S.getBoxBalance bch boxId) readBoxChain
  pure Nothing

getTxSigmaEndpoint :: Tx -> ServerM SigmaTxResponse
getTxSigmaEndpoint tx =
  --fmap (\bch -> uncurry SigmaTxResponse $ execInBoxChain tx bch) readBoxChain
  pure $ SigmaTxResponse (Left "I can't") ("Yes, you heard right - I can't")

getEnvEndpoint :: ServerM GetEnvResponse
getEnvEndpoint = do
  --bch <- readBoxChain
  --GetEnvResponse <$> getEnv bch
  return undefined

getStateEndpoint :: ServerM BoxChain
getStateEndpoint =
  --readBoxChain
  return undefined

-- |Application environment.
data AppEnv

-- | Server monad that holds internal environment
newtype ServerM a = ServerM { unServerM :: ReaderT AppEnv Handler a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadReader AppEnv
           , MonadThrow, MonadCatch)

newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT AppEnv Handler) a }

-- | Execution of 'ServerM'
runServerM :: AppEnv -> ServerM a -> Handler a
runServerM e = flip runReaderT e . unServerM

-- | Execution of 'ServerM' in IO monad
runServerMIO :: AppEnv -> ServerM a -> IO a
runServerMIO env m = do
  ea <- runHandler $ runServerM env m
  case ea of
    Left e -> fail $ "runServerMIO: " <> show e
    Right a -> return a

-- | Reads current state of the block chain
readBoxChain :: ServerM BoxChain
readBoxChain =
  --readBoxChainState
  return undefined


-- | Connection to hschain internals.
-- Low level API to post transactions.
data Bchain (m :: * -> *)

class MonadIO m => MonadBChain m where
  askBchain :: m (Bchain IO)

--------------------------------------------------
------ bchain store operations

writeTx :: (MonadBChain m) => Tx -> m (Maybe TxHash)
writeTx tx = do
  --Bchain{..} <- askBchain
  --liftIO $ fmap ((\(Crypto.Hashed (Crypto.Hash h)) -> TxHash h)) <$>
  --  ((\cursor -> pushTransaction cursor tx) =<< getMempoolCursor bchain'mempool)
  pure Nothing

readBlock :: (MonadIO m, MonadBChain m) => Int -> m (Maybe [Tx])
readBlock height = do
  --Bchain{..} <- askBchain
  --liftIO $ do
  --  mb <- runDBT bchain'conn $ queryRO $ retrieveBlock (Height $ fromIntegral height)
  --  pure $ unBData . merkleValue . blockData <$> mb
  pure Nothing

blockchainSize :: (MonadIO m, MonadBChain m) => m Int
blockchainSize = do
  --Bchain{..} <- askBchain
  --liftIO $ do
  --  Height h <- runDBT bchain'conn $ queryRO blockchainHeight
  --  pure $! fromIntegral h
  return 0

readBoxChainState :: (MonadBChain m) => m BoxChain
readBoxChainState = do
  --Bchain{..} <- askBchain
  --liftIO $ merkleValue . snd <$> bchCurrentState bchain'store
  return undefined

waitForTx :: (MonadBChain m) => m (TxHash -> m Bool)
waitForTx = do
  --Bchain{..} <- askBchain
  --fmap liftIO <$> liftIO bchain'waitForTx
  return (const $ return False)

postTxWait :: (MonadBChain m) => Tx -> m (Maybe TxHash)
postTxWait tx = do
  -- We start listening before sending transaction to mempool to avoid
  -- race when tx is commited before we start listening
  listener <- waitForTx
  runMaybeT $ do
    h <- MaybeT $ writeTx tx
    guard =<< lift (listener h)
    -- lift $ incMetricSurelyPostedTx tx
    return h


