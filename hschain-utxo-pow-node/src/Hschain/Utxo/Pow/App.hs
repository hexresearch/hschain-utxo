-- |Hschain.Utxo.App.hs
--
-- Full fledged PoW consensus node, with external REST API.
--
-- Copyright (C) 2020 ...
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
-- Servant orphans
{-# OPTIONS_GHC -Wno-orphans #-}
module Hschain.Utxo.Pow.App(
    runApp
  , runNode
  , runLightNode
  , genesisTest
  , genesisMock
  , TestNet
  , MockChain
  , UtxoRestAPI(..)
    -- * Monad for running
  , UTXOT(..)
  , runUTXOT
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Cont

import qualified Data.Aeson as JSON
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import Data.Yaml.Config   (loadYamlSettings, requireEnv)
import Data.Word
import GHC.Generics

import Servant.API         ((:>),Capture,Summary,Get,JSON)
import Servant.API.Generic ((:-)(..))
import qualified Servant.API              as Servant
import qualified Servant.Server           as Servant
import qualified Servant.API.Generic      as Servant
import qualified Servant.Server.Generic   as Servant
import qualified Network.Wai.Handler.Warp as Warp

import Katip (LogEnv,Namespace)
import System.IO

import HSChain.Control.Class
import HSChain.Crypto.Classes
import HSChain.Store.Query
import HSChain.Logger
import HSChain.Config
import HSChain.Types.Merkle.Tree
import HSChain.Network.Types
import HSChain.Network.TCP
import HSChain.PoW.API
import qualified HSChain.Control.Channels as HControl
import qualified HSChain.Control.Class    as HControl
import qualified HSChain.Control.Util     as HControl
import qualified HSChain.PoW.P2P        as POW
import qualified HSChain.PoW.P2P.Types  as POW
import qualified HSChain.PoW.Consensus  as POW
import qualified HSChain.PoW.Node       as POW
import qualified HSChain.PoW.Types      as POW
import qualified HSChain.POW            as POW

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Pow.App.Options (Command(..), readCommandOptions)
import Hschain.Utxo.Pow.App.Types
 
-------------------------------------------------------------------------------
-- Executable part.

-- |Node's configuration.
data Cfg = Cfg
  { cfgPort           :: Word16
  , cfgPeers          :: [NetAddr]
  , cfgKnownPeers     :: Int
  , cfgConnectedPeers :: Int
  , cfgLog            :: [ScribeSpec]
  , cfgDB             :: Maybe FilePath
  , cfgWebAPI         :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving (JSON.FromJSON) via SnakeCase (DropSmart (Config Cfg))

data UTXOEnv = UTXOEnv
  { ueLogEnv      :: !LogEnv
  , ueNamespace   :: !Namespace
  , ueConn        :: !(Connection 'RW)
  }
  deriving (Generic)

newtype UTXOT m a = UTXOT (ReaderT UTXOEnv m a)
  deriving newtype ( Functor, Applicative, Monad, MonadIO
                   , MonadCatch, MonadThrow, MonadMask, MonadFork, MonadReader UTXOEnv)
  deriving (MonadLogger)          via LoggerByTypes  (ReaderT UTXOEnv m)
  deriving (MonadDB, MonadReadDB) via DatabaseByType (ReaderT UTXOEnv m)

runUTXOT :: LogEnv -> Connection 'RW -> UTXOT m a -> m a
runUTXOT logenv conn (UTXOT act) = runReaderT act (UTXOEnv logenv mempty conn)


-- | Standard configuration for testnet
data TestNet

instance UtxoPOWConfig TestNet where
  powConfig _ = POW.defaultPOWConfig

-- | Configuration that disables checking of work at all. This primarily useful for testing
data MockChain

instance UtxoPOWConfig MockChain where
  powConfig      _ = POW.defaultPOWConfig
  checkBlockWork _ = False


runApp :: UtxoPOWConfig t => POW.Block (UTXOBlock t) -> IO ()
runApp genesis = do
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  -- Parse configurationx
  command <- readCommandOptions
  case command of
    GenerateKey {..} -> do
      error "gen key is not done!"
    RunNode {..} -> do
      config <- loadYamlSettings runnode'config [] requireEnv
      runNode genesis config runnode'nodeSecret

genesisTest :: POW.Block (UTXOBlock TestNet)
genesisTest = POW.Block
  { blockHeight = POW.Height 0
  , blockTime   = POW.Time   0
  , prevBlock   = Nothing
  , blockData   = UTXOBlock
    { ubNonce  = ""
    , ubData   = createMerkleTreeNE1 $ coinbase :| []
    , ubTarget = POW.Target $ 2^(256::Int) - 1
    }
  }
  where
    coinbase = Tx { tx'inputs     = mempty
                  , tx'outputs    = mempty
                  , tx'dataInputs = mempty
                  }

genesisMock :: POW.Block (UTXOBlock MockChain)
genesisMock = POW.Block
  { blockHeight = POW.Height 0
  , blockTime   = POW.Time   0
  , prevBlock   = Nothing
  , blockData   = UTXOBlock
    { ubNonce  = ""
    , ubData   = createMerkleTreeNE1 $ coinbase :| []
    , ubTarget = POW.Target $ 2^(256::Int) - 1
    }
  }
  where
    coinbase = Tx { tx'inputs     = mempty
                  , tx'outputs    = mempty
                  , tx'dataInputs = mempty
                  }


-------------------------------------------------------------------------------
-- Node.

runNode :: UtxoPOWConfig t => POW.Block (UTXOBlock t) -> Cfg -> Maybe c -> IO ()
runNode genesisBlk Cfg{..} maybePrivK = do
  -- Acquire resources
  let net    = newNetworkTcp cfgPort
      netcfg = POW.NodeCfg { POW.nKnownPeers     = cfgKnownPeers
                           , POW.nConnectedPeers = cfgConnectedPeers
                           , POW.initialPeers    = cfgPeers
                           }
  withConnection (fromMaybe "" cfgDB) $ \conn ->
    withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv -> runUTXOT logEnv conn $ evalContT $ do
      -- Initialize PoW node
      (db, bIdx, sView) <- lift $ utxoStateView genesisBlk
      c0  <- lift $ POW.createConsensus db sView bIdx
      pow <- POW.startNode netcfg net db c0
      -- Report progress
      void $ liftIO $ forkIO $ do
        ch <- HControl.atomicallyIO (POW.chainUpdate pow)
        forever $ do bh <- POW.stateBH <$> HControl.awaitIO ch
                     print (POW.bhHeight bh, POW.bhBID bh)
                     print $ POW.retarget bh
      -- Start web API
      forM_ cfgWebAPI $ \port -> do
        utxoEnv <- lift ask
        let run :: UTXOT IO a -> Servant.Handler a
            run (UTXOT x) = liftIO $ runReaderT x utxoEnv
        liftIO $ hPutStrLn stderr $ "starting server at "++show port
        HControl.cforkLinkedIO $ do
          hPutStrLn stderr $ "server started at "++show port
          Warp.run port $ Servant.genericServeT run $ utxoRestServer (POW.mempoolAPI pow)
      -- Mining loop
      case maybePrivK of
        Just _privk -> do
          -- FIXME: add sensible spend script
          let script = coreProgToScript $ EPrim (PrimBool True)
              mine st t txs = do
                bData <- createUtxoCandidate st script txs
                pure $ POW.createCandidateBlock (POW.stateBH st) t bData
          HControl.cforkLinked $ POW.genericMiningLoop mine pow
        Nothing -> return ()
      -- Wait forever
      liftIO $ forever $ threadDelay maxBound

runLightNode :: UtxoPOWConfig t => POW.Block (UTXOBlock t) -> Cfg -> IO ()
runLightNode genesisBlk Cfg{..} = do
  -- Acquire resources
  let net    = newNetworkTcp cfgPort
      netcfg = POW.NodeCfg { POW.nKnownPeers     = cfgKnownPeers
                           , POW.nConnectedPeers = cfgConnectedPeers
                           , POW.initialPeers    = cfgPeers
                           }
  withConnection (fromMaybe "" cfgDB) $ \conn ->
    withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv ->
      runUTXOT logEnv conn $ evalContT $ do
        -- Initialize PoW node
        (db, bIdx, _sView) <- lift $ utxoStateView genesisBlk
        let c0 = POW.createLightConsensus genesisBlk bIdx
        pow <- POW.lightNode netcfg net db c0
        -- Report to stdout
        void $ liftIO $ forkIO $ do
          ch <- HControl.atomicallyIO $ POW.bestHeadUpdates pow
          forever $ do bh <- fst . POW._bestLightHead <$> HControl.awaitIO ch
                       print (POW.bhHeight bh, POW.bhBID bh)
                       print $ POW.retarget bh
        --
        liftIO $ forever $ threadDelay maxBound
  

----------------------------------------------------------------
-- REST API for PoW node
----------------------------------------------------------------

data UtxoRestAPI route = UtxoRestAPI
  { utxoMempoolAPI :: route
      :- Summary "Operations with Mempool"
      :> "mempool" :> Servant.ToServantApi (MempoolRestAPI (UTXOBlock TestNet))
  , endpointGetBox :: route
      :- Summary "Gets the box by identifier"
      :> "box" :> "get" :> Capture "box-id" BoxId :> Get '[JSON] (Maybe Box)
  , debugGetState :: route
      :- Summary "Get full state of blockchain"
      :> "debug" :> "state" :> "get" :> Get '[JSON] [(BoxId, Box)]
  }
  deriving (Generic)

utxoRestServer
  :: (MonadIO m, MonadReadDB m, UtxoPOWConfig t)
  => POW.MempoolAPI (UtxoState m t) -> UtxoRestAPI (Servant.AsServerT m)
utxoRestServer mempool = UtxoRestAPI
  { utxoMempoolAPI = Servant.toServant $ mempoolApiServer mempool
  , endpointGetBox = endpointGetBoxImpl
  , debugGetState  = debugGetStateImpl
  }

endpointGetBoxImpl :: (MonadIO m, MonadReadDB m) => BoxId -> m (Maybe Box)
endpointGetBoxImpl boxId = do
  r <- retrieveUTXOByBoxId boxId
  liftIO $ hPutStrLn stderr $ "getBoxEndpoint: boxid "++show boxId++", box "++show r
  return r

debugGetStateImpl :: (MonadIO m, MonadReadDB m) => m [(BoxId, Box)]
debugGetStateImpl = queryRO $ basicQuery_
  "SELECT box_id, box \
  \  FROM utxo_set \
  \  JOIN utxo_state ON live_utxo = utxo_id"




-- FIXME: Deal with orphans

instance Servant.FromHttpApiData BoxId where
  parseQueryParam = (\txt -> maybe (err txt) Right $ decodeBase58 txt) <=< Servant.parseQueryParam
    where
      err txt = Left $ "Failed to parse boxId from: " <> txt

instance Servant.ToHttpApiData BoxId where
  toQueryParam = encodeBase58
