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
  , TestNet
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

import Data.Maybe
import Data.Yaml.Config (loadYamlSettings, requireEnv)
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
import HSChain.Types.Merkle.Types
import HSChain.Network.TCP
import HSChain.Logger
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

import Hschain.Utxo.Lang hiding (Height)
import Hschain.Utxo.Pow.App.Options (Command(..), readCommandOptions)
import Hschain.Utxo.Pow.App.Types


-------------------------------------------------------------------------------
-- Executable part.

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


data TestNet

instance UtxoPOWCongig TestNet where
  powConfig _ = POW.defaultPOWConfig


runApp :: IO ()
runApp = do
  hSetBuffering stderr NoBuffering
  hSetBuffering stdout NoBuffering
  -- Parse configuration
  command <- readCommandOptions
  case command of
    GenerateKey {..} -> do
      error "gen key is not done!"
    RunNode {..} -> do
      config <- loadYamlSettings runnode'config [] requireEnv
      runNode genesis config runnode'nodeSecret

genesis :: POW.Block (UTXOBlock TestNet)
genesis = POW.GBlock
  { blockHeight = POW.Height 0
  , blockTime   = POW.Time   0
  , prevBlock   = Nothing
  , blockData   = UTXOBlock
    { ubNonce  = ""
    , ubData   = merkled []
    , ubTarget = POW.Target $ 2^(256::Int) - 1
    }
  }


-------------------------------------------------------------------------------
-- Node.

runNode :: POW.Block (UTXOBlock TestNet) -> POW.Cfg -> Maybe c -> IO ()
runNode genesisBlk POW.Cfg{..} maybePrivK = do
  -- Acquire resources
  let net    = newNetworkTcp cfgPort
      netcfg = POW.NetCfg { POW.nKnownPeers     = 3
                          , POW.nConnectedPeers = 3
                          }
  withConnection (fromMaybe "" cfgDB) $ \conn ->
    withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv -> runUTXOT logEnv conn $ evalContT $ do
      (db, bIdx, sView) <- lift $ utxoStateView genesisBlk
      c0  <- lift $ POW.createConsensus db sView bIdx
      pow <- POW.startNode netcfg net cfgPeers db c0
      -- report progress
      void $ liftIO $ forkIO $ do
        ch <- HControl.atomicallyIO (POW.chainUpdate pow)
        forever $ do (bh,_) <- HControl.awaitIO ch
                     print (POW.bhHeight bh, POW.bhBID bh)
                     print $ POW.retarget bh
      utxoEnv <- lift ask
      liftIO $ hPutStrLn stderr $ "web API port: "++show cfgWebAPI
      forM_ cfgWebAPI $ \port -> do
        let run :: UTXOT IO a -> Servant.Handler a
            run (UTXOT x) = liftIO $ runReaderT x utxoEnv
        liftIO $ hPutStrLn stderr $ "starting server at "++show port
        HControl.cforkLinkedIO $ do
          hPutStrLn stderr $ "server started at "++show port
          Warp.run port $ Servant.genericServeT run $ utxoRestServer (POW.mempoolAPI pow)
      case maybePrivK of
        Just _privk -> do
          HControl.cforkLinked $ POW.genericMiningLoop pow
        Nothing -> return ()
      -- Wait forever
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
  :: (MonadIO m, MonadReadDB m)
  => POW.MempoolAPI m (UTXOBlock TestNet) -> UtxoRestAPI (Servant.AsServerT m)
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
