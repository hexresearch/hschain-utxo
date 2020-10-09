-- |Hschain.Utxo.App.hs
--
-- Full fledged PoW consensus node, with external REST API.
--
-- Copyright (C) 2020 ...

-- Please keep switched off -Wno-orphans.
-- We need an instance of CryptoHashable of elliptic curve
-- scalar (Ed.Scalarbelow ) provided by very much external package.
-- We cannot fork that package and add an instance there.

{-# OPTIONS  -Wno-orphans               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RecordWildCards, StandaloneDeriving #-}

module Hschain.Utxo.Pow.App(
    runApp
  , runNode
  , Genesis
) where

import Hex.Common.Aeson
import Hex.Common.Yaml

import Codec.Serialise

import Control.Concurrent

import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe

import qualified Data.Aeson as JSON

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8

import Data.Fix

import Data.Fixed

import Data.Functor.Classes (Show1)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Maybe

import Data.Text (Text)

import qualified Data.Vector as V

import Data.Word

import Data.Yaml.Config (loadYamlSettings, requireEnv)

import GHC.Generics

import qualified Servant.API              as Servant
import qualified Servant.Server           as Servant
import qualified Network.Wai.Handler.Warp as Warp

import qualified System.Environment as SE
import System.IO

import HSChain.Crypto.Classes
import HSChain.Crypto.SHA
import qualified HSChain.Crypto.Classes.Hash as Crypto
import HSChain.Types.Merkle.Types

import qualified HSChain.Control.Channels as HControl
import qualified HSChain.Control.Class    as HControl
import qualified HSChain.Control.Util     as HControl
import HSChain.Crypto hiding (PublicKey)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Types.Merkle.Types

import HSChain.Network.TCP

import qualified HSChain.POW            as POW
import qualified HSChain.PoW.P2P        as POW
import qualified HSChain.PoW.P2P.Types  as POW
import qualified HSChain.PoW.Consensus  as POW
import qualified HSChain.PoW.BlockIndex as POW
import qualified HSChain.PoW.Node       as POW
import qualified HSChain.PoW.Types      as POW

import HSChain.Logger
import HSChain.Store

import Hschain.Utxo.Lang hiding (Height)
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.State.React
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.State.Types

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import qualified Hschain.Utxo.State.Query as S
import Hschain.Utxo.State.Types
import qualified Crypto.ECC.Edwards25519  as Ed

import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Interpreter as Sigma


import Hschain.Utxo.API.Rest

import Hschain.Utxo.Pow.App.Options (Command(..), readCommandOptions)
import Hschain.Utxo.Pow.App.Types

-------------------------------------------------------------------------------
-- Executable part.

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
      --secret <- maybe (return Nothing) (fmap (Just . read) . SE.getEnv) options'nodeSecret
      genesis <- readGenesis runnode'genesis
      config <- loadYamlSettings runnode'config [] requireEnv
      runNode genesis config runnode'nodeSecret

readGenesis :: FilePath -> IO (POW.Block UTXOBlock)
readGenesis = fmap (fromMaybe err) . readJson
  where
    err = error "Error: failed to read genesis"

-- |Create a genesis block with single transaction
createGenesis :: Money -> PublicKey -> IO Genesis
createGenesis amount owner = do
  time <- POW.getCurrentTime
  let tx = Tx
           { tx'inputs = V.empty
           , tx'outputs = V.fromList
                            [ Box
                                { box'id     = BoxId $ hash $ BS.pack $ map (fromIntegral . fromEnum) $ "genesis:"++show time
                                , box'value  = amount
                                , box'script = mainScriptUnsafe $ pk' owner
                                , box'args   = mempty
                                }
                            ]
           }
  return $ POW.GBlock
           { POW.blockHeight = POW.Height 0
           , POW.blockTime   = time
           , POW.prevBlock   = Nothing
           , POW.blockData   = UTXOBlock
               { ubNonce = ""
               , ubProper = UTXOBlockProper
                   { ubpPrevious  = Nothing
                   , ubpData      = merkled [tx]
                   , ubpTime      = time
                   , ubpTarget    = POW.Target $ 2^256 - 1
                   }
               }
           }
  where


-------------------------------------------------------------------------------
-- Node.

runNode :: POW.Block UTXOBlock -> POW.Cfg -> Maybe c -> IO ()
runNode genesis config@POW.Cfg{..} maybePrivK = do
      -- Acquire resources
      let net    = newNetworkTcp cfgPort
          netcfg = POW.NetCfg { POW.nKnownPeers     = 3
                              , POW.nConnectedPeers = 3
                              }
      withConnection (fromMaybe "" cfgDB) $ \conn ->
        let unassignedMempool = error "mempool is not assigned!"
        in withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv -> runUTXOT logEnv conn unassignedMempool $ evalContT $ do
          (db, bIdx, sView) <- lift $ utxoStateView genesis
          c0  <- lift $ POW.createConsensus db sView bIdx
          pow <- POW.startNode netcfg net cfgPeers db c0
      -- report progress
          void $ liftIO $ forkIO $ do
            ch <- HControl.atomicallyIO (POW.chainUpdate pow)
            forever $ do (bh,_) <- HControl.awaitIO ch
                         print (POW.bhHeight bh, POW.bhBID bh)
                         print $ POW.retarget bh
          utxoEnv <- lift ask
          let endpointUTXOEnv = utxoEnv { ueMempool = POW.mempoolAPI pow }
          liftIO $ hPutStrLn stderr $ "web API port: "++show cfgWebAPI
          forM_ cfgWebAPI $ \port -> do
            let api = Proxy :: Proxy UtxoAPI
                run :: ServerM a -> Servant.Handler a
                run (UTXOT x) = liftIO $ runReaderT x endpointUTXOEnv
            liftIO $ hPutStrLn stderr $ "starting server at "++show port
            HControl.cforkLinkedIO $ do
              hPutStrLn stderr $ "server started at "++show port
              Warp.run port $ Servant.serve api $ Servant.hoistServer api run utxoServer
          case maybePrivK of
            Just privk -> do
              HControl.cforkLinked $ POW.genericMiningLoop pow
            Nothing -> return ()
          -- Wait forever
          liftIO $ forever $ threadDelay maxBound

type Genesis = POW.Block UTXOBlock

-- | Server implementation for 'UtxoAPI'
utxoServer :: Servant.ServerT UtxoAPI (UTXOT IO)
utxoServer =
               postTxEndpoint                -- posts transaction
  Servant.:<|> getBoxEndpoint                -- gets box by id
  Servant.:<|> getBoxBalanceEndpoint         -- reads balance for a box
  Servant.:<|> getTxSigmaEndpoint            -- executes script to sigma-expression without commiting
  Servant.:<|> getEnvEndpoint                -- reads blockchain environment
  Servant.:<|> getStateEndpoint              -- reads whole state (for debug only)
  Servant.:<|> getUtxosEndpoint              -- reads list of all available UTXOs
  Servant.:<|> hasUtxoEndpoint               -- is UTXO exists (available to spend)
  Servant.:<|> readBlockEndpoint             -- reads block at the given height
  Servant.:<|> readBlockchainHeightEndpoint  -- reads current height of the blockchain

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
  bch <- readBoxChain
  return $ GetEnvResponse $ getEnv bch

getStateEndpoint :: ServerM BoxChain
getStateEndpoint =
  readBoxChain

getUtxosEndpoint :: ServerM [BoxId]
getUtxosEndpoint = return []

hasUtxoEndpoint :: BoxId -> ServerM Bool
hasUtxoEndpoint boxId = return False

readBlockEndpoint :: Int -> ServerM (Maybe [Tx])
readBlockEndpoint height = return Nothing

readBlockchainHeightEndpoint :: ServerM Int
readBlockchainHeightEndpoint = return 0

getBoxEndpoint :: BoxId -> ServerM (Maybe Box)
getBoxEndpoint boxId = do
  r <- retrieveUTXOByBoxId boxId
  liftIO $ hPutStrLn stderr $ "getBoxEndpoint: boxid "++show boxId++", box "++show r
  return r

type ServerM a = UTXOT IO a

-- | Reads current state of the block chain
readBoxChain :: ServerM BoxChain
readBoxChain =
  readBoxChainState


--------------------------------------------------
------ bchain store operations

writeTx :: Tx -> UTXOT IO (Maybe TxHash)
writeTx tx = do
  POW.MempoolAPI {..} <- fmap ueMempool ask
  liftIO $ hPutStrLn stderr $ "posting transaction "++show tx
  HControl.sinkIO postTransaction tx
  --Bchain{..} <- askBchain
  --liftIO $ fmap ((\(Crypto.Hashed (Crypto.Hash h)) -> TxHash h)) <$>
  --  ((\cursor -> pushTransaction cursor tx) =<< getMempoolCursor bchain'mempool)
  return $ Just $ TxHash h
  where
    Hash h = hash tx :: Hash SHA256

readBlock :: Int -> UTXOT IO (Maybe [Tx])
readBlock height = do
  --Bchain{..} <- askBchain
  --liftIO $ do
  --  mb <- runDBT bchain'conn $ queryRO $ retrieveBlock (Height $ fromIntegral height)
  --  pure $ unBData . merkleValue . blockData <$> mb
  pure Nothing

blockchainSize :: UTXOT IO Int
blockchainSize = do
  --Bchain{..} <- askBchain
  --liftIO $ do
  --  Height h <- runDBT bchain'conn $ queryRO blockchainHeight
  --  pure $! fromIntegral h
  return 0

readBoxChainState :: UTXOT IO BoxChain
readBoxChainState = do
  --Bchain{..} <- askBchain
  --liftIO $ merkleValue . snd <$> bchCurrentState bchain'store
  return $ BoxChain Map.empty 0

postTxWait :: Tx -> UTXOT IO (Maybe TxHash)
postTxWait tx = do
  -- We start listening before sending transaction to mempool to avoid
  -- race when tx is commited before we start listening
  h <- writeTx tx
  liftIO $ hPutStrLn stderr $ "posted tx "++show tx++", hash "++show h
  return h


