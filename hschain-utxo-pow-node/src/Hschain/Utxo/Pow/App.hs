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
      POW.Cfg{..} <- loadYamlSettings runnode'config [] requireEnv
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
          forM_ cfgWebAPI $ \port -> do
            let api = Proxy :: Proxy UtxoAPI
                run :: ServerM a -> Servant.Handler a
                run (UTXOT x) = liftIO $ runReaderT x endpointUTXOEnv
            HControl.cforkLinkedIO $ Warp.run port $ Servant.serve api $ Servant.hoistServer api undefined undefined
          case runnode'nodeSecret of
            Just privk -> do
              HControl.cforkLinked $ POW.genericMiningLoop pow
            Nothing -> return ()
          -- Wait forever
          liftIO $ forever $ threadDelay maxBound

readGenesis :: FilePath -> IO (POW.Block UTXOBlock)
readGenesis = fmap (fromMaybe err) . readJson
  where
    err = error "Error: failed to read genesis"

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
  --bch <- readBoxChain
  --GetEnvResponse <$> getEnv bch
  return undefined

getStateEndpoint :: ServerM BoxChain
getStateEndpoint =
  --readBoxChain
  return undefined

getUtxosEndpoint :: ServerM [BoxId]
getUtxosEndpoint = return []

hasUtxoEndpoint :: BoxId -> ServerM Bool
hasUtxoEndpoint boxId = return False

readBlockEndpoint :: Int -> ServerM (Maybe [Tx])
readBlockEndpoint height = return Nothing

readBlockchainHeightEndpoint :: ServerM Int
readBlockchainHeightEndpoint = return 0

getBoxEndpoint :: BoxId -> ServerM (Maybe Box)
getBoxEndpoint boxId = return Nothing

-- |Application environment.
data AppEnv m = AppEnv
            { appEnvMempool           :: POW.MempoolAPI (UTXOT m) UTXOBlock }

type ServerM a = UTXOT IO a

-- | Reads current state of the block chain
readBoxChain :: ServerM BoxChain
readBoxChain =
  --readBoxChainState
  return undefined


--------------------------------------------------
------ bchain store operations

writeTx :: Monad m => Tx -> m (Maybe TxHash)
writeTx tx = do
  --Bchain{..} <- askBchain
  --liftIO $ fmap ((\(Crypto.Hashed (Crypto.Hash h)) -> TxHash h)) <$>
  --  ((\cursor -> pushTransaction cursor tx) =<< getMempoolCursor bchain'mempool)
  pure Nothing

readBlock :: Monad m => Int -> m (Maybe [Tx])
readBlock height = do
  --Bchain{..} <- askBchain
  --liftIO $ do
  --  mb <- runDBT bchain'conn $ queryRO $ retrieveBlock (Height $ fromIntegral height)
  --  pure $ unBData . merkleValue . blockData <$> mb
  pure Nothing

blockchainSize :: Monad m => m Int
blockchainSize = do
  --Bchain{..} <- askBchain
  --liftIO $ do
  --  Height h <- runDBT bchain'conn $ queryRO blockchainHeight
  --  pure $! fromIntegral h
  return 0

readBoxChainState :: (Monad m) => m BoxChain
readBoxChainState = do
  --Bchain{..} <- askBchain
  --liftIO $ merkleValue . snd <$> bchCurrentState bchain'store
  return undefined

waitForTx :: (Monad m) => m (TxHash -> m Bool)
waitForTx = do
  --Bchain{..} <- askBchain
  --fmap liftIO <$> liftIO bchain'waitForTx
  return (const $ return False)

postTxWait :: (Monad m) => Tx -> m (Maybe TxHash)
postTxWait tx = do
  -- We start listening before sending transaction to mempool to avoid
  -- race when tx is commited before we start listening
  listener <- waitForTx
  runMaybeT $ do
    h <- MaybeT $ writeTx tx
    guard =<< lift (listener h)
    -- lift $ incMetricSurelyPostedTx tx
    return h


