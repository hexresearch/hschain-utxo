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

import Servant.API
import Servant.Server
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
{-
-- |Run the PoW node.
runNode :: Options -> IO ()
runNode (Options cfgConfigPath pathToGenesis mbNodeSecret dbPath) = do
  genesisBlock <- readGenesis pathToGenesis
  db <- POW.inMemoryDB genesisBlock --POWNode.blockDatabase genesisBlock
  POW.runNode [cfgConfigPath] optMine undefined db
  where
    readGenesis :: FilePath -> IO (POWTypes.Block UTXOBlock)
    readGenesis = fmap (fromMaybe err) . readJson
      where
        err = error "Error: failed to read genesis"
    getHashBytes :: Hash a -> BS.ByteString
    getHashBytes (Hash bytes) = bytes
    getBlockToMine bh st@(UTXONodeState{..}) = (POWTypes.GBlock
      { blockHeight = blockHeight
      , blockTime   = POWTypes.Time 0
      , prevBlock   = Just $! POWTypes.bhBID bh
      , blockData   = UTXOBlock {
                           ubNonce = BS.empty
                         , ubProper = UTXOBlockProper
                              { ubpPrevious   = Just $! POWTypes.bhBID bh
                              , ubpData       = merkled $ miningTx : []
                              , ubpTarget     = POWTypes.retarget bh
                              , ubpTime       = POWTypes.Time 0
                              }
                      }
      }, st)
      where
        blockHeight = succ $ POWTypes.bhHeight bh
        withSecret secret = do
          Right proof <- newProof env (Fix $ SigmaPk publicKey) $ getTxBytes tx
          return $ [tx]
          where
            publicKey = getPublicKey secret
            env = proofEnvFromKeys [getKeyPair secret]

            box = Box
                  { box'id     = BoxId $ BS8.pack $ "reward:height:"++show blockHeight
                  , box'value  = 1
                  , box'script = mainScriptUnsafe $ pk' publicKey
                  , box'args   = mempty
                  }

            tx = Tx
                       { tx'inputs  = V.empty
                       , tx'outputs = V.fromList [box]
                       }

        currentSecret = getHashBytes (hash (unsUTXORandomness, blockHeight) :: Hash SHA256) -- ^ currentSecret depends on height and randomness and can be computed knowing both. Height is open to wide world, randomness is not.
        currentSecretHash = getHashBytes (hash currentSecret :: Hash SHA256) -- ^This is what will be put into open world.
        rewardBox = error "is not done"
        miningTx = Tx
                   { tx'inputs  = V.empty   -- ^ List of identifiers of input boxes in blockchain 
                   , tx'outputs = V.fromList []    -- ^ List of outputs 
                   }
         

    optNodeName = error "optnodename"
    utxoViewStep :: POWTypes.Block UTXOBlock -> UTXONodeState -> Maybe UTXONodeState
    utxoViewStep b m
      | otherwise                               = error "utxo view step is not done"
      where
        txs = merkleValue $ ubpData $ ubProper $ POWTypes.blockData b
-}

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
        withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv -> runUTXOT logEnv conn $ evalContT $ do
          (db, bIdx, sView) <- lift $ utxoStateView genesis
          c0  <- lift $ POW.createConsensus db sView bIdx
          pow <- POW.startNode netcfg net cfgPeers db c0
      -- report progress
          void $ liftIO $ forkIO $ do
            ch <- HControl.atomicallyIO (POW.chainUpdate pow)
            forever $ do (bh,_) <- HControl.awaitIO ch
                         print (POW.bhHeight bh, POW.bhBID bh)
                         print $ POW.retarget bh
          forM_ cfgWebAPI $ \port -> do
            dict <- lift $ UTXOT ask
            let run :: UTXOT IO a -> Handler a
                run (UTXOT m) = liftIO $ runReaderT m dict
            HControl.cforkLinkedIO $ Warp.run port $ genericServeT run $ coinServer (POW.mempoolAPI pow)
          case runnode'nodeSecret of
            Just pk -> do
              HControl.cforkLinked $ POW.genericMiningLoop pow
            Nothing -> return ()
          -- Wait forever
          liftIO $ forever $ threadDelay maxBound

readGenesis :: FilePath -> IO (POW.Block UTXOBlock)
readGenesis = fmap (fromMaybe err) . readJson
  where
    err = error "Error: failed to read genesis"

-- | Server implementation for 'UtxoAPI'
utxoServer :: ServerT UtxoAPI ServerM
utxoServer =
       postTxEndpoint                -- posts transaction
  :<|> getBoxEndpoint                -- gets box by id
  :<|> getBoxBalanceEndpoint         -- reads balance for a box
  :<|> getTxSigmaEndpoint            -- executes script to sigma-expression without commiting
  :<|> getEnvEndpoint                -- reads blockchain environment
  :<|> getStateEndpoint              -- reads whole state (for debug only)
  :<|> getUtxosEndpoint              -- reads list of all available UTXOs
  :<|> hasUtxoEndpoint               -- is UTXO exists (available to spend)
  :<|> readBlockEndpoint             -- reads block at the given height
  :<|> readBlockchainHeightEndpoint  -- reads current height of the blockchain

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
data AppEnv = AppEnv
            { appEnvMempool           :: POW.MempoolAPI ServerM UTXOBlock }

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


