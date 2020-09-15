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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
module Hschain.Utxo.Pow.App(
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

import qualified Data.Aeson as JSON

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Fix

import Data.Fixed

import Data.Functor.Classes (Show1)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Maybe

import Data.Text (Text)

import qualified Data.Vector as V

import Data.Word

import GHC.Generics

import Servant.API
import Servant.Server

import HSChain.Crypto.Classes
import HSChain.Crypto.SHA
import qualified HSChain.Crypto.Classes.Hash as Crypto
import qualified HSChain.POW as POWFunc
import qualified HSChain.PoW.Types as POWTypes
import qualified HSChain.PoW.Node as POWNode
import HSChain.Types.Merkle.Types

import HSChain.Crypto hiding (PublicKey)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Types
import HSChain.Types.Merkle.Types

import HSChain.PoW.Consensus
import qualified HSChain.PoW.Types as POWTypes

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

import Hschain.Utxo.Pow.App.Options (Options(..), readOptions)
import Hschain.Utxo.Pow.App.Types

-------------------------------------------------------------------------------
-- Executable part.

-- |Run the PoW node.
runNode :: Options -> IO ()
runNode (Options cfgConfigPath pathToGenesis nodeSecret optMine dbPath) = do
  genesisBlock <- readGenesis pathToGenesis
  db <- POWNode.inMemoryDB genesisBlock --POWNode.blockDatabase genesisBlock
  POWNode.runNode [cfgConfigPath] optMine undefined db
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
                  { box'id     = BoxId $ "reward:height:"++show blockHeight
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


makeStateView
  :: (MonadDB m, MonadThrow m, MonadIO m)
  => PrivKey Alg
  -> BlockIndex UTXOBlock
  -> StateOverlay
  -> StateView m UTXOBlock
makeStateView pk bIdx0 overlay = sview where
  bh0    = overlayTip overlay
  sview  = StateView
    { stateBID          = bhBID bh0
    -- FIXME: We need block index in order to be able to compute path
    --        from state to last known state
    , applyBlock        = \bIdx bh b -> runExceptT $ do
        -- Consistency checks
        unless (bhPrevious bh == Just bh0)  $ throwError $ InternalErr "BH mismatich"
        unless (bhBID bh      == blockID b) $ throwError $ InternalErr "BH don't match block"
        --
        let txList = merkleValue $ ubpBlockData $ ubProper $ blockData b
        -- Perform context free validation of all transactions in
        -- block
        () <- except
            $ mapM_ (validateTxContextFree @UTXOBlock) txList
        -- Now we need to fully verify each transaction and build new
        -- overlay for database
        overlay' <- hoist mustQueryRW $ do
          -- First we need to prepare path between block corresponding
          -- to current state of block
          pathInDB <- do
            Just stateBid <- retrieveCurrentStateBlock
            let Just bhState = lookupIdx stateBid bIdx
            makeBlockIndexPathM (retrieveCoinBlockTableID . bhBID)
              bhState (overlayBase overlay)
          -- Now we can just validate every TX and update overlay
          let activeOverlay = addOverlayLayer overlay
          case txList of
            []            -> return activeOverlay
            coinbase:rest -> do
              o' <- processCoinbaseTX (bhBID bh0) activeOverlay coinbase
              foldM (processTX pathInDB) o' rest
        return
          $ makeStateView pk bIdx
          $ fromMaybe (error "Coin: invalid BH in apply block")
          $ finalizeOverlay bh overlay'
    --
    , revertBlock = return $ makeStateView pk bIdx0 (rollbackOverlay overlay)
    --
    , flushState = mustQueryRW $ do
        -- Dump overlay content.
        dumpOverlay overlay
        -- Rewind state stored in the database from its current state
        -- to current head.
        Just bid <- retrieveCurrentStateBlock
        case bid `lookupIdx` bIdx0 of
          Nothing -> error "makeStateView: bad index"
          Just bh -> traverseBlockIndexM_ revertBlockDB applyBlockDB bh bh0
        do i <- retrieveCoinBlockTableID (bhBID bh0)
           basicExecute "UPDATE coin_state_bid SET state_block = ?" (Only i)
        return $ makeStateView pk bIdx0 (emptyOverlay bh0)
      -- FIXME: not implemented
    , checkTx = \tx@(TxCoin _ _ TxSend{..}) -> queryRO $ runExceptT $ do
        inputs <- forM txInputs $ \utxo -> do
          u <- getDatabaseUTXO NoChange utxo
          return (utxo,u)
        checkSpendability inputs tx
      --
    , createCandidateBlockData = \bh _ txlist -> queryRO $ do
        -- Create and process coinbase transaction
        let coinbase = signTX pk $ TxSend
              { txInputs  = [ UTXO 0 (coerce (bhBID bh)) ]
              , txOutputs = [ Unspent (publicKey pk) 100 ]
              }
            activeOverlay = addOverlayLayer overlay
        aOverlay <- runExceptT (processCoinbaseTX (bhBID bh0) activeOverlay coinbase) >>= \case
          Left  e -> error $ "Invalid coinbase: " ++ show e
          Right o -> return o
        -- Select transactions
        let selectTX []     _ = return []
            selectTX (t:ts) o = runExceptT (processTX NoChange o t) >>= \case
              Left  _  -> selectTX ts o
              Right o' -> (t:) <$> selectTX ts o'
        txs <- selectTX txlist aOverlay
        -- Create block!
        return UTXOBlock
          { ubNonce  = ""
          , ubProper = UTXOBlockProper
                      {
                      }
          }
    }
runApp :: IO ()
runApp = readOptions >>= runNode

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


