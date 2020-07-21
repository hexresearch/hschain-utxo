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

import Hschain.Utxo.Lang hiding (Height)
import Hschain.Utxo.State.Types
import Hschain.Utxo.State.React

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import qualified Hschain.Utxo.State.Query as S
import Hschain.Utxo.State.Types
import qualified Crypto.ECC.Edwards25519  as Ed

import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Interpreter as Sigma


import Hschain.Utxo.API.Rest

-------------------------------------------------------------------------------
-- Instances.

instance Crypto.CryptoHashable PublicKey where
  hashStep = genericHashStep "public key"

instance Crypto.CryptoHashable Ed.Point where
  hashStep x = hashStep (Ed.pointEncode x :: ByteString)

instance Crypto.CryptoHashable Ed.Scalar where
  hashStep x = hashStep (Ed.scalarEncode x :: ByteString)

instance Crypto.CryptoHashable Pico where
  hashStep = hashStep . serialise

-------------------------------------------------------------------------------
-- The Block.

-- ^A block proper. It does not contain nonce to solve PoW puzzle
-- but it contains all information about block.
data UTXOBlockProper f = UTXOBlockProper
  { ubpPrevious   :: !(POWTypes.BlockID UTXOBlock)
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

instance POWTypes.BlockData UTXOBlock where

  newtype BlockID UTXOBlock = UB'BID { fromUBBID :: Crypto.Hash SHA256 }
    deriving newtype
      (Show, Eq, Ord, Crypto.CryptoHashable, Serialise, ToJSON, FromJSON)

  type Tx UTXOBlock = Tx

  blockID b = let Hashed h = hashed b in UB'BID h
  validateHeader bh (POWTypes.Time now) header
    | POWTypes.blockHeight header == 0 = return True -- skip genesis check.
    | otherwise = do
      answerIsGood <- error "no puzzle check right now"
      return
        $ and
              [ answerIsGood
              , ubpTarget (ubProper $ POWTypes.blockData header) == POWTypes.retarget bh
              -- Time checks
              , t <= now + (2*60*60*1000)
              -- FIXME: Check that we're ahead of median time of N prev block
              ]
    where
      POWTypes.Time t = POWTypes.blockTime header

  validateBlock = const $ return True

  blockWork b = POWTypes.Work $ fromIntegral $ ((2^(256 :: Int)) `div`)
                              $ POWTypes.targetInteger $ ubpTarget $ ubProper
                              $ POWTypes.blockData b

  blockTargetThreshold b = POWTypes.Target $ POWTypes.targetInteger $
                          ubpTarget $ ubProper $ POWTypes.blockData b


instance MerkleMap UTXOBlock where
  merkleMap f ub = ub
                 { ubProper = (ubProper ub) { ubpData = mapMerkleNode f $ ubpData $ ubProper ub } }

instance POWTypes.Mineable UTXOBlock where
  adjustPuzzle b0@POWTypes.GBlock{..} = do
    (maybeAnswer, hash) <- liftIO $ POWFunc.solve [LBS.toStrict $ serialise blockData] powCfg
    let tgt = POWTypes.hash256AsTarget hash
    return (fmap (\answer -> b0 { POWTypes.blockData = blockData { ubNonce = answer} }) maybeAnswer, tgt)
    where
      h0 = POWTypes.toHeader b0
      powCfg = defaultPOWConfig
                       { POWFunc.powCfgTarget = POWTypes.targetInteger tgt }
      tgt = POWTypes.blockTargetThreshold b0
      defaultPOWConfig = POWFunc.defaultPOWConfig


-------------------------------------------------------------------------------
-- Executable part.

newtype Profitability = Profitability Rational
  deriving (Eq, Show)

instance Ord Profitability where
  compare (Profitability a) (Profitability b) = compare b a -- ^More profitable first.

data ProfitTx = PTx
  { ptxProfitability :: !Profitability
  , ptxTx            :: !Tx
  }
  deriving (Eq, Ord, Show)

data UTXONodeState = UTXONodeState
  { unsTransactions   :: !(Set.Set ProfitTx)
  , unsUTXOSet        :: !(Set.Set Box)
  , unsUTXORandomness :: !BS.ByteString
  , unsUTXOIndex      :: !Word64
  }
  deriving (Eq, Ord, Show)

-- |Run the PoW node.
runNode :: String -> String -> IO ()
runNode secretNodeName cfgConfigPath =
  POWNode.runNode [cfgConfigPath] optMine genesisBlock utxoViewStep getBlockToMine (UTXONodeState Set.empty Set.empty (getHashBytes (hash secretNodeName :: Hash SHA256)) 0)
  where
    getHashBytes :: Hash a -> BS.ByteString
    getHashBytes (Hash bytes) = bytes
    getBlockToMine bh st@(UTXONodeState{..}) = (POWTypes.GBlock
      { blockHeight = blockHeight
      , blockTime   = POWTypes.Time 0
      , prevBlock   = Just $! POWTypes.bhBID bh
      , blockData   = UTXOBlock {
                           ubNonce = BS.empty
                         , ubProper = UTXOBlockProper
                              { ubpPrevious   = POWTypes.bhBID bh
                              , ubpData       = merkled $ miningTx : []
                              , ubpTarget     = POWTypes.retarget bh
                              , ubpTime       = POWTypes.Time 0
                              }
                      }
      }, st)
      where
        blockHeight = succ $ POWTypes.bhHeight bh
        currentSecret = getHashBytes (hash (unsUTXORandomness, blockHeight) :: Hash SHA256) -- ^ currentSecret depends on height and randomness and can be computed knowing both. Height is open to wide world, randomness is not.
        currentSecretHash = getHashBytes (hash currentSecret :: Hash SHA256) -- ^This is what will be put into open world.
        miningTx = Tx
                   { tx'inputs  = V.empty   -- ^ List of identifiers of input boxes in blockchain 
                   , tx'outputs = V.fromList []    -- ^ List of outputs 
                   , tx'proof   = Nothing    -- ^ Proof of the resulting sigma expression 
                   , tx'args    = mempty            -- ^ Arguments for the scripts 
                   }
         

    optNodeName = error "optnodename"
    optMine = True
    genesisBlock = undefined
    utxoViewStep :: POWTypes.Block UTXOBlock -> UTXONodeState -> Maybe UTXONodeState
    utxoViewStep b m
      | otherwise                               = error "utxo view step is not done"
      where
        txs = merkleValue $ ubpData $ ubProper $ POWTypes.blockData b

runApp :: IO ()
runApp = putStrLn "running the app!"

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


