-- |Hschain.Utxo.Pow.App.Types
--
-- Full fledged PoW consensus node, with external REST API.
--
-- Copyright (C) 2020 ...
{-# LANGUAGE DataKinds                                       #-}
{-# LANGUAGE DeriveAnyClass, DerivingVia, DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts                                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications    #-}
{-# LANGUAGE MultiWayIf, ScopedTypeVariables, TypeOperators  #-}
{-# LANGUAGE UndecidableInstances                            #-}
-- SQL-related orphans
{-# OPTIONS_GHC -Wno-orphans #-}
module Hschain.Utxo.Pow.App.Types where

import Hex.Common.Aeson
import Hex.Common.Yaml
import Hex.Common.Lens

import Codec.Serialise

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Morph (hoist)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Maybe

import Data.Coerce
import qualified Data.Aeson as JSON

import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Fix

import Data.Fixed

import Data.Functor.Classes (Show1)

import Data.Generics.Product.Typed (typed)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.List (foldl')

import Data.Maybe

import Data.Text (Text)

import qualified Data.Vector as V

import Data.Word

import Database.SQLite.Simple ((:.)(..))
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.Ok        as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.FromRow   as SQL
import qualified Database.SQLite.Simple.ToRow     as SQL

import GHC.Generics (Generic)

import Katip (LogEnv, Namespace)

-- import Servant.API
-- import Servant.Server

import HSChain.Crypto.Classes
import HSChain.Crypto.SHA
import qualified HSChain.Crypto.Classes.Hash as Crypto
import qualified HSChain.POW            as POW
import qualified HSChain.PoW.Consensus  as POW
import qualified HSChain.PoW.BlockIndex as POW
import qualified HSChain.PoW.P2P        as POW
import qualified HSChain.PoW.Types      as POW
import qualified HSChain.PoW.Node       as POW
import HSChain.Types.Merkle.Types

import HSChain.Control.Class
import HSChain.Crypto hiding (PublicKey)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Types.Merkle.Types

import HSChain.Logger

import HSChain.Store.Query

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

import qualified Debug.Trace as Debug

$(makeLensesWithL ''TxArg)
$(makeLensesWithL ''BoxInput)
$(makeLensesWithL ''Box)
ibox'boxL :: Lens' IBox Box
ibox'boxL = lens (\(IBox _ b) -> b) (\(IBox i _) b -> IBox i b)

----------------------------------------------------------------
-- The Block.
----------------------------------------------------------------

-- ^ The block. Nonce (puzzle answer) is prepended to the header as it
--   is more secure - prevents selfish pool mining utilization.  When
--   nonce is before block and answer is computed as a hash of nonce
--   and header, then each (pool) miner sees what is previous block
--   and can detect selfish mining and mining pool utilization for
--   currencies it does not support.
--
--   A block proper. It does not contain nonce to solve PoW puzzle but
--   it contains all information about block.
data UTXOBlock f = UTXOBlock
  { ubData   :: !(MerkleNode f SHA256 [Tx])
  , ubTarget :: !POW.Target
  , ubNonce  :: !ByteString
  }
  deriving stock (Generic)

deriving stock instance (Show1 f)    => Show (UTXOBlock f)
deriving stock instance (IsMerkle f) => Eq   (UTXOBlock f)
instance Serialise (UTXOBlock Identity)
instance Serialise (UTXOBlock Proxy)

instance IsMerkle f => JSON.FromJSON (UTXOBlock f) where
  parseJSON = JSON.withObject "UtxoBlock" $ \bp ->
    UTXOBlock
      <$> (bp JSON..: "data")
      <*> (POW.Target <$> bp JSON..: "target")
      <*> ((\(ViaBase58 s :: ViaBase58 "UTXOBlock" ByteString) -> s) <$> (bp JSON..: "nonce"))
instance IsMerkle f => JSON.ToJSON (UTXOBlock f) where
  toJSON (UTXOBlock d (POW.Target t) b) =
    JSON.object
      [ "data"   JSON..= d
      , "target" JSON..= t
      , "nonce"  JSON..= ViaBase58 b
      ]

$(makeLensesWithL ''UTXOBlock)
$(makeLensesWithL ''POW.GBlock)

----------------------------------------------------------------
-- BlockData instance
----------------------------------------------------------------

instance POW.BlockData UTXOBlock where
  --
  newtype BlockID UTXOBlock = UB'BID { fromUBBID :: Crypto.Hash SHA256 }
    deriving newtype
      (Show, Eq, Ord, Crypto.CryptoHashable, Serialise, ToJSON, FromJSON, ByteRepr)
    deriving (SQL.FromField, SQL.ToField) via ByteRepred (POW.BlockID UTXOBlock)

  type Tx UTXOBlock = Tx

  newtype TxID UTXOBlock = UTXOTxID (Hash SHA256)
    deriving newtype ( Show, Eq, Ord, CryptoHashable, Serialise, ByteRepr
                     , JSON.ToJSON, JSON.FromJSON)
    deriving (SQL.FromField, SQL.ToField) via ByteRepred (POW.TxID UTXOBlock)

  data BlockException UTXOBlock =
                                  WrongAnswer
                                | WrongTarget
                                | AheadOfTime
                                | BadCoinbase String
                                | BadTx String
                                | EmptyBlock
                                | InternalErr String
    deriving stock    (Show,Generic)
    deriving anyclass (Exception,JSON.ToJSON)

  txID    = UTXOTxID . hash
  blockID = computeBlockID
  blockTransactions = merkleValue . ubData . POW.blockData

  validateHeader bh (POW.Time now) header
    | POW.blockHeight header == 0 = return $ Right () -- skip genesis check.
    | otherwise = do
      answerIsGood <- {-Debug.trace "no puzzle check right now" $ -}return True
      return $ if
         | not answerIsGood -> Left WrongAnswer
         | ubTarget (POW.blockData header) /= POW.retarget bh
                            -> Left WrongTarget
         | t > now + (2 * 60 * 60 * 1000)
                            -> Left AheadOfTime
         | otherwise        -> Right ()
    where
      POW.Time t = POW.blockTime header

  validateBlock = const $ return $ Right ()

  validateTxContextFree = validateTransactionContextFree

  blockWork b = POW.Work $ fromIntegral $ ((2^(256 :: Int)) `div`)
                              $ POW.targetInteger $ ubTarget
                              $ POW.blockData b

  blockTargetThreshold b = POW.Target $ POW.targetInteger $ ubTarget $ POW.blockData b


instance MerkleMap UTXOBlock where
  merkleMap f ub = ub { ubData = mapMerkleNode f $ ubData ub }

computeBlockID :: POW.GBlock UTXOBlock f -> POW.BlockID UTXOBlock
computeBlockID b
  = UB'BID . hashBuilder
  $ blockIdBuilder b
 <> hashStep (ubNonce $ POW.blockData b)

-- | Builder without which doesn't include nonce
blockIdBuilder (POW.GBlock{blockData = UTXOBlock{..}, ..})
  = hashStep (Crypto.UserType "utxo" "Block")
 <> hashStep blockHeight
 <> hashStep blockTime
 <> hashStep prevBlock
 <> hashStep ubData
 <> hashStep ubTarget


-- |The Reward.
miningRewardAmount :: Money
miningRewardAmount = 100

instance POW.Mineable UTXOBlock where
  adjustPuzzle b0@POW.GBlock{..} = do
    (maybeAnswer, hash) <- liftIO $ POW.solve [bs] powCfg
    Debug.traceShowM (isJust maybeAnswer)
    let tgt = POW.hash256AsTarget hash
    return ( do answer <- maybeAnswer
                pure $ b0 & blockDataL . ubNonceL .~ answer
           , tgt
           )
    where
      bs     = LBS.toStrict $ toLazyByteString $ blockIdBuilder b0
      powCfg = POW.defaultPOWConfig
        { POW.powCfgTarget = POW.targetInteger tgt
        }
      tgt = POW.blockTargetThreshold b0

data UTXOEnv = UTXOEnv
  { ueLogEnv      :: !LogEnv
  , ueNamespace   :: !Namespace
  , ueConn        :: !(Connection 'RW)
  , ueMempool     :: POW.MempoolAPI (UTXOT IO) UTXOBlock
  }
  deriving (Generic)

newtype UTXOT m a = UTXOT (ReaderT UTXOEnv m a)
  deriving newtype ( Functor, Applicative, Monad, MonadIO
                   , MonadCatch, MonadThrow, MonadMask, MonadFork, MonadReader UTXOEnv)
  deriving (MonadLogger)          via LoggerByTypes  (ReaderT UTXOEnv m)
  deriving (MonadDB, MonadReadDB) via DatabaseByType (ReaderT UTXOEnv m)


runUTXOT :: LogEnv -> Connection 'RW -> POW.MempoolAPI (UTXOT IO) UTXOBlock -> UTXOT m a -> m a
runUTXOT logenv conn mempool (UTXOT act) = runReaderT act (UTXOEnv logenv mempty conn mempool)

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

initialUTXONodeState :: UTXONodeState
initialUTXONodeState = UTXONodeState
  { unsTransactions   = Set.empty
  , unsUTXOSet        = Set.empty
  , unsUTXORandomness = BS.pack $ replicate 32 71
  , unsUTXOIndex      = 0
  }

----------------------------------------------------------------
-- Blockchain state management
----------------------------------------------------------------

-- | Create triple: block storage, block index, state view using
--   current database state.
utxoStateView
  :: (MonadThrow m, MonadDB m, MonadIO m, MonadDB m)
  => POW.Block UTXOBlock
  -> m (POW.BlockDB m UTXOBlock, POW.BlockIndex UTXOBlock, POW.StateView m UTXOBlock)
utxoStateView genesis = do
  initUTXODB
  storeUTXOBlock genesis
  bIdx <- POW.buildBlockIndex utxoBlockDB
  st   <- mustQueryRW $ initializeStateView genesis bIdx
  return (utxoBlockDB, bIdx, st)

initializeStateView
  :: (MonadDB m, MonadThrow m, MonadQueryRW q,  MonadIO m)
  => POW.Block UTXOBlock            -- ^ Genesis block
  -> POW.BlockIndex UTXOBlock       -- ^ Block index
  -> q (POW.StateView m UTXOBlock)
initializeStateView genesis bIdx = do
  retrieveCurrentStateBlock >>= \case
    Just bid -> do let Just bh = {-Debug.trace ("looking up bid "++show bid) $ -}POW.lookupIdx bid bIdx
                   return $ makeStateView bIdx (emptyOverlay bh)
    -- We need to initialize state table
    Nothing  -> do
      let bid     = POW.blockID genesis
          Just bh = POW.lookupIdx bid bIdx
      basicExecute
        "INSERT INTO utxo_state_bid SELECT blk_id,0 FROM utxo_blocks WHERE bid = ?"
        (Only bid)
      return $ makeStateView bIdx (emptyOverlay bh)

makeStateView
  :: (MonadDB m, MonadThrow m, MonadIO m)
  => POW.BlockIndex UTXOBlock
  -> StateOverlay
  -> POW.StateView m UTXOBlock
makeStateView bIdx0 overlay = sview where
  bh0    = overlayTip overlay
  sview  = POW.StateView
    { stateBID    = POW.bhBID bh0
    , applyBlock  = applyUtxoBlock overlay
    , revertBlock = return $ makeStateView bIdx0 (rollbackOverlay overlay)
    --
    , flushState = mustQueryRW $ do
        -- Dump overlay content.
        dumpOverlay overlay
        -- Rewind state stored in the database from its current state
        -- to current head.
        Just bid <- retrieveCurrentStateBlock
        case bid `POW.lookupIdx` bIdx0 of
          Nothing -> error "makeStateView: bad index"
          Just bh -> POW.traverseBlockIndexM_ revertBlockDB applyBlockDB bh bh0
        do i <- retrieveUTXOBlockTableID (POW.bhBID bh0)
           basicExecute "UPDATE utxo_state_bid SET state_block = ?" (Only i)
        return $ makeStateView bIdx0 (emptyOverlay bh0)
      -- FIXME: not implemented
    , checkTx = \tx@Tx{..} -> queryRO $ runExceptT $ do
        undefined
        -- inputs <- forM tx'inputs $ \bir@BoxInputRef{..} -> do
        --   u <- getDatabaseBox POW.NoChange bir
        --   return (boxInputRef'id, u)
        -- checkSpendability inputs tx
      --
    , createCandidateBlockData = createUtxoCandidate overlay bIdx0
    }


-- | Validate and apply block to current state view. Returns Left on failed validation
applyUtxoBlock
  :: (MonadDB m, MonadThrow m, MonadIO m)
  => StateOverlay
  -> POW.BlockIndex UTXOBlock
  -> POW.BH UTXOBlock
  -> POW.GBlock UTXOBlock Identity
  -> m (Either (POW.BlockException UTXOBlock) (POW.StateView m UTXOBlock))
applyUtxoBlock overlay bIdx bh b = runExceptT $ do
  when (null txList) $ throwError EmptyBlock
  -- Consistency checks
  unless (POW.bhPrevious bh ==      Just bh0) $ throwError $ InternalErr "BH mismatich"
  unless (POW.bhBID bh      == POW.blockID b) $ throwError $ InternalErr "BH don't match block"
  -- Perform context free validation of all transactions in block
  --
  -- FIXME: We're doing nothing here so far
  () <- except
      $ mapM_ POW.validateTxContextFree txList
  -- Now we need to fully verify each transaction and build new
  -- overlay for database
  overlay' <- hoist mustQueryRW $ do
    -- First we need to prepare path between block corresponding
    -- to current state and base of overlay
    pathInDB <- do
      Just stateBid <- retrieveCurrentStateBlock
      let Just bhState = POW.lookupIdx stateBid bIdx
      POW.makeBlockIndexPathM (retrieveUTXOBlockTableID . POW.bhBID)
        bhState (overlayBase overlay)
    -- Fetch boxes for all transaction in block. Coinbase is treated
    -- specially.
    --
    -- NOTE: we don't protect from double-spending same box in one or
    --       in different transactions here
    txArgs <- case txList of
      []             -> throwError $ InternalErr "Empty block"
      (coinbase:txs) -> do t  <- coinbaseTxArg (POW.bhBID <$> POW.bhPrevious bh) env coinbase
                           ts <- mapM (buildTxArg (getDatabaseBox pathInDB) env) txs
                           pure (t : ts)
    -- Check that block preserves balance
    checkBalance txArgs
    -- Check that each input of every transaction could be spent.
    either (throwError . InternalErr . T.unpack) pure
      $ mapM_ evalProveTx txArgs
    -- Mark every input as spend and create outputs
    foldM (processTX pathInDB) overlay0 txArgs
  return
    $ makeStateView bIdx
    $ fromMaybe (error "UTXO: invalid BH in apply block")
    $ finalizeOverlay bh overlay'
  where
    env      = Env $ fromIntegral h where POW.Height h = POW.blockHeight b
    bh0      = overlayTip overlay
    txList   = merkleValue $ ubData $ POW.blockData b
    overlay0 = addOverlayLayer overlay


createUtxoCandidate
  :: (MonadReadDB m, MonadIO m)
  => StateOverlay
  -> POW.BlockIndex UTXOBlock
  -> POW.BH UTXOBlock
  -> p
  -> [Tx]
  -> m (UTXOBlock Identity)
createUtxoCandidate overlay bIdx bh _time txlist = queryRO $ do
  pathInDB <- do
    Just stateBid <- retrieveCurrentStateBlock
    let Just bhState = POW.lookupIdx stateBid bIdx
    POW.makeBlockIndexPathM (retrieveUTXOBlockTableID . POW.bhBID)
      bhState (overlayBase overlay)
  -- Select transaction
  let tryTX o tx = do
        txArg <- buildTxArg (getDatabaseBox pathInDB) env tx
        -- FIXME: Tx preserves value (module commission)
        --
        either (throwError . InternalErr . T.unpack) pure
          $ evalProveTx txArg
        o' <- processTX pathInDB o txArg
        return (txArg, o')
  -- Select transactions
  let selectTX []     _ = return []
      selectTX (t:ts) o = runExceptT (tryTX o t) >>= \case
        Left  _ -> selectTX ts o
        Right (tA,o') -> ((tA,t):) <$> selectTX ts o'
      --
  txList <- selectTX txlist $ addOverlayLayer overlay
  -- FIXME: Compute commisions
  -- -- Create and process coinbase transaction
  let coinbaseBox = Box { box'value  = miningRewardAmount
                        , box'script = mainScriptUnsafe true
                        , box'args   = mempty
                        }
      coinbase = Tx { tx'inputs  = V.singleton BoxInputRef
                        { boxInputRef'id      = coerce (POW.bhBID bh)
                        , boxInputRef'args    = mempty
                        , boxInputRef'proof   = Nothing
                        , boxInputRef'sigMask = SigAll
                        }
                    , tx'outputs = V.fromList [coinbaseBox]
                    }
      blockTxs = coinbase : fmap snd txList
  -- Create block!

  return UTXOBlock
    { ubNonce  = ""
    , ubData   = merkled blockTxs
    , ubTarget = POW.retarget bh
    }
  where
    env = Env $ fromIntegral (h + 1) where POW.Height h = POW.bhHeight bh


-- | Create TxArg for coinbase transaction. It's treated differently
--   so we couldn't use 'buildTxArg'
coinbaseTxArg
  :: (MonadError (POW.BlockException UTXOBlock) m)
  => Maybe (POW.BlockID UTXOBlock) -> Env -> Tx -> m TxArg
coinbaseTxArg Nothing    _   _  = throwError $ InternalErr "No previous block"
coinbaseTxArg (Just bid) env tx@Tx{..}
  -- Coinbase contains single mock input which references previous
  -- block instead of box.
  | [BoxInputRef{ boxInputRef'id      = boxId
                , boxInputRef'args    = args
                , boxInputRef'proof   = Nothing
                , boxInputRef'sigMask = SigAll
                }] <- V.toList tx'inputs
  , args  == mempty
  , boxId == coerce bid
  -- Coinbase contains single output
  , [_] <- V.toList tx'outputs
  -- Build TxArg
  = pure TxArg { txArg'inputs   = V.empty
               , txArg'outputs  = V.imap (\i b -> IBox (computeBoxId txId (fromIntegral i)) b) tx'outputs
               , txArg'env      = env
               , txArg'id       = txId
               }
  where
    txId = computeTxId tx
coinbaseTxArg _ _ _ = throwError $ InternalErr "Invalid coinbase"



-- | Check that block preserves value. Individual transactions will
--   leave part of value as reward for miners. So transactions do
--   not preserve value while whole block should
checkBalance
  :: (MonadError (POW.BlockException UTXOBlock) m)
  => [TxArg] -> m ()
-- FIXME: Overflows
checkBalance []                = throwError $ InternalErr "Empty block"
checkBalance (coinbase:txArgs) = do
  forM_ balances $ \(i, o) -> unless (i <= o) $ throwError $ InternalErr "Tx spends more than it has"
  unless (inputs == outputs) $ throwError $ InternalErr "Block is not balanced"
  where
    inputs     = miningRewardAmount  + sumOf (each . _1) balances
    outputs    = sumOutputs coinbase + sumOf (each . _2) balances
    balances   = (sumInputs &&& sumOutputs) <$> txArgs
    --
    sumInputs  = sumOf (txArg'inputsL  . each . boxInput'boxL . box'valueL)
    sumOutputs = sumOf (txArg'outputsL . each . ibox'boxL     . box'valueL)


-- | Mark every input as spent and mark every output as created
processTX
  :: POW.BlockIndexPath (ID (POW.Block UTXOBlock))
  -> ActiveOverlay
  -> TxArg
  -> ExceptT (POW.BlockException UTXOBlock) (Query rw) ActiveOverlay
processTX pathInDB overlay0 tx@TxArg{..} = do
  overlay1 <- foldM spendBox  overlay0 txArg'inputs
  pure $ foldl' createBox overlay1 txArg'outputs
  where
    -- We must ensure that we won't spend same input twice
    spendBox o input
      = traverseOf (activeLayer . lensSpent) (spend input) o
    spend BoxInput{..} = Map.alterF
      (\case
          Just _  -> throwError $ InternalErr "Double spend or unknown ID"
          Nothing -> pure $ Just boxInput'box
      ) boxInput'id
    -- On other hand new
    createBox o (IBox boxId box) = o & activeLayer . lensCreated . at boxId .~ Just box

{-
  -- Fetch all inputs & check that we can spend them
  inputs <- forM tx'inputs $ \bir@BoxInputRef{..} -> do
    let boxid = boxInputRef'id
    case getOverlayBoxId overlay boxid of
      Just (Spent _) -> throwError $ InternalErr "Input already spent"
      Just (Added u) -> return (boxid,u)
      Nothing        -> (,) boxid <$> getDatabaseBox pathInDB bir
  checkSpendability inputs tx
  let inputsSum = sum $ fmap (box'value . snd) inputs
      outputsSum = sum $ fmap box'value tx'outputs
  -- Update overlay
  let overlay1 = foldl' (\o (boxid,u) -> spendBox boxid u o) overlay inputs
      overlay2 = foldl' (\o (boxid,u) -> createUnspentBox boxid u o) overlay1
               $ V.imap (\i b -> (computeBoxId txId (fromIntegral i), b)) tx'outputs
  return (outputsSum - inputsSum, overlay2)
  where
    txId = computeTxId tx
-}


-------------------------------------------------------------------------------
-- Transaction validation.

-- | Context free TX validation for transactions. This function
--   performs all checks that could be done having only transaction at
--   hand.
validateTransactionContextFree :: Tx -> Either (POW.BlockException UTXOBlock) ()
validateTransactionContextFree (Tx{}) = do
  return ()
--  -- Inputs and outputs are not null
--  when (null txInputs)  $ Left $ CoinError "Empty input list"
--  when (null txOutputs) $ Left $ CoinError "Empty output list"
--  -- No duplicate inputs
--  when (nub txInputs /= txInputs) $ Left $ CoinError "Duplicate inputs"
--  -- Outputs are all positive
--  forM_ txOutputs $ \(Unspent _ n) ->
--    unless (n > 0) $ Left $ CoinError "Negative output"
--  -- Signature must be valid.
--  unless (verifySignatureHashed pubK txSend sig)
--    $ Left $ CoinError "Invalid signature"

-- | We check transactions in block as a whole.
--
checkBlockTransactions
  :: POW.BlockID UTXOBlock
  -> POW.BlockIndexPath (ID (POW.Block UTXOBlock))
  -> ActiveOverlay
  -> [Tx]
  -> ExceptT (POW.BlockException UTXOBlock) (Query rw) ()
checkBlockTransactions prevBID pathInDB overlay txs = do
  {-
  moneyCreated <- fmap sum $ forM (zip txs $ True : repeat False) $ \(tx@Tx{..}, canCreateMoney) -> do
    inputs <- forM tx'inputs $ \bir@BoxInputRef{..} -> do
      let boxid = boxInputRef'id
      case getOverlayBoxId overlay boxid of
        Just (Spent _) -> throwError $ InternalErr "Input already spent"
        Just (Added u) -> return (boxid,u)
        Nothing        -> (,) boxid <$> getDatabaseBox pathInDB bir
    checkSpendability inputs tx
    let inputsSum  = sum $ fmap (box'value . snd) inputs
        outputsSum = sum $ fmap box'value tx'outputs
    when (inputsSum < outputsSum && not canCreateMoney) $ throwError $ BadTx "money creation not in coinbase"
    return $ outputsSum - inputsSum
  when (moneyCreated /= miningRewardAmount) $ throwError $
    BadCoinbase $ "block reward is "++show moneyCreated++" instead of "++show miningRewardAmount
-}
  return ()
{-
  -- Check inputs
  case ins of
    [UTXO 0 h] | h == coerce prevBID -> return ()
    _ -> throwError $ InternalErr "Invalid backreference in coinbase"
  -- Check outputs
  u <- case outs of
    [u@(Unspent _ 100)] -> return u
    _ -> throwError $ InternalErr "Invalid output in coinbase"
  return $ createUnspentBox boxid u overlay
  where
    txHash = hashed tx
    utxo   = UTXO 0 txHash
-}


getDatabaseBox
  :: ()
  => POW.BlockIndexPath (ID (POW.Block UTXOBlock))
  -> BoxId
  -> ExceptT (POW.BlockException UTXOBlock) (Query rw) Box
-- Check whether output was created in the block
getDatabaseBox (POW.ApplyBlock i path) boxId = do
  isSpentAtBlock i boxId >>= \case
    Just _  -> throwError $ InternalErr "Output is already spent"
    Nothing -> return ()
  isCreatedAtBlock i boxId >>= \case
    Just u  -> return u
    Nothing -> getDatabaseBox path boxId
-- Perform check in block being reverted. If UTXO was create in that
-- block it didn't exist before and we should abort.
getDatabaseBox (POW.RevertBlock i path) boxId = do
  isCreatedAtBlock i boxId >>= \case
    Just _  -> throwError $ InternalErr "Output does not exists"
    Nothing -> return ()
  isSpentAtBlock i boxId >>= \case
    Just u  -> return u
    Nothing -> getDatabaseBox path boxId
getDatabaseBox POW.NoChange boxId = do
  r <- {-Debug.trace ("fetching box for id "++show boxid) $ -}basicQuery1
    "SELECT box \
    \  FROM utxo_set \
    \  JOIN utxo_state ON live_utxo = utxo_id \
    \ WHERE box_id = ?"
    (Only boxId)
  {-Debug.trace ("fetched "++show r) $ -}
  case r of
    Just (Only u) -> return u
    Nothing       -> throwError $ InternalErr "No such UTXO"

spendBox :: BoxId -> Box -> ActiveOverlay -> ActiveOverlay
spendBox boxid val
  = activeLayer . lensSpent . at boxid .~ Just val

createUnspentBox :: BoxId -> Box -> ActiveOverlay -> ActiveOverlay
createUnspentBox boxid val
  = activeLayer . lensCreated . at boxid .~ Just val

isSpentAtBlock :: MonadQueryRO m => ID (POW.Block UTXOBlock) -> BoxId -> m (Maybe Box)
isSpentAtBlock i boxid
  =  fmap fromOnly
 <$> basicQuery1
       "SELECT box \
       \  FROM utxo_set \
       \  JOIN utxo_spent ON utxo_id = utxo_ref \
       \ WHERE box_id = ? AND block_ref = ?"
       (boxid, i)

isCreatedAtBlock :: MonadQueryRO m => ID (POW.Block UTXOBlock) -> BoxId -> m (Maybe Box)
isCreatedAtBlock i boxid
  =  fmap fromOnly
 <$> basicQuery1
       "SELECT box \
       \  FROM utxo_set \
       \  JOIN utxo_created ON utxo_id = utxo_ref \
       \ WHERE box_id = ? AND block_ref = ?"
       (boxid, i)

retrieveCurrentStateBlock :: MonadQueryRO m => m (Maybe (POW.BlockID UTXOBlock))
retrieveCurrentStateBlock = fmap fromOnly <$> basicQuery1
  "SELECT bid \
  \  FROM utxo_blocks \
  \  JOIN utxo_state_bid ON state_block = blk_id"
  ()

retrieveUTXOBlockTableID :: MonadQueryRO m => POW.BlockID UTXOBlock -> m (ID (POW.Block UTXOBlock))
retrieveUTXOBlockTableID bid = do
  r <- basicQuery1
    "SELECT blk_id FROM utxo_blocks WHERE bid =?"
    (Only bid)
  case r of
    Nothing       -> error "Unknown BID"
    Just (Only i) -> return i

retrieveUTXOIO :: MonadQueryRO m => BoxId -> m Int
retrieveUTXOIO utxo = do
  r <- basicQuery1
    "SELECT utxo_id FROM utxo_set WHERE box_id = ?"
    (Only utxo)
  case r of
    Just (Only i) -> return i
    Nothing       -> error "retrieveUTXOIO"

retrieveUTXOByBoxId :: (MonadReadDB m, MonadIO m) => BoxId -> m (Maybe Box)
retrieveUTXOByBoxId boxid
  =  queryRO
  $  fmap fromOnly
 <$> basicQuery1 "SELECT box FROM utxo_set WHERE box_id = ?" (Only boxid)

revertBlockDB :: MonadQueryRW m => POW.BH UTXOBlock -> m ()
revertBlockDB bh = do
  i <- retrieveUTXOBlockTableID $ POW.bhBID bh
  basicExecute
    "DELETE FROM utxo_state WHERE live_utxo IN \
    \  (SELECT utxo_ref FROM utxo_created WHERE block_ref = ?)"
    (SQL.Only i)
  basicExecute
    "INSERT OR IGNORE INTO utxo_state \
    \  SELECT utxo_ref FROM utxo_spent WHERE block_ref = ?"
    (SQL.Only i)

applyBlockDB :: MonadQueryRW m => POW.BH UTXOBlock -> m ()
applyBlockDB bh = do
  i <- retrieveUTXOBlockTableID $ POW.bhBID bh
  basicExecute
    "DELETE FROM utxo_state WHERE live_utxo IN \
    \  (SELECT utxo_ref FROM utxo_spent WHERE block_ref = ?)"
    (SQL.Only i)
  basicExecute
    "INSERT OR IGNORE INTO utxo_state \
    \  SELECT utxo_ref FROM utxo_created WHERE block_ref = ?"
    (SQL.Only i)

----------------------------------------------------------------
-- Overlays
--
-- Difference between state recorded in database and our current state
-- is kept in overlays which are linked lists of changes to state
----------------------------------------------------------------

-- | In-memory overlay for UTXO state. It contains changes to UTXO set
--   that are not commited to the database. Each layer corresponds to
--   block and considered immutable. Updates to
--
--   Note that it only contains blocks that are added to blockchain but
--   not rollbacks since latter are already commited.
data StateOverlay
  = OverlayBase  (POW.BH UTXOBlock)
  | OverlayLayer (POW.BH UTXOBlock) Layer StateOverlay

-- | Overlay that is guaranteed to have layer which is used to
--   add/remove UTXOs
data ActiveOverlay = ActiveOverlay Layer StateOverlay
  deriving Generic

-- | Changes to database set generated by single block
data Layer = Layer
  { utxoCreated :: Map.Map BoxId Box
  , utxoSpent   :: Map.Map BoxId Box
  }

-- | Change to UTXO set
data Change a
  = Added a
  | Spent a

activeLayer :: Lens' ActiveOverlay Layer
activeLayer = lens (\(ActiveOverlay l _) -> l) (\(ActiveOverlay _ o) l -> ActiveOverlay l o)

lensCreated, lensSpent :: Lens' Layer (Map.Map BoxId Box)
lensCreated = lens utxoCreated (\m x -> m { utxoCreated = x })
lensSpent   = lens utxoSpent   (\m x -> m { utxoSpent   = x })

-- | Convert overlay to active overlay by adding empty layer
addOverlayLayer :: StateOverlay -> ActiveOverlay
addOverlayLayer = ActiveOverlay (Layer mempty mempty)

-- | Finalize layer and convert active overlay into normal overlay
--   with no modification possible.
finalizeOverlay :: POW.BH UTXOBlock -> ActiveOverlay -> Maybe StateOverlay
finalizeOverlay bh (ActiveOverlay l o) = do
  bid <- POW.bhBID <$> POW.bhPrevious bh
  guard $ POW.bhBID (overlayTip o) == bid
  pure  $ OverlayLayer bh l o

-- | Create empty overlay build over given block. It correspond to
--   state after evaluation of that block.
emptyOverlay :: POW.BH UTXOBlock -> StateOverlay
emptyOverlay = OverlayBase

-- | Get pointer to block index to block over which overlay is built.
overlayBase :: StateOverlay -> POW.BH UTXOBlock
overlayBase (OverlayBase  bh)    = bh
overlayBase (OverlayLayer _ _ o) = overlayBase o

overlayTip :: StateOverlay -> POW.BH UTXOBlock
overlayTip (OverlayBase  bh)     = bh
overlayTip (OverlayLayer bh _ _) = bh

-- | Roll back overlay by one block. Will throw if rolling past
--   genesis is attempted.
rollbackOverlay :: StateOverlay -> StateOverlay
rollbackOverlay (OverlayBase bh0) = case POW.bhPrevious bh0 of
  Just bh -> OverlayBase bh
  Nothing -> error "Cant rewind overlay past genesis"
rollbackOverlay (OverlayLayer _ _ o) = o

-- | Find whether given UTXO is awaialble to be spent or spent
--   already. We need latter since UTXO could be available in
--   underlying state but spent in overlay and we need to account for
--   that explicitly.
getOverlayBoxId :: ActiveOverlay -> BoxId -> Maybe (Change Box)
getOverlayBoxId (ActiveOverlay l0 o0) boxid
  =  getFromLayer l0
 <|> recur o0
 where
   recur (OverlayBase  _)     = Nothing
   recur (OverlayLayer _ l o) =  getFromLayer l
                             <|> recur o
   getFromLayer Layer{..}
     =  Spent <$> Map.lookup boxid utxoSpent
    <|> Added <$> Map.lookup boxid utxoCreated

-- | Dump overlay content into database. We write both UTXO content
--   (table utxo_set) and when given utxo was created/spent. Operation
--   is idempotent.
dumpOverlay :: MonadQueryRW m => StateOverlay -> m ()
dumpOverlay (OverlayLayer bh Layer{..} o) = do
  dumpOverlay o
  -- Store create UTXO
  forM_ (Map.toList utxoCreated) $ \(utxo, unspent) -> do
    basicExecute
      "INSERT OR IGNORE INTO utxo_set VALUES (NULL,?,?)"
      (utxo, unspent)
  -- Write down block delta
  bid <- retrieveUTXOBlockTableID (POW.bhBID bh)
  forM_ (Map.keys utxoCreated) $ \utxo -> do
    box <- retrieveUTXOIO utxo
    basicExecute
      "INSERT OR IGNORE INTO utxo_created VALUES (?,?)"
      (bid, box)
  forM_ (Map.keys utxoSpent) $ \utxo -> do
    box <- retrieveUTXOIO utxo
    basicExecute
      "INSERT OR IGNORE INTO utxo_spent VALUES (?,?)"
      (bid, box)
dumpOverlay OverlayBase{} = return ()


----------------------------------------------------------------
-- Block storage
----------------------------------------------------------------

-- | Database-backed storage for blocks
utxoBlockDB :: (MonadIO m, MonadDB m, MonadThrow m) => POW.BlockDB m UTXOBlock
utxoBlockDB = POW.BlockDB
  { storeBlock         = storeUTXOBlock
  , retrieveBlock      = retrieveUTXOBlock
  , retrieveHeader     = retrieveUTXOHeader
  , retrieveAllHeaders = retrieveAllUTXOHeaders
  }

retrieveAllUTXOHeaders :: (MonadIO m, MonadReadDB m) => m [POW.Header UTXOBlock]
retrieveAllUTXOHeaders
  = queryRO
  $ basicQueryWith_ utxoBlockHeaderDecoder
    "SELECT height, time, prev, dataHash, target, nonce FROM utxo_blocks ORDER BY height"

retrieveUTXOHeader :: (MonadIO m, MonadReadDB m) => POW.BlockID UTXOBlock -> m (Maybe (POW.Header UTXOBlock))
retrieveUTXOHeader bid
  = queryRO
  $ basicQueryWith1 utxoBlockHeaderDecoder
    "SELECT height, time, prev, dataHash, target, nonce FROM utxo_blocks WHERE bid = ?"
    (Only bid)

retrieveUTXOBlock :: (MonadIO m, MonadReadDB m) => POW.BlockID UTXOBlock -> m (Maybe (POW.Block UTXOBlock))
retrieveUTXOBlock bid
  = queryRO
  $ basicQueryWith1 utxoBlockDecoder
    "SELECT height, time, prev, blockData FROM utxo_blocks WHERE bid = ?"
    (Only bid)

storeUTXOBlock :: (MonadThrow m, MonadIO m, MonadDB m) => POW.Block UTXOBlock -> m ()
storeUTXOBlock b@POW.GBlock{POW.blockData=blk, ..} = mustQueryRW $ do
  let bid = POW.blockID b
  basicExecute
    "INSERT OR IGNORE INTO utxo_blocks VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?)"
    ( bid
    , blockHeight
    , blockTime
    , prevBlock
    , ByteRepred $ merkleHash $ ubData blk
    , CBORed blk
    , CBORed $ ubTarget blk
    , ubNonce blk
    )

utxoBlockDecoder :: SQL.RowParser (POW.Block UTXOBlock)
utxoBlockDecoder = do
  blockHeight <- field
  blockTime   <- field
  prevBlock   <- field
  blockData   <- fieldCBOR
  pure POW.GBlock {..}

utxoBlockHeaderDecoder :: SQL.RowParser (POW.Header UTXOBlock)
utxoBlockHeaderDecoder = do
  blockHeight <- field
  blockTime   <- field
  prevBlock   <- field
  ubData      <- fromHashed <$> fieldByteRepr
  ubTarget    <- fieldCBOR
  ubNonce     <- field
  return POW.GBlock{ blockData = UTXOBlock{..}, .. }


----------------------------------------------------------------
-- Database schema
----------------------------------------------------------------

-- Initialize database for mock coin blockchain
initUTXODB :: (MonadThrow m, MonadDB m, MonadIO m) => m ()
initUTXODB = mustQueryRW $ do
  -- Table for blocks. We store both block data is serialized form and
  -- separately UTXOs for working with blockchain state so data is
  -- duplicated
  basicExecute_
    "CREATE TABLE IF NOT EXISTS utxo_blocks \
    \  ( blk_id     INTEGER PRIMARY KEY AUTOINCREMENT \
    \  , bid        BLOB NOT NULL UNIQUE \
    \  , height     INTEGER NOT NULL \
    \  , time       INTEGER NUT NULL \
    \  , prev       BLOB NULL \
    \  , dataHash   BLOB NOT NULL \
    \  , blockData  BLOB NOT NULL \
    \  , target     BLOB NOT NULL \
    \  , nonce      BLOB NOT NULL \
    \)"
  -- All UTXOs known to blockchain. We never delete them so node works
  -- as archival node.
  basicExecute_
    "CREATE TABLE IF NOT EXISTS utxo_set \
    \  ( utxo_id INTEGER PRIMARY KEY \
    \  , box_id BLOB NOT NULL UNIQUE \
    \  , box    BLOB NOT NULL \
    \)"
  -- UTXO's created in given block. Due to blockchain reorganizations
  -- same UTXO may appear in several blocks
  basicExecute_
    "CREATE TABLE IF NOT EXISTS utxo_created \
    \  ( block_ref INTEGER NOT NULL \
    \  , utxo_ref  INTEGER NOT NULL \
    \  , FOREIGN KEY (block_ref) REFERENCES utxo_blocks(blk_id) \
    \  , FOREIGN KEY (utxo_ref)  REFERENCES utxo_set(utxo_id)  \
    \  , UNIQUE (block_ref, utxo_ref) \
    \)"
  basicExecute_
    "CREATE TABLE IF NOT EXISTS utxo_spent \
    \  ( block_ref INTEGER NOT NULL \
    \  , utxo_ref  INTEGER NOT NULL \
    \  , FOREIGN KEY (block_ref) REFERENCES utxo_blocks(blk_id) \
    \  , FOREIGN KEY (utxo_ref)  REFERENCES utxo_utxo(utxo_id)  \
    \  , UNIQUE (block_ref, utxo_ref) \
    \)"
  -- Current state of blockchain. It's just set of currently live UTXO
  -- with pointer to the block for which it corresponds
  basicExecute_
    "CREATE TABLE IF NOT EXISTS utxo_state \
    \  ( live_utxo INTEGER NOT NULL \
    \  , FOREIGN KEY (live_utxo) REFERENCES utxo_set(utxo_id)\
    \  , UNIQUE (live_utxo) \
    \)"
  basicExecute_
    "CREATE TABLE IF NOT EXISTS utxo_state_bid \
    \  ( state_block INTEGER NOT NULL \
    \  , uniq        INTEGER NOT NULL UNIQUE \
    \  , FOREIGN KEY (state_block) REFERENCES utxo_blocks(blk_id) \
    \  , CHECK (uniq = 0) \
    \)"



----------------------------------------------------------------
-- SQL instances
----------------------------------------------------------------

deriving via (ByteRepred BoxId) instance SQL.FromField BoxId
deriving via (ByteRepred BoxId) instance SQL.ToField   BoxId
deriving via (CBORed Box)       instance SQL.FromField Box
deriving via (CBORed Box)       instance SQL.ToField   Box
