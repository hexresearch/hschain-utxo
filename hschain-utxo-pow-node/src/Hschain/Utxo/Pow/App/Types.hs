-- |Hschain.Utxo.Pow.App.Types
--
-- Full fledged PoW consensus node, with external REST API.
--
-- Copyright (C) 2020 ...
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
-- SQL-related orphans
{-# OPTIONS_GHC -Wno-orphans #-}
module Hschain.Utxo.Pow.App.Types
  ( UTXOBlock(..)
  , ubDataL
  , ubTargetL
  , ubNonceL
  , UtxoPOWCongig(..)
  , utxoStateView
  , miningRewardAmount
    -- * Working with state
  , retrieveUTXOByBoxId
  , getDatabaseBox
  , sumTxInputs
  , sumTxOutputs
    -- * Data families constructors
  , POW.BlockID(..)
  , POW.TxID(..)
  ) where

import Hex.Common.Aeson
import Hex.Common.Lens

import Codec.Serialise

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Except (except)

import Data.Coerce
import Data.ByteString         (ByteString)
import Data.ByteString.Builder (toLazyByteString,Builder)
import Data.Functor.Classes    (Show1)
import Data.Typeable           (Typeable)
import Data.List               (foldl')
import Data.Maybe
import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Map.Strict      as Map
import qualified Data.Vector          as V

import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.FromRow   as SQL

import GHC.Generics (Generic)

import HSChain.Crypto.Classes
import HSChain.Crypto.SHA
import qualified HSChain.Crypto.Classes.Hash as Crypto
import qualified HSChain.POW            as POW
import qualified HSChain.PoW.Consensus  as POW
import qualified HSChain.PoW.BlockIndex as POW
import qualified HSChain.PoW.Types      as POW
import HSChain.Types.Merkle.Types
import HSChain.Crypto hiding (PublicKey)
import HSChain.Store.Query

import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Eval
import Hschain.Utxo.Lang.Core.Types


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
data UTXOBlock t f = UTXOBlock
  { ubData   :: !(MerkleNode f SHA256 [Tx])
  , ubTarget :: !POW.Target
  , ubNonce  :: !ByteString
  }
  deriving stock (Generic)

-- | Configuration of PoW function for given tag. It allows to pick
--   concrete set of parameters of disable work check at all
class Typeable t => UtxoPOWCongig t where
  powConfig :: Proxy t -> POW.POWConfig
  checkBlockWork :: Proxy t -> Bool
  checkBlockWork _ = True

deriving stock instance (Show1 f)    => Show (UTXOBlock t f)
deriving stock instance (IsMerkle f) => Eq   (UTXOBlock t f)
instance Serialise (UTXOBlock t Identity)
instance Serialise (UTXOBlock t Proxy)

instance IsMerkle f => JSON.FromJSON (UTXOBlock t f) where
  parseJSON = JSON.withObject "UtxoBlock" $ \bp ->
    UTXOBlock
      <$> (bp JSON..: "data")
      <*> (POW.Target <$> bp JSON..: "target")
      <*> ((\(ViaBase58 s :: ViaBase58 "UTXOBlock" ByteString) -> s) <$> (bp JSON..: "nonce"))
instance IsMerkle f => JSON.ToJSON (UTXOBlock t f) where
  toJSON (UTXOBlock d (POW.Target t) b) =
    JSON.object
      [ "data"   JSON..= d
      , "target" JSON..= t
      , "nonce"  JSON..= ViaBase58 b
      ]

ubDataL :: Lens (UTXOBlock t f) (UTXOBlock t g) (MerkleNode f SHA256 [Tx]) (MerkleNode g SHA256 [Tx])
ubDataL = lens ubData (\b x -> b { ubData = x })

ubTargetL :: Lens' (UTXOBlock t f) POW.Target
ubTargetL = lens ubTarget (\b x -> b { ubTarget = x })

ubNonceL :: Lens' (UTXOBlock t f) ByteString
ubNonceL = lens ubNonce (\b x -> b { ubNonce = x })

$(makeLensesWithL ''POW.GBlock)


----------------------------------------------------------------
-- BlockData instance
----------------------------------------------------------------

instance UtxoPOWCongig t => POW.BlockData (UTXOBlock t) where
  --
  newtype BlockID (UTXOBlock t) = UB'BID (Crypto.Hash SHA256)
    deriving newtype
      (Show, Eq, Ord, Crypto.CryptoHashable, Serialise, ToJSON, FromJSON, ByteRepr)
    deriving (SQL.FromField, SQL.ToField) via ByteRepred (Crypto.Hash SHA256)

  type Tx (UTXOBlock t) = Tx

  newtype TxID (UTXOBlock t) = UTXOTxID (Hash SHA256)
    deriving newtype ( Show, Eq, Ord, CryptoHashable, Serialise, ByteRepr
                     , JSON.ToJSON, JSON.FromJSON)
    deriving (SQL.FromField, SQL.ToField) via ByteRepred (Crypto.Hash SHA256)

  data BlockException (UTXOBlock t)
    = WrongAnswer
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
      answerIsGood <- case checkBlockWork (Proxy @t) of
                        True  -> liftIO $ checkPuzzle header
                        False -> pure True
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


instance MerkleMap (UTXOBlock t) where
  merkleMap f = ubDataL %~ mapMerkleNode f

computeBlockID :: POW.GBlock (UTXOBlock t) f -> POW.BlockID (UTXOBlock t)
computeBlockID b
  = UB'BID . hashBuilder
  $ blockIdBuilder b
 <> hashStep (ubNonce $ POW.blockData b)

-- | Builder without which doesn't include nonce
blockIdBuilder :: POW.GBlock (UTXOBlock t) f -> Builder
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

checkPuzzle :: forall t f. (UtxoPOWCongig t) => POW.GBlock (UTXOBlock t) f -> IO Bool
checkPuzzle b = POW.check bs nonce h powCfg
  where
    bs     = LBS.toStrict $ toLazyByteString $ blockIdBuilder b
    nonce  = ubNonce $ POW.blockData b
    tgt    = POW.blockTargetThreshold b
    powCfg = (powConfig (Proxy @t))
      { POW.powCfgTarget = POW.targetInteger tgt
      }
    Hash h = hashBlob @SHA256 $ nonce <> bs

instance (UtxoPOWCongig t) => POW.Mineable (UTXOBlock t) where
  adjustPuzzle b0@POW.GBlock{..} = do
    (maybeAnswer, hashR) <- liftIO $ POW.solve [bs] powCfg
    return ( do answer <- maybeAnswer
                pure $ b0 & blockDataL . ubNonceL .~ answer
           , POW.hash256AsTarget hashR
           )
    where
      bs     = LBS.toStrict $ toLazyByteString $ blockIdBuilder b0
      powCfg = (powConfig (Proxy @t))
        { POW.powCfgTarget = POW.targetInteger $ POW.blockTargetThreshold b0
        }

----------------------------------------------------------------
-- Blockchain state management
----------------------------------------------------------------

-- | Create triple: block storage, block index, state view using
--   current database state.
utxoStateView
  :: (MonadThrow m, MonadDB m, MonadIO m, MonadDB m, UtxoPOWCongig t)
  => POW.Block (UTXOBlock t)
  -> m (POW.BlockDB m (UTXOBlock t), POW.BlockIndex (UTXOBlock t), POW.StateView m (UTXOBlock t))
utxoStateView genesis = do
  initUTXODB
  storeUTXOBlock genesis
  bIdx <- POW.buildBlockIndex utxoBlockDB
  st   <- mustQueryRW $ initializeStateView genesis bIdx
  return (utxoBlockDB, bIdx, st)

initializeStateView
  :: (MonadDB m, MonadThrow m, MonadQueryRW q, MonadIO m, UtxoPOWCongig t)
  => POW.Block (UTXOBlock t)            -- ^ Genesis block
  -> POW.BlockIndex (UTXOBlock t)       -- ^ Block index
  -> q (POW.StateView m (UTXOBlock t))
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
  :: (MonadDB m, MonadThrow m, MonadIO m, UtxoPOWCongig t)
  => POW.BlockIndex (UTXOBlock t)
  -> StateOverlay t
  -> POW.StateView m (UTXOBlock t)
makeStateView bIdx0 overlay = sview where
  bh0    = overlayTip overlay
  env    = let POW.Height h = POW.bhHeight bh0 in Env (fromIntegral h)
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
    , checkTx = \tx@Tx{..} -> queryRO $ runExceptT $ do
        -- Incoming transactions are validated against current state as recorde in DB
        txArg <- buildTxArg (getDatabaseBox POW.NoChange) env tx
        unless (sumTxInputs txArg <= sumTxOutputs txArg) $
          throwError $ InternalErr "Transaction create money"
        either (throwError . InternalErr . T.unpack) pure
          $ evalProveTx txArg
      --
    , createCandidateBlockData = createUtxoCandidate overlay bIdx0
    }


-- | Validate and apply block to current state view. Returns Left on failed validation
applyUtxoBlock
  :: (MonadDB m, MonadThrow m, MonadIO m, UtxoPOWCongig t)
  => StateOverlay t
  -> POW.BlockIndex (UTXOBlock t)
  -> POW.BH (UTXOBlock t)
  -> POW.GBlock (UTXOBlock t) Identity
  -> m (Either (POW.BlockException (UTXOBlock t)) (POW.StateView m (UTXOBlock t)))
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
    foldM processTX overlay0 txArgs
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
  :: (MonadReadDB m, MonadIO m, UtxoPOWCongig t)
  => StateOverlay t
  -> POW.BlockIndex (UTXOBlock t)
  -> POW.BH (UTXOBlock t)
  -> p
  -> [Tx]
  -> m (UTXOBlock t Identity)
createUtxoCandidate overlay bIdx bh _time txlist = queryRO $ do
  pathInDB <- do
    Just stateBid <- retrieveCurrentStateBlock
    let Just bhState = POW.lookupIdx stateBid bIdx
    POW.makeBlockIndexPathM (retrieveUTXOBlockTableID . POW.bhBID)
      bhState (overlayBase overlay)
  -- Select transaction
  let tryTX o tx = do
        -- FIXME: duplication of checks with check TX
        txArg <- buildTxArg (getDatabaseBox pathInDB) env tx
        unless (sumTxInputs txArg <= sumTxOutputs txArg) $
          throwError $ InternalErr "Transaction create money"
        either (throwError . InternalErr . T.unpack) pure
          $ evalProveTx txArg
        o' <- processTX o txArg
        return (txArg, o')
  -- Select transactions
  let selectTX []     _ = return []
      selectTX (t:ts) o = runExceptT (tryTX o t) >>= \case
        Left  _       -> selectTX ts o
        Right (tA,o') -> ((tA,t):) <$> selectTX ts o'
      --
  txList <- selectTX txlist $ addOverlayLayer overlay
  -- Create and process coinbase transaction
  let commission  = sumOf (each . _1 . to sumTxOutputs) txList
                  - sumOf (each . _1 . to sumTxInputs)  txList
      coinbaseBox = Box { box'value  = miningRewardAmount + commission
                        , box'script = coreProgToScript $ EPrim (PrimBool True)
                        , box'args   = mempty
                        }
      coinbase = Tx { tx'inputs  = V.singleton BoxInputRef
                        { boxInputRef'id      = coerce (POW.bhBID bh)
                        , boxInputRef'args    = mempty
                        , boxInputRef'proof   = Nothing
                        , boxInputRef'sigs    = V.empty
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
  :: (MonadError (POW.BlockException (UTXOBlock t)) m)
  => Maybe (POW.BlockID (UTXOBlock t)) -> Env -> Tx -> m TxArg
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
               , txArg'outputs  = V.imap
                   (\i b -> BoxOutput (PostBox b h) (computeBoxId txId (fromIntegral i)))
                   tx'outputs
               , txArg'env      = env
               , txArg'id       = txId
               }
  where
    h    = env'height env
    txId = computeTxId tx
coinbaseTxArg _ _ _ = throwError $ InternalErr "Invalid coinbase"



-- | Check that block preserves value. Individual transactions will
--   leave part of value as reward for miners. So transactions do
--   not preserve value while whole block should
checkBalance
  :: (MonadError (POW.BlockException (UTXOBlock t)) m)
  => [TxArg] -> m ()
-- FIXME: Overflows
checkBalance []                = throwError $ InternalErr "Empty block"
checkBalance (coinbase:txArgs) = do
  forM_ balances $ \(ins, outs) ->
    unless (ins >= outs) $ throwError $ InternalErr "Tx spends more than it has"
  unless (inputs == outputs) $ throwError $ InternalErr "Block is not balanced"
  where
    inputs     = miningRewardAmount    + sumOf (each . _1) balances
    outputs    = sumTxOutputs coinbase + sumOf (each . _2) balances
    balances   = (sumTxInputs &&& sumTxOutputs) <$> txArgs


sumTxInputs, sumTxOutputs :: TxArg -> Money
sumTxInputs  = sumOf (txArg'inputsL  . each . boxInput'boxL    . to postBox'content . box'valueL)
sumTxOutputs = sumOf (txArg'outputsL . each . to boxOutput'box . to postBox'content . box'valueL)


-- | Mark every input as spent and mark every output as created
processTX
  :: ActiveOverlay t
  -> TxArg
  -> ExceptT (POW.BlockException (UTXOBlock t)) (Query rw) (ActiveOverlay t)
processTX overlay0 TxArg{..} = do
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
    createBox o BoxOutput{..} = o & activeLayer . lensCreated . at boxOutput'id .~ Just boxOutput'box


-------------------------------------------------------------------------------
-- Transaction validation.

-- | Context free TX validation for transactions. This function
--   performs all checks that could be done having only transaction at
--   hand.
validateTransactionContextFree :: Tx -> Either (POW.BlockException (UTXOBlock t)) ()
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


getDatabaseBox
  :: ()
  => POW.BlockIndexPath (ID (POW.Block (UTXOBlock t)))
  -> BoxId
  -> ExceptT (POW.BlockException (UTXOBlock t)) (Query rw) PostBox
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
  r <- basicQuery1
    "SELECT box, utxo_height \
    \  FROM utxo_set \
    \  JOIN utxo_state ON live_utxo = utxo_id   \
    \ WHERE box_id = ?"
    (Only boxId)
  case r of
    Just u  -> return u
    Nothing -> throwError $ InternalErr "No such UTXO"


isSpentAtBlock :: MonadQueryRO m => ID (POW.Block (UTXOBlock t)) -> BoxId -> m (Maybe PostBox)
isSpentAtBlock i boxid = basicQuery1
  "SELECT box, height \
  \  FROM utxo_set \
  \  JOIN utxo_spent  ON utxo_id = utxo_ref  \
  \  JOIN utxo_blocks ON blk_id  = block_ref \
  \ WHERE box_id = ? AND block_ref = ?"
  (boxid, i)

isCreatedAtBlock :: MonadQueryRO m => ID (POW.Block (UTXOBlock t)) -> BoxId -> m (Maybe PostBox)
isCreatedAtBlock i boxid = basicQuery1
  "SELECT box \
  \  FROM utxo_set \
  \  JOIN utxo_created ON utxo_id = utxo_ref  \
  \  JOIN utxo_blocks  ON blk_id  = block_ref \
  \ WHERE box_id = ? AND block_ref = ?"
  (boxid, i)

retrieveCurrentStateBlock :: MonadQueryRO m => m (Maybe (POW.BlockID (UTXOBlock t)))
retrieveCurrentStateBlock = fmap fromOnly <$> basicQuery1
  "SELECT bid \
  \  FROM utxo_blocks \
  \  JOIN utxo_state_bid ON state_block = blk_id"
  ()

retrieveUTXOBlockTableID :: MonadQueryRO m => POW.BlockID (UTXOBlock t) -> m (ID (POW.Block (UTXOBlock t)))
retrieveUTXOBlockTableID bid = do
  r <- basicQuery1
    "SELECT blk_id FROM utxo_blocks WHERE bid =?"
    (Only bid)
  case r of
    Nothing       -> error "Unknown BID"
    Just (Only i) -> return i

retrieveUTXOByBoxId :: (MonadReadDB m, MonadIO m) => BoxId -> m (Maybe Box)
retrieveUTXOByBoxId boxid
  =  queryRO
  $  fmap fromOnly
 <$> basicQuery1 "SELECT box FROM utxo_set WHERE box_id = ?" (Only boxid)

revertBlockDB :: MonadQueryRW m => POW.BH (UTXOBlock t) -> m ()
revertBlockDB bh = do
  i <- retrieveUTXOBlockTableID $ POW.bhBID bh
  basicExecute
    "DELETE FROM utxo_state WHERE live_utxo IN \
    \  (SELECT utxo_ref FROM utxo_created WHERE block_ref = ?)"
    (SQL.Only i)
  basicExecute
    "INSERT OR IGNORE INTO utxo_state \
     \  SELECT utxo_ref, height \
     \     FROM utxo_spent \
     \     JOIN utxo_blocks ON blk_id = block_ref \
     \    WHERE block_ref = ?"
    (SQL.Only i)

applyBlockDB :: MonadQueryRW m => POW.BH (UTXOBlock t) -> m ()
applyBlockDB bh = do
  i <- retrieveUTXOBlockTableID $ POW.bhBID bh
  basicExecute
    "DELETE FROM utxo_state WHERE live_utxo IN \
    \  (SELECT utxo_ref FROM utxo_spent WHERE block_ref = ?)"
    (SQL.Only i)
  basicExecute
    "INSERT OR IGNORE INTO utxo_state \
     \  SELECT utxo_ref, height \
     \     FROM utxo_created \
     \     JOIN utxo_blocks ON blk_id = block_ref \
     \    WHERE block_ref = ?"
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
data StateOverlay t
  = OverlayBase  (POW.BH (UTXOBlock t))
  | OverlayLayer (POW.BH (UTXOBlock t)) Layer (StateOverlay t)

-- | Overlay that is guaranteed to have layer which is used to
--   add/remove UTXOs
data ActiveOverlay t = ActiveOverlay Layer (StateOverlay t)
  deriving Generic

-- | Changes to database set generated by single block
data Layer = Layer
  { utxoCreated :: Map.Map BoxId PostBox
  , utxoSpent   :: Map.Map BoxId PostBox
  }

activeLayer :: Lens' (ActiveOverlay t) Layer
activeLayer = lens (\(ActiveOverlay l _) -> l) (\(ActiveOverlay _ o) l -> ActiveOverlay l o)

lensCreated, lensSpent :: Lens' Layer (Map.Map BoxId PostBox)
lensCreated = lens utxoCreated (\m x -> m { utxoCreated = x })
lensSpent   = lens utxoSpent   (\m x -> m { utxoSpent   = x })

-- | Convert overlay to active overlay by adding empty layer
addOverlayLayer :: StateOverlay t -> ActiveOverlay t
addOverlayLayer = ActiveOverlay (Layer mempty mempty)

-- | Finalize layer and convert active overlay into normal overlay
--   with no modification possible.
finalizeOverlay :: POW.BH (UTXOBlock t) -> ActiveOverlay t -> Maybe (StateOverlay t)
finalizeOverlay bh (ActiveOverlay l o) = do
  bid <- POW.bhBID <$> POW.bhPrevious bh
  guard $ POW.bhBID (overlayTip o) == bid
  pure  $ OverlayLayer bh l o

-- | Create empty overlay build over given block. It correspond to
--   state after evaluation of that block.
emptyOverlay :: POW.BH (UTXOBlock t) -> StateOverlay t
emptyOverlay = OverlayBase

-- | Get pointer to block index to block over which overlay is built.
overlayBase :: StateOverlay t -> POW.BH (UTXOBlock t)
overlayBase (OverlayBase  bh)    = bh
overlayBase (OverlayLayer _ _ o) = overlayBase o

overlayTip :: StateOverlay t -> POW.BH (UTXOBlock t)
overlayTip (OverlayBase  bh)     = bh
overlayTip (OverlayLayer bh _ _) = bh

-- | Roll back overlay by one block. Will throw if rolling past
--   genesis is attempted.
rollbackOverlay :: StateOverlay t -> StateOverlay t
rollbackOverlay (OverlayBase bh0) = case POW.bhPrevious bh0 of
  Just bh -> OverlayBase bh
  Nothing -> error "Cant rewind overlay past genesis"
rollbackOverlay (OverlayLayer _ _ o) = o


-- | Dump overlay content into database. We write both UTXO content
--   (table utxo_set) and when given utxo was created/spent. Operation
--   is idempotent.
dumpOverlay :: MonadQueryRW m => StateOverlay t -> m ()
dumpOverlay (OverlayLayer bh Layer{..} o) = do
  dumpOverlay o
  -- Store create UTXO
  forM_ (Map.toList utxoCreated) $ \(utxo, unspent) -> do
    basicExecute
      "INSERT OR IGNORE INTO utxo_set VALUES (NULL,?,?)"
      (utxo, postBox'content unspent)
  -- Write down block delta
  bid <- retrieveUTXOBlockTableID (POW.bhBID bh)
  forM_ (Map.keys utxoCreated) $ \utxo -> do
    basicExecute
      "INSERT OR IGNORE INTO utxo_created \
      \  SELECT ?,utxo_id FROM utxo_set WHERE box_id = ?"
      (bid, utxo)
  forM_ (Map.keys utxoSpent) $ \utxo -> do
    basicExecute
      "INSERT OR IGNORE INTO utxo_spent \
      \  SELECT ?,utxo_id FROM utxo_set WHERE box_id = ?"
      (bid, utxo)
dumpOverlay OverlayBase{} = return ()


----------------------------------------------------------------
-- Block storage
----------------------------------------------------------------

-- | Database-backed storage for blocks
utxoBlockDB :: (MonadIO m, MonadDB m, MonadThrow m, UtxoPOWCongig t) => POW.BlockDB m (UTXOBlock t)
utxoBlockDB = POW.BlockDB
  { storeBlock         = storeUTXOBlock
  , retrieveBlock      = retrieveUTXOBlock
  , retrieveHeader     = retrieveUTXOHeader
  , retrieveAllHeaders = retrieveAllUTXOHeaders
  }

retrieveAllUTXOHeaders :: (MonadIO m, MonadReadDB m) => m [POW.Header (UTXOBlock t)]
retrieveAllUTXOHeaders
  = queryRO
  $ basicQueryWith_ utxoBlockHeaderDecoder
    "SELECT height, time, prev, dataHash, target, nonce FROM utxo_blocks ORDER BY height"

retrieveUTXOHeader :: (MonadIO m, MonadReadDB m) => POW.BlockID (UTXOBlock t) -> m (Maybe (POW.Header (UTXOBlock t)))
retrieveUTXOHeader bid
  = queryRO
  $ basicQueryWith1 utxoBlockHeaderDecoder
    "SELECT height, time, prev, dataHash, target, nonce FROM utxo_blocks WHERE bid = ?"
    (Only bid)

retrieveUTXOBlock :: (MonadIO m, MonadReadDB m) => POW.BlockID (UTXOBlock t) -> m (Maybe (POW.Block (UTXOBlock t)))
retrieveUTXOBlock bid
  = queryRO
  $ basicQueryWith1 utxoBlockDecoder
    "SELECT height, time, prev, blockData FROM utxo_blocks WHERE bid = ?"
    (Only bid)

storeUTXOBlock :: (MonadThrow m, MonadIO m, MonadDB m, UtxoPOWCongig t) => POW.Block (UTXOBlock t) -> m ()
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

utxoBlockDecoder :: SQL.RowParser (POW.Block (UTXOBlock t))
utxoBlockDecoder = do
  blockHeight <- field
  blockTime   <- field
  prevBlock   <- field
  blockData   <- fieldCBOR
  pure POW.GBlock {..}

utxoBlockHeaderDecoder :: SQL.RowParser (POW.Header (UTXOBlock t))
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
  -- and height of block whenit was introduced
  basicExecute_
    "CREATE TABLE IF NOT EXISTS utxo_state \
    \  ( live_utxo   INTEGER NOT NULL \
    \  , utxo_height INTEGER NOT NULL \
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

instance SQL.FromRow PostBox where
  fromRow = PostBox <$> field <*> field
