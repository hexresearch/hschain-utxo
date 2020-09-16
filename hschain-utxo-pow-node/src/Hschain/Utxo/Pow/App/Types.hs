-- |Hschain.Utxo.Pow.App.Types
--
-- Full fledged PoW consensus node, with external REST API.
--
-- Copyright (C) 2020 ...

-- Please keep switched off -Wno-orphans.
-- We need an instance of CryptoHashable of elliptic curve
-- scalar (Ed.Scalarbelow ) provided by very much external package.
-- We cannot fork that package and add an instance there.

--{-# OPTIONS  -Wno-orphans                                    #-}
{-# LANGUAGE DataKinds                                       #-}
{-# LANGUAGE DeriveAnyClass, DerivingVia, DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts                                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving                      #-}
{-# LANGUAGE MultiWayIf, TypeOperators                       #-}
{-# LANGUAGE UndecidableInstances                            #-}
module Hschain.Utxo.Pow.App.Types where

import Hex.Common.Aeson
import Hex.Common.Yaml

import Codec.Serialise

import Control.Applicative
import Control.Lens
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

import Data.Generics.Product.Typed (typed)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.List (foldl')

import Data.Maybe

import Data.Text (Text)

import qualified Data.Vector as V

import Data.Word

import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.FromRow   as SQL
import qualified Database.SQLite.Simple.ToRow     as SQL

import GHC.Generics

import Servant.API
import Servant.Server

import HSChain.Crypto.Classes
import HSChain.Crypto.SHA
import qualified HSChain.Crypto.Classes.Hash as Crypto
import qualified HSChain.POW as POWFunc
import qualified HSChain.PoW.Consensus as POWConsensus
import qualified HSChain.PoW.BlockIndex as POWBlockIndex
import qualified HSChain.PoW.Types as POWTypes
import qualified HSChain.PoW.Node as POWNode
import HSChain.Types.Merkle.Types

import HSChain.Crypto hiding (PublicKey)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Types.Merkle.Types

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

import Hschain.Utxo.Pow.App.Options (Options(..), readOptions)

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

instance SQL.ToRow BoxId where
  toRow = undefined

instance SQL.FromRow BoxId where
  fromRow = undefined

instance SQL.ToRow Box where
  toRow = undefined

instance SQL.FromRow box where
  fromRow = undefined

-------------------------------------------------------------------------------
-- The Block.

-- |Signature and hash used.
type Alg = Ed25519 :& SHA512

-- ^A block proper. It does not contain nonce to solve PoW puzzle
-- but it contains all information about block.
data UTXOBlockProper f = UTXOBlockProper
  { ubpPrevious   :: !(Maybe (POWTypes.BlockID UTXOBlock))
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
instance IsMerkle f => JSON.FromJSON (UTXOBlockProper f) where
  parseJSON = JSON.withObject "utxoblockproper" $ \bp ->
    UTXOBlockProper
      <$> (bp JSON..: "previous")
      <*> (bp JSON..: "data")
      <*> (POWTypes.Target <$> bp JSON..: "target")
      <*> (bp JSON..: "time")
instance IsMerkle f => JSON.ToJSON (UTXOBlockProper f) where
  toJSON (UTXOBlockProper prev d (POWTypes.Target t) time) =
    JSON.object
      [ "previous" JSON..= prev
      , "data" JSON..= d
      , "target" JSON..= t
      , "time" JSON..= time
      ]
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
instance IsMerkle f => JSON.FromJSON (UTXOBlock f) where
  parseJSON = JSON.withObject "utxoblock" $ \b ->
    UTXOBlock
      <$> fmap (\(ViaBase58 s :: ViaBase58 "UTXOBlock" ByteString) -> s) (b JSON..: "nonce")
      <*> (b JSON..: "proper")
instance IsMerkle f => JSON.ToJSON (UTXOBlock f) where
  toJSON (UTXOBlock nonce proper) =
     JSON.object ["nonce" JSON..= (JSON.toJSON $ ViaBase58 nonce), "proper" JSON..= proper]
instance Serialise (UTXOBlock Identity) -- XXX questionable. We probably need custom things here.
instance Serialise (UTXOBlock Proxy)


instance (IsMerkle f) => Crypto.CryptoHashable (UTXOBlock f) where
  hashStep = Crypto.genericHashStep "block proper"


instance POWTypes.BlockData UTXOBlock where

  newtype BlockID UTXOBlock = UB'BID { fromUBBID :: Crypto.Hash SHA256 }
    deriving newtype
      (Show, Eq, Ord, Crypto.CryptoHashable, Serialise, ToJSON, FromJSON, ByteRepr)
    deriving (SQL.FromField, SQL.ToField) via ByteRepred (POWTypes.BlockID UTXOBlock)

  type Tx UTXOBlock = Tx

  newtype TxID UTXOBlock = UTXOTxID (Hash SHA256)
    deriving newtype ( Show, Eq, Ord, CryptoHashable, Serialise, ByteRepr
                     , JSON.ToJSON, JSON.FromJSON)
    deriving (SQL.FromField, SQL.ToField) via ByteRepred (POWTypes.TxID UTXOBlock)

  data BlockException UTXOBlock =
                                  WrongAnswer
                                | WrongTarget
                                | AheadOfTime
                                | InternalErr String
    deriving stock    (Show,Generic)
    deriving anyclass (Exception,JSON.ToJSON)

  txID    = UTXOTxID . hash
  blockID = UB'BID   . hash
  blockTransactions = merkleValue . ubpData . ubProper . POWTypes.blockData

  validateHeader bh (POWTypes.Time now) header
    | POWTypes.blockHeight header == 0 = return $ Right () -- skip genesis check.
    | otherwise = do
      answerIsGood <- error "no puzzle check right now"
      return $ if
         | not answerIsGood -> Left WrongAnswer
         | ubpTarget (ubProper $ POWTypes.blockData header) /= POWTypes.retarget bh
                            -> Left WrongTarget
         | t > now + (2 * 60 * 60 * 1000)
                            -> Left AheadOfTime
         | otherwise        -> Right ()
    where
      POWTypes.Time t = POWTypes.blockTime header

  validateBlock = const $ return $ Right ()

  validateTxContextFree = validateTransactionContextFree

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

-- | In-memory overlay for coin state. It contain changes to UTXO set
--   that are not commited to the database.
--
--   Note that it only contains block that are added to blockchain but
--   not rollbacks since latter are already commited.
data StateOverlay
  = OverlayBase  (POWTypes.BH UTXOBlock)
  | OverlayLayer (POWTypes.BH UTXOBlock) Layer StateOverlay

-- | Overlay that is guaranteed to have layer to add UTXOs
data ActiveOverlay = ActiveOverlay Layer StateOverlay
  deriving Generic

-- | Changes to database set generated by single block
data Layer = Layer
  { utxoCreated :: Map.Map BoxId Unspent
  , utxoSpent   :: Map.Map BoxId Unspent
  }

-- | Change to UTXO set
data Change a
  = Added a
  | Spent a

type Unspent = Box

lensCreated, lensSpent :: Lens' Layer (Map.Map BoxId Unspent)
lensCreated = lens utxoCreated (\m x -> m { utxoCreated = x })
lensSpent   = lens utxoSpent   (\m x -> m { utxoSpent   = x })

-------------------------------------------------------------------------------
-- Transaction validation.

-- | Context free TX validation for transactions. This function
--   performs all checks that could be done having only transaction at
--   hand.
validateTransactionContextFree :: Tx -> Either (POWTypes.BlockException UTXOBlock) ()
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

-- | Finally process transaction. Check that sum of inputs is greater or equal to
-- sum of outputs.
processTX
  :: POWBlockIndex.BlockIndexPath (ID (POWTypes.Block UTXOBlock))
  -> ActiveOverlay
  -> Tx
  -> ExceptT (POWTypes.BlockException UTXOBlock) (Query rw) ActiveOverlay
processTX pathInDB overlay tx@Tx{..} = do
  -- Fetch all inputs & check that we can spend them
  inputs <- forM tx'inputs $ \box@BoxInputRef{..} -> do
    let boxid = boxInputRef'id
    case getOverlayBoxId overlay boxid of
      Just (Spent _) -> throwError $ InternalErr "Input already spent"
      Just (Added u) -> return (boxid,u)
      Nothing        -> (,) boxid <$> getDatabaseBox pathInDB boxid
  checkSpendability inputs tx
  -- Update overlay
  let overlay1 = foldl' (\o (boxid,u) -> spendBox boxid u o) overlay inputs
      overlay2 = foldl' (\o (boxid,u) -> createUnspentBox boxid u o) overlay1 $
                        V.map (\b -> (box'id b, b)) tx'outputs
  return overlay2

-- |Validate transaction against block environment.
checkSpendability
  :: V.Vector (BoxId, Unspent)
  -> Tx
  -> ExceptT (POWTypes.BlockException UTXOBlock) (Query rw) ()
checkSpendability inputs tx@Tx{..} = do
  -- XXX TODO: validate transaction against environment with our own machinery.
  return ()

-- | Find whether given UTXO is awaialble to be spent or spent
--   already. We need latter since UTXO could be available in
--   underlying state but spent in overlay and we need to account for
--   that explicitly.
getOverlayBoxId :: ActiveOverlay -> BoxId -> Maybe (Change Unspent)
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



getDatabaseBox
  :: ()
  => POWBlockIndex.BlockIndexPath (ID (POWTypes.Block UTXOBlock))
  -> BoxId
  -> ExceptT (POWTypes.BlockException UTXOBlock) (Query rw) Unspent
-- Check whether output was created in the block
getDatabaseBox (POWBlockIndex.ApplyBlock i path) boxid = do
  isSpentAtBlock i boxid >>= \case
    Just _  -> throwError $ InternalErr "Output is already spent"
    Nothing -> return ()
  isCreatedAtBlock i boxid >>= \case
    Just u  -> return u
    Nothing -> getDatabaseBox path boxid
-- Perform check in block being reverted. If UTXO was create in that
-- block it didn't exist before and we should abort.
getDatabaseBox (POWBlockIndex.RevertBlock i path) boxid = do
  isCreatedAtBlock i boxid >>= \case
    Just _  -> throwError $ InternalErr "Output does not exists"
    Nothing -> return ()
  isSpentAtBlock i boxid >>= \case
    Just u  -> return u
    Nothing -> getDatabaseBox path boxid
getDatabaseBox POWBlockIndex.NoChange boxid = do
  r <- basicQuery1
    "SELECT pk_dest, n_coins \
    \  FROM coin_utxo \
    \  JOIN coin_state ON live_utxo = utxo_id \
    \ WHERE n_out = ? AND tx_hash = ?"
    boxid
  case r of
    Just u  -> return u
    Nothing -> throwError $ InternalErr "No such UTXO"

spendBox :: BoxId -> Unspent -> ActiveOverlay -> ActiveOverlay
spendBox boxid val
  = typed . lensSpent . at boxid .~ Just val

createUnspentBox :: BoxId -> Unspent -> ActiveOverlay -> ActiveOverlay
createUnspentBox boxid val
  = typed . lensCreated . at boxid .~ Just val

isSpentAtBlock :: MonadQueryRO m => ID (POWTypes.Block UTXOBlock) -> BoxId -> m (Maybe Unspent)
isSpentAtBlock i boxid = basicQuery1
  "SELECT pk_dest, n_coins \
  \  FROM coin_utxo \
  \  JOIN coin_utxo_spent ON utxo_id = utxo_ref \
  \ WHERE n_out = ? AND tx_hash = ? AND block_ref = ?"
  (boxid SQL.:. SQL.Only i)

isCreatedAtBlock :: MonadQueryRO m => ID (POWTypes.Block UTXOBlock) -> BoxId -> m (Maybe Unspent)
isCreatedAtBlock i boxid = basicQuery1
  "SELECT pk_dest, n_coins \
  \  FROM coin_utxo \
  \  JOIN coin_utxo_created ON utxo_id = utxo_ref \
  \ WHERE n_out = ? AND tx_hash = ? AND block_ref = ?"
  (boxid SQL.:. SQL.Only i)


