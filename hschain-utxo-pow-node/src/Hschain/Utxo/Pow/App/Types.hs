-- |Hschain.Utxo.Pow.App.Types
--
-- Full fledged PoW consensus node, with external REST API.
--
-- Copyright (C) 2020 ...

-- Please keep switched off -Wno-orphans.
-- We need an instance of CryptoHashable of elliptic curve
-- scalar (Ed.Scalarbelow ) provided by very much external package.
-- We cannot fork that package and add an instance there.

{-# OPTIONS  -Wno-orphans                                    #-}
{-# LANGUAGE DataKinds                                       #-}
{-# LANGUAGE DeriveAnyClass, DerivingVia, DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts                                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving                      #-}
{-# LANGUAGE MultiWayIf                                      #-}
{-# LANGUAGE UndecidableInstances                            #-}
module Hschain.Utxo.Pow.App.Types where

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

import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL

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

-------------------------------------------------------------------------------
-- The Block.

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
instance Serialise (UTXOBlock Identity)
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
    deriving stock    (Show,Generic)
    deriving anyclass (Exception,JSON.ToJSON)

  blockID b = let Hashed h = hashed b in UB'BID h
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


