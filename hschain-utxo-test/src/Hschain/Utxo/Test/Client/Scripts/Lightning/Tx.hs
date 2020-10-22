-- | Create specific TXs for lightning network
module Hschain.Utxo.Test.Client.Scripts.Lightning.Tx(
    fundingTx
  , getSharedBoxId
  , commitmentTx
) where

import Control.Monad.IO.Class

import Data.Maybe

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Test.Client.Wallet

import qualified Data.Vector as V

type Balance = (Money, Money)

-- | Funding TX
fundingTx :: MonadIO io => Wallet -> (Money, Money) -> [BoxId] -> PublicKey -> io Tx
fundingTx wallet (value, change) inputIds otherPubKey = newProofTx  (getProofEnv wallet) $ Tx
  { tx'inputs  = V.fromList $ fmap (singleOwnerBoxRef wallet) inputIds
  , tx'outputs = V.fromList $ catMaybes [Just fundBox, changeBox]
  }
  where
    fundBox = Box
      { box'value  = value
      , box'script = mainScriptUnsafe $ toSigma $ checkMultiSig (int 2) (fromVec keys) (fromVec indices)
      , box'args   = mempty
      }
      where
        keys = fmap (text . publicKeyToText) [ownerPubKey, otherPubKey]
        indices = fmap int [0, 1]

    changeBox
      | change <= 0 = Nothing
      | otherwise   = Just $ singleSpendBox change ownerPubKey

    ownerPubKey = getWalletPublicKey wallet

getSharedBoxId :: Tx -> BoxId
getSharedBoxId tx = computeBoxId (computeTxId tx) 0

-- | Commitment TX
commitmentTx :: MonadIO io => Wallet -> BoxId -> Balance -> io Tx
commitmentTx = undefined

