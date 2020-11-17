-- | Create specific TXs for lightning network
module Hschain.Utxo.Test.Client.Scripts.Lightning.Tx(
    fundingTx
  , getSharedBoxId
  , commitmentTx
  , closeChanTx
) where

import HSChain.Crypto (ByteRepr(..))

import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Int
import Data.Maybe

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Test.Client.Wallet
import Hschain.Utxo.Test.Client.Scripts.Lightning.Protocol

import qualified Data.Vector as V

type Balance = (Money, Money)

-- | Funding TX
fundingTx :: MonadIO io => Wallet -> (Money, Money) -> [BoxId] -> PublicKey -> io Tx
fundingTx wallet (value, change) inputIds otherPubKey = newProofTx  (getProofEnv wallet) $ Tx
  { tx'inputs     = V.fromList $ fmap (singleOwnerBoxRef wallet) inputIds
  , tx'outputs    = V.fromList $ catMaybes [Just fundBox, changeBox]
  , tx'dataInputs = []
  }
  where
    fundBox = Box
      { box'value  = value
      , box'script = mainScriptUnsafe $ toSigma $ checkMultiSig (int 2) (fromVec keys) (fromVec indices)
      , box'args   = mempty
      }
      where
        keys = fmap (bytes . encodeToBS) [ownerPubKey, otherPubKey]
        indices = fmap int [0, 1]

    changeBox
      | change <= 0 = Nothing
      | otherwise   = Just $ singleSpendBox change ownerPubKey

    ownerPubKey = getWalletPublicKey wallet

getSharedBoxId :: Tx -> BoxId
getSharedBoxId tx = computeBoxId (computeTxId tx) 0

-- | Commitment TX
commitmentTx :: PublicKey -> BoxId -> Balance -> PublicKey -> Int64 -> ByteString -> [Htlc] -> Tx
commitmentTx myPk commonBoxId (myValue, otherValue) otherPk spendDelay revokeHash htlcs =
  Tx
    { tx'inputs     = [commonInput commonBoxId]
    , tx'outputs    = V.fromList $ [myBox, otherBox] ++ fmap fromHtlc htlcs
    , tx'dataInputs = []
    }
  where
    myBox = Box
      { box'value  = myValue
      , box'script = mainScriptUnsafe revokeScript
      , box'args   = mempty
      }
      where
        revokeScript =
              (pk' myPk &&* (toSigma $ getHeight >* getBoxPostHeight getSelf + int (fromIntegral spendDelay)))
          ||* (pk' otherPk &&* (toSigma $ sha256 readKey ==* bytes revokeHash))

    readKey = listAt getBytesVars 0

    otherBox = singleSpendBox otherValue otherPk

    fromHtlc Htlc{..} = Box
      { box'value  = abs $ htlc'value
      , box'script = mainScriptUnsafe $ htlcScript htlc'payHash (if (htlc'value > 0) then (otherPk, myPk) else (myPk, otherPk)) (fromIntegral htlc'time)
      , box'args   = mempty
      }

    htlcScript payHash (receiverKey, senderKey) time =
          (pk' receiverKey &&* (toSigma $ sha256 readKey ==* bytes payHash))
      ||* (pk' senderKey &&* (toSigma $ getHeight >=* int time))

closeChanTx :: BoxId -> Balance -> (PublicKey, PublicKey) -> Tx
closeChanTx commonBoxId (valA, valB) (pkA, pkB) =
  Tx
    { tx'inputs     = [commonInput commonBoxId]
    , tx'outputs    = [singleSpendBox valA pkA, singleSpendBox valB pkB]
    , tx'dataInputs = []
    }

commonInput :: BoxId -> BoxInputRef a
commonInput commonBoxId = BoxInputRef
  { boxInputRef'id    = commonBoxId
  , boxInputRef'proof = Nothing
  , boxInputRef'args  = mempty
  , boxInputRef'sigs  = mempty
  , boxInputRef'sigMask = SigAll
  }

