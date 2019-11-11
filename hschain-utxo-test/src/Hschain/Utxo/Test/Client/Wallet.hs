module Hschain.Utxo.Test.Client.Wallet(
    Wallet(..)
  , newWallet
  , allocAddress
  , getBalance
  , getBoxBalance
  , newSendTx
  , Send(..)
  , getOwnerProof
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class

import Data.Maybe
import Data.UUID

import System.Random

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Test.Client.Monad

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

data Wallet = Wallet
  { wallet'user       :: !UserId
  , wallet'publicKey  :: !PubKey
  , wallet'privateKey :: !PrivateKey
  , wallet'utxos      :: !(TVar [BoxId])
  }

newWallet :: MonadIO io => UserId -> PrivateKey -> io Wallet
newWallet userId pk = liftIO $ do
  utxos <- newTVarIO []
  return $ Wallet userId (unUserId userId) pk utxos

-- | Generates address name and saves it to the wallet utxo list
allocAddress :: MonadIO io => Wallet -> io BoxId
allocAddress wallet@Wallet{..} = liftIO $ do
  addr <- newAddress wallet
  atomically $ modifyTVar' wallet'utxos $ (addr : )
  return addr


-- | Generates new address name
newAddress :: MonadIO io => Wallet -> io BoxId
newAddress Wallet{..} = liftIO $ do
  idx <- fmap (T.pack .show) (randomIO :: IO UUID)
  return $ BoxId $ mconcat [ userId, "-", idx]
  where
    userId = (\(UserId uid) -> uid) wallet'user

getBalance :: Wallet -> App Money
getBalance wallet@Wallet{..} = do
  xs <- liftIO $ readTVarIO wallet'utxos
  fmap (sum . catMaybes) $ mapM getBoxBalance xs

getOwnerProof :: Wallet -> Proof
getOwnerProof Wallet{..} =
  Proof $ S.fromList [wallet'publicKey]

data Send = Send
  { send'from    :: !BoxId
  , send'to      :: !BoxId
  , send'back    :: !BoxId
  , send'amount  :: !Money
  , send'recepientWallet :: !Wallet -- TODO: we need it right now, substitute it with public key in the future
  }

data SendBack = SendBack
  { sendBack'totalAmount  :: !Money
  , sendBack'backBox      :: !BoxId
  }

newSendTx :: Wallet -> Send -> App Tx
newSendTx wallet send@Send{..} = fmap (toSendTx wallet send) getSendBack
  where
    getSendBack = do
      totalAmount <- fmap (fromMaybe 0) $ getBoxBalance send'from
      return $ SendBack totalAmount send'back

toSendTx :: Wallet -> Send -> SendBack -> Tx
toSendTx wallet Send{..} SendBack{..} = Tx
  { tx'inputs   = V.fromList [inputBox]
  , tx'outputs  = V.fromList $ catMaybes [senderUtxo, Just receiverUtxo]
  , tx'proof    = getOwnerProof wallet
  , tx'args     = M.empty
  }
  where
    inputBox = send'from

    senderUtxo
      | sendBack'totalAmount > send'amount = Just $ Box
                { box'id     = sendBack'backBox
                , box'value  = sendBack'totalAmount - send'amount
                , box'script = toScript $ pk (text $ wallet'publicKey wallet)
                , box'args   = M.empty
                }
      | otherwise                 = Nothing

    receiverUtxo = Box
      { box'id     = send'to
      , box'value  = send'amount
      , box'script = toScript $ pk (text $ wallet'publicKey send'recepientWallet)
      , box'args   = M.empty
      }

