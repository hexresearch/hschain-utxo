-- | This module defines test user wallet.
-- It keeps balances, keys and allocates data for transactions.
module Hschain.Utxo.Test.Client.Wallet(
    Wallet(..)
  , newWallet
  , getWalletPublicKey
  , allocAddress
  , getBalance
  , getBoxBalance
  , newSendTx
  , Send(..)
  , getSigmaForProof
  , getOwnerProof
  , getOwnerProofUnsafe
  , getProofEnv
  , newProofOrFail
  , getTxSigmaUnsafe
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Except

import Data.Fix
import Data.Maybe
import Data.Text (Text)
import Data.UUID

import System.Random

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Test.Client.Monad

import qualified Data.Text as T
import qualified Data.Vector as V

-- | User wallet
data Wallet = Wallet
  { wallet'user       :: !UserId           -- ^ User id for test print
  , wallet'privateKey :: !Secret           -- ^ User private key
  , wallet'utxos      :: !(TVar [BoxId])   -- ^ User UTXOs
  }

-- | Allocate new wallet
newWallet :: MonadIO io => UserId -> Secret -> io Wallet
newWallet userId pubKey = liftIO $ do
  utxos <- newTVarIO []
  return $ Wallet userId pubKey utxos

-- | Read public key
getWalletPublicKey :: Wallet -> PublicKey
getWalletPublicKey = getPublicKey . wallet'privateKey

-- | Generates address name and saves it to the wallet utxo list
allocAddress :: MonadIO io => Wallet -> io BoxId
allocAddress wallet@Wallet{..} = liftIO $ do
  addr <- newAddress wallet
  atomically $ modifyTVar' wallet'utxos $ (addr : )
  return addr

-- | Gets user proof environment or list of keys
getProofEnv :: Wallet -> ProofEnv
getProofEnv Wallet{..} = proofEnvFromKeys [getKeyPair wallet'privateKey]

-- | Generates new address name
newAddress :: MonadIO io => Wallet -> io BoxId
newAddress Wallet{..} = liftIO $ do
  idx <- fmap (T.pack .show) (randomIO :: IO UUID)
  return $ BoxId $ mconcat [ userId, "-", idx]
  where
    userId = (\(UserId uid) -> uid) wallet'user

-- | Query the user balance.
getBalance :: Wallet -> App Money
getBalance Wallet{..} = do
  xs <- liftIO $ readTVarIO wallet'utxos
  fmap (sum . catMaybes) $ mapM getBoxBalance xs

-- | Create proof for a most simple expression of @pk user-key@
getOwnerProof :: MonadIO io => Wallet -> io (Either Text Proof)
getOwnerProof w@Wallet{..} =
  liftIO $ newProof env $ Fix $ SigmaPk (getWalletPublicKey w)
  where
    env = toProofEnv [getKeyPair wallet'privateKey]

getOwnerProofUnsafe :: Wallet -> App Proof
getOwnerProofUnsafe wallet =
  either throwError pure =<< getOwnerProof wallet

-- | Send money from one user to another
data Send = Send
  { send'from    :: !BoxId
  -- ^ from user box
  , send'to      :: !BoxId
  -- ^ to user box
  , send'back    :: !BoxId
  -- ^ where to put exchange
  , send'amount  :: !Money
  -- ^ amount of money to send
  , send'recepientWallet :: !Wallet -- TODO: we need it right now, substitute it with public key in the future
  }

-- | Data to hold the data for exchange send
data SendBack = SendBack
  { sendBack'totalAmount  :: !Money  -- ^ amount of money
  , sendBack'backBox      :: !BoxId  -- ^ where to send exchange
  }

-- | Creates script that sends money from user to another
newSendTx :: Wallet -> Send -> App (Either Text Tx)
newSendTx wallet send@Send{..} = do
  back <- getSendBack
  preTx <- toSendTx wallet send back Nothing
  eSigma <- getTxSigma preTx
  fmap join $ forM eSigma $ \sigma -> do
    let env = getProofEnv wallet
    eProof <- liftIO $ newProof env sigma
    case eProof of
      Right proof -> fmap Right $ toSendTx wallet send back (Just proof)
      Left err    -> return $ Left err
  where
    getSendBack = do
      totalAmount <- fmap (fromMaybe 0) $ getBoxBalance send'from
      return $ SendBack totalAmount send'back

newProofOrFail :: ProofEnv -> Sigma PublicKey -> App Proof
newProofOrFail env expr = do
  eProof <- liftIO $ newProof env expr
  case eProof of
    Right proof -> return proof
    Left err    -> throwError err

getTxSigmaUnsafe :: Tx -> App (Sigma PublicKey)
getTxSigmaUnsafe tx = either throwError pure =<< getTxSigma tx

getSigmaForProof :: Tx -> App (Sigma PublicKey)
getSigmaForProof tx = getTxSigmaUnsafe tx

-- | Sends money with exchange
toSendTx :: Wallet -> Send -> SendBack -> Maybe Proof -> App Tx
toSendTx wallet Send{..} SendBack{..} mProof = do
  return $ Tx
        { tx'inputs   = V.fromList [inputBox]
        , tx'outputs  = V.fromList $ catMaybes [senderUtxo, Just receiverUtxo]
        , tx'proof    = mProof
        , tx'args     = mempty
        }
  where
    inputBox = send'from

    senderUtxo
      | sendBack'totalAmount > send'amount = Just $ Box
                { box'id     = sendBack'backBox
                , box'value  = sendBack'totalAmount - send'amount
                , box'script = mainScriptUnsafe $ pk (text $ publicKeyToText $ getWalletPublicKey wallet)
                , box'args   = mempty
                }
      | otherwise                 = Nothing

    receiverUtxo = Box
      { box'id     = send'to
      , box'value  = send'amount
      , box'script = mainScriptUnsafe $ pk (text $ publicKeyToText $ getWalletPublicKey send'recepientWallet)
      , box'args   = mempty
      }

