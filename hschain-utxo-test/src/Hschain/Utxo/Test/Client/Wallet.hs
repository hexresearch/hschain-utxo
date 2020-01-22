module Hschain.Utxo.Test.Client.Wallet(
    Wallet(..)
  , newWallet
  , getWalletPublicKey
  , allocAddress
  , getBalance
  , getBoxBalance
  , newSendTx
  , Send(..)
  , getOwnerProof
  , getOwnerProofUnsafe
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

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

data Wallet = Wallet
  { wallet'user       :: !UserId
  , wallet'privateKey :: !Secret
  , wallet'utxos      :: !(TVar [BoxId])
  }

newWallet :: MonadIO io => UserId -> Secret -> io Wallet
newWallet userId pk = liftIO $ do
  utxos <- newTVarIO []
  return $ Wallet userId pk utxos

getWalletPublicKey :: Wallet -> PublicKey
getWalletPublicKey = getPublicKey . wallet'privateKey

-- | Generates address name and saves it to the wallet utxo list
allocAddress :: MonadIO io => Wallet -> io BoxId
allocAddress wallet@Wallet{..} = liftIO $ do
  addr <- newAddress wallet
  atomically $ modifyTVar' wallet'utxos $ (addr : )
  return addr

getProofEnv :: Wallet -> ProofEnv
getProofEnv Wallet{..} = proofEnvFromKeys [getKeyPair wallet'privateKey]

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

getOwnerProof :: MonadIO io => Wallet -> io (Either Text Proof)
getOwnerProof w@Wallet{..} =
  liftIO $ newProof env $ Fix $ SigmaPk (getWalletPublicKey w)
  where
    env = toProofEnv [getKeyPair wallet'privateKey]

getOwnerProofUnsafe :: Wallet -> App Proof
getOwnerProofUnsafe wallet =
  either throwError pure =<< getOwnerProof wallet

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
newSendTx wallet send@Send{..} = do
  back <- getSendBack
  preTx <- toSendTx wallet send back Nothing
  sigma <- getSigmaForProof preTx
  let env = getProofEnv wallet
  proof <- newProofOrFail env sigma
  toSendTx wallet send back (Just proof)
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

toSendTx :: Wallet -> Send -> SendBack -> Maybe Proof -> App Tx
toSendTx wallet Send{..} SendBack{..} mProof = do
  return $ Tx
        { tx'inputs   = V.fromList [inputBox]
        , tx'outputs  = V.fromList $ catMaybes [senderUtxo, Just receiverUtxo]
        , tx'proof    = mProof
        , tx'args     = M.empty
        }
  where
    inputBox = send'from

    senderUtxo
      | sendBack'totalAmount > send'amount = Just $ Box
                { box'id     = sendBack'backBox
                , box'value  = sendBack'totalAmount - send'amount
                , box'script = toScript $ pk (text $ publicKeyToText $ getWalletPublicKey wallet)
                , box'args   = M.empty
                }
      | otherwise                 = Nothing

    receiverUtxo = Box
      { box'id     = send'to
      , box'value  = send'amount
      , box'script = toScript $ pk (text $ publicKeyToText $ getWalletPublicKey send'recepientWallet)
      , box'args   = M.empty
      }

