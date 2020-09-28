{-# LANGUAGE DerivingStrategies #-}
-- | This module defines test user wallet.
-- It keeps balances, keys and allocates data for transactions.
module Hschain.Utxo.Test.Client.Wallet(
    Wallet(..)
  , UserId(..)
  , newWallet
  , getWalletPublicKey
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
  , singleOwnerSigmaExpr
  , appendSenderReceiverIds
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Except

import Data.Fix
import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector)


import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Test.Client.Monad

import qualified Data.Vector as V

-- | User wallet
data Wallet = Wallet
  { wallet'user       :: !UserId           -- ^ User id for test print
  , wallet'privateKey :: !Secret           -- ^ User private key
  , wallet'utxos      :: !(TVar [BoxId])   -- ^ User UTXOs
  }

-- | User identifier.
newtype UserId = UserId { unUserId :: Text }
  deriving newtype  (Show, Eq)

-- | Allocate new wallet
newWallet :: MonadIO io => UserId -> Secret -> io Wallet
newWallet userId pubKey = liftIO $ do
  utxos <- newTVarIO []
  return $ Wallet userId pubKey utxos

-- | Read public key
getWalletPublicKey :: Wallet -> PublicKey
getWalletPublicKey = getPublicKey . wallet'privateKey

-- | Gets user proof environment or list of keys
getProofEnv :: Wallet -> ProofEnv
getProofEnv Wallet{..} = proofEnvFromKeys [getKeyPair wallet'privateKey]

-- | Query the user balance.
getBalance :: Wallet -> App Money
getBalance Wallet{..} = do
  xs <- liftIO $ readTVarIO wallet'utxos
  fmap (sum . catMaybes) $ mapM getBoxBalance xs

-- | Create proof for a most simple expression of @pk user-key@
getOwnerProof :: MonadIO io => Wallet -> Tx -> io (Either Text Proof)
getOwnerProof w@Wallet{..} tx =
  liftIO $ newProof env (Fix $ SigmaPk (getWalletPublicKey w)) (computeTxId tx)
  where
    env = toProofEnv [getKeyPair wallet'privateKey]

getOwnerProofUnsafe :: Wallet -> Tx -> App Proof
getOwnerProofUnsafe wallet tx =
  either throwError pure =<< getOwnerProof wallet tx

-- | Send money from one user to another
data Send = Send
  { send'from    :: !BoxId
  -- ^ from user box
  , send'amount  :: !Money
  -- ^ amount of money to send
  , send'recepientWallet :: !Wallet -- TODO: we need it right now, substitute it with public key in the future
  }

-- | Data to hold the data for exchange send
data SendBack = SendBack
  { sendBack'totalAmount  :: !Money  -- ^ amount of money
  }

-- | Creates script that sends money from user to another
newSendTx :: Wallet -> Send -> App (Either Text (Tx, Maybe BoxId, BoxId))
newSendTx wallet send@Send{..} = do
  back <- getSendBack
  toSendTx wallet send back
  where
    getSendBack = do
      totalAmount <- fmap (fromMaybe 0) $ getBoxBalance send'from
      return $ SendBack totalAmount

newProofOrFail :: ProofEnv -> Sigma PublicKey -> TxId -> App Proof
newProofOrFail env expr message = do
  eProof <- liftIO $ newProof env expr message
  case eProof of
    Right proof -> return proof
    Left err    -> throwError err

getTxSigmaUnsafe :: Tx -> App (Vector (Sigma PublicKey))
getTxSigmaUnsafe tx = either throwError pure =<< getTxSigma tx

getSigmaForProof :: Tx -> App (Vector (Sigma PublicKey))
getSigmaForProof tx = getTxSigmaUnsafe tx

singleOwnerSigmaExpr :: Wallet -> Sigma PublicKey
singleOwnerSigmaExpr wallet = Fix $ SigmaPk $ getWalletPublicKey wallet

-- | Sends money with exchange
--
-- returns tripple: (tx, box address for change if needed, receiver output result)
toSendTx :: Wallet -> Send -> SendBack -> App (Either Text (Tx, Maybe BoxId, BoxId))
toSendTx wallet Send{..} SendBack{..} =
  fmap (fmap appendSenderReceiverIds) $ newProofTxOrFail (getProofEnv wallet) preTx
  where
    preTx = PreTx
      { preTx'inputs  = V.fromList [ExpectedBox (Just $ singleOwnerSigmaExpr wallet) inputBox]
      , preTx'outputs = V.fromList $ catMaybes [senderUtxo, Just receiverUtxo]
      }

    inputBox = BoxInputRef
      { boxInputRef'id    = send'from
      , boxInputRef'args  = mempty
      , boxInputRef'proof = Nothing
      }

    senderUtxo
      | sendBack'totalAmount > send'amount = Just $ PreBox
                { preBox'value  = sendBack'totalAmount - send'amount
                , preBox'script = mainScriptUnsafe $ pk (text $ publicKeyToText $ getWalletPublicKey wallet)
                , preBox'args   = mempty
                }
      | otherwise                 = Nothing

    receiverUtxo = PreBox
      { preBox'value  = send'amount
      , preBox'script = mainScriptUnsafe $ pk (text $ publicKeyToText $ getWalletPublicKey send'recepientWallet)
      , preBox'args   = mempty
      }

appendSenderReceiverIds :: Tx -> (Tx, Maybe BoxId, BoxId)
appendSenderReceiverIds tx = (tx, sender, receiver)
  where
    (sender, receiver) = extractSenderReceiverIds tx

extractSenderReceiverIds :: Tx -> (Maybe BoxId, BoxId)
extractSenderReceiverIds tx = case tx'outputs tx of
  [receiver]         -> (Nothing, box'id receiver)
  [sender, receiver] -> (Just $ box'id sender, box'id receiver)
  _                  -> error "Not enough outputs fot TX"

