module Hschain.Utxo.Test.Client.Scripts.Utils(
    debugSend
  , send
  , SendResult(..)
  , newUser
  , initMasterBox
  , initUsers
  , User(..)
  , Scene(..)
  , getTxHash
  , mainScriptUnsafe
) where

import Control.Timeout
import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
import Data.Text (Text)

import Hschain.Utxo.Lang
import Hschain.Utxo.API.Rest
import Hschain.Utxo.Test.Client.Wallet
import Hschain.Utxo.Test.Client.Monad

data SendResult = SendResult
  { sendResult'from   :: BoxId
  , sendResult'to     :: BoxId
  , sendResult'txHash :: Maybe TxHash
  } deriving (Show, Eq)

debugSend :: Bool -> Text -> Wallet -> BoxId -> Wallet -> Money -> App SendResult
debugSend isSuccess msg from fromBox to amount = do
  logTest msg
  eRes <- send from fromBox to amount
  case eRes of
    Right res -> do
      printTest res
      st <- getState
      logTest $ renderText st
      wait
      testCase msg (isJust (sendResult'txHash res) == isSuccess)
      return res
    Left err -> do
      testCase (mconcat [msg, " ", err]) (False == isSuccess)
      return $ SendResult fromBox fromBox Nothing
  where
    wait = sleep 0.25

newUser :: Text -> App Wallet
newUser name = do
  secret <- liftIO newSecret
  newWallet (UserId name) secret

newMasterUser :: App Wallet
newMasterUser = do
  secret <- getMasterSecret
  newWallet (UserId "master") secret

send :: Wallet -> BoxId -> Wallet -> Money -> App (Either Text SendResult)
send from fromBoxId to money = do
  toBoxId <- allocAddress to
  backBoxId <- allocAddress from
  eTx <- newSendTx from  (Send fromBoxId toBoxId backBoxId money to)
  forM eTx $ \tx -> do
    logTest $ renderText tx
    resp <- postTx tx
    printTest resp
    return $ SendResult backBoxId toBoxId (getTxHash resp)

getTxHash :: PostTxResponse -> Maybe TxHash
getTxHash PostTxResponse{..} = postTxResponse'value

--------------------------------------------------
-- init three users

data User = User
  { user'wallet  :: !Wallet
  , user'box     :: !BoxId
  }

data Scene = Scene
  { scene'alice    :: !User
  , scene'bob      :: !User
  , scene'john     :: !User
  , scene'master   :: !User
  }

initUsers :: App Scene
initUsers = do
  master <- newMasterUser
  alice  <- newUser "alice"
  bob    <- newUser "bob"
  john   <- newUser "john"
  setupScene $ Scene
    { scene'alice   = User alice  (box "alice")
    , scene'bob     = User bob    (box "bob")
    , scene'john    = User john   (box "john")
    , scene'master  = User master initMasterBox
    }
  where
    box user = BoxId $ mconcat [user, ":box-0"]

setupScene :: Scene -> App Scene
setupScene scene@Scene{..} = do
  logTest "Start state:"
  st <- getState
  logTest $ renderText st
  let master = user'wallet scene'master
      alice  = user'wallet scene'alice
      bob    = user'wallet scene'bob
      john   = user'wallet scene'john

  SendResult masterBox1 aliceBox _ <- debugSend True "Master sends 10 to alice" master initMasterBox alice 10
  SendResult masterBox2 bobBox _   <- debugSend True "Master sends 10 to bob"   master masterBox1    bob   10
  SendResult masterBox3 johnBox _  <- debugSend True "Master sends 10 to john"  master masterBox2    john  10
  return $ scene
    { scene'alice  = withBox scene'alice  aliceBox
    , scene'bob    = withBox scene'bob    bobBox
    , scene'john   = withBox scene'john   johnBox
    , scene'master = withBox scene'master masterBox3
    }
    where
      withBox u b = u { user'box = b }

