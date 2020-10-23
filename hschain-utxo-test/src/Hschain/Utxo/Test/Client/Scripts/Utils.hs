module Hschain.Utxo.Test.Client.Scripts.Utils(
    debugSend
  , send
  , SendResult(..)
  , newUser
  , initUsers
  , User(..)
  , Scene(..)
  , getTxHash
  , mainScriptUnsafe
  , postTxDebug
  , postTxSuccess
  , postTxFailure
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
  { sendResult'changeId :: Maybe BoxId
  , sendResult'to       :: BoxId
  , sendResult'txHash   :: Maybe TxHash
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
      return $ SendResult (Just fromBox) fromBox Nothing
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
  eTx <- newSendTx from  (Send fromBoxId money to)
  forM eTx $ \(tx, backId, toId) -> do
    logTest $ renderText tx
    resp <- postTx tx
    printTest resp
    return $ SendResult backId toId (getTxHash resp)

getTxHash :: PostTxResponse -> Maybe TxHash
getTxHash PostTxResponse{..} = postTxResponse'value

--------------------------------------------------
-- init three users

data User = User
  { user'wallet  :: !Wallet
  , user'box     :: !(Maybe BoxId)
  }

data Scene = Scene
  { scene'alice    :: !User
  , scene'bob      :: !User
  , scene'john     :: !User
  , scene'master   :: !User
  }

initUsers :: App Scene
initUsers = do
  master        <- newMasterUser
  alice         <- newUser "alice"
  bob           <- newUser "bob"
  john          <- newUser "john"
  initMasterBox <- getMasterBoxId
  setupScene $ Scene
    { scene'alice   = User alice  Nothing
    , scene'bob     = User bob    Nothing
    , scene'john    = User john   Nothing
    , scene'master  = User master (Just initMasterBox)
    }

setupScene :: Scene -> App Scene
setupScene scene@Scene{..} = do
  initMasterBox <- getMasterBoxId
  logTest "Start state:"
  st <- getState
  logTest $ renderText st
  let master = user'wallet scene'master
      alice  = user'wallet scene'alice
      bob    = user'wallet scene'bob
      john   = user'wallet scene'john

  SendResult (Just masterBox1) aliceBox _ <- debugSend True "Master sends 10 to alice" master initMasterBox alice 10
  SendResult (Just masterBox2) bobBox _   <- debugSend True "Master sends 10 to bob"   master masterBox1    bob   10
  SendResult (Just masterBox3) johnBox _  <- debugSend True "Master sends 10 to john"  master masterBox2    john  10
  return $ scene
    { scene'alice  = withBox scene'alice  aliceBox
    , scene'bob    = withBox scene'bob    bobBox
    , scene'john   = withBox scene'john   johnBox
    , scene'master = withBox scene'master masterBox3
    }
    where
      withBox u b = u { user'box = Just b }


postTxDebug :: Bool -> Text -> Tx -> App (Either Text TxHash)
postTxDebug isSuccess msg tx = do
  logTest msg
  logTest "Going to post TX:"
  logTest $ renderText tx
  resp <- postTx tx
  printTest $ postTxResponse'value resp
  st <- getState
  logTest $ renderText st
  wait
  testCase msg $ (isJust $ getTxHash resp) == isSuccess
  return $ maybe  (Left "Error postTxDebug") Right $ postTxResponse'value resp
  where
    wait = sleep 0.1

postTxSuccess :: Text -> Tx -> App ()
postTxSuccess msg tx = void $ postTxDebug True msg tx

postTxFailure :: Text -> Tx -> App ()
postTxFailure msg tx = void $ postTxDebug False msg tx



