module Hschain.Utxo.Test.Client.Scripts.Utils(
    debugSend
  , send
  , SendResult(..)
  , newUser
  , initMasterBox
  , initUsers
  , User(..)
  , Scene(..)
) where

import Control.Timeout

import Data.Text (Text)

import Text.Show.Pretty

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.State.Pretty
import Hschain.Utxo.Test.Client.Proc
import Hschain.Utxo.Test.Client.Wallet

import qualified Data.Text.IO as T

import qualified Hschain.Utxo.API.Client as C

data SendResult = SendResult
  { sendResult'from :: BoxId
  , sendResult'to   :: BoxId
  } deriving (Show, Eq)

debugSend :: Text -> Wallet -> BoxId -> Wallet -> Money -> IO SendResult
debugSend msg from fromBox to amount = do
  T.putStrLn msg
  res <- send from fromBox to amount
  print res
  st <- C.call (wallet'client from) C.getState
  T.putStrLn $  either (const mempty) renderText st
  wait
  return res
  where
    wait = sleep 0.25

newUser :: Text -> C.ClientSpec -> IO Wallet
newUser name client =
  newWallet (UserId name) (mconcat [name, "-private-key"]) client

send :: Wallet -> BoxId -> Wallet -> Money -> IO SendResult
send from fromBoxId to money = do
  toBoxId <- allocAddress to
  backBoxId <- allocAddress from
  tx <- newSendTx from  (Send fromBoxId toBoxId backBoxId money to)
  T.putStrLn $ renderText tx
  resp <- C.call (wallet'client from) (C.postTx tx)
  print resp
  return $ SendResult backBoxId toBoxId

-- | Initial master box for default genesis.
-- All funds belong to master-user.
initMasterBox :: BoxId
initMasterBox = BoxId "master:box-0"

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

initUsers :: C.ClientSpec -> IO Scene
initUsers client = do
  master <- newUser "master" client
  alice  <- newUser "alice"  client
  bob    <- newUser "bob"    client
  john   <- newUser "john"   client
  putStrLn "Start state:"
  st <- C.call (wallet'client master) C.getState
  T.putStrLn $  either (const mempty) renderText st

  SendResult masterBox1 aliceBox <- debugSend "Master sends 10 to alice" master initMasterBox alice 10
  SendResult masterBox2 bobBox   <- debugSend "Master sends 10 to bob"   master masterBox1    bob   10
  SendResult masterBox3 johnBox  <- debugSend "Master sends 10 to john"  master masterBox2    john  10
  return $ Scene
    { scene'alice   = User alice  aliceBox
    , scene'bob     = User bob    bobBox
    , scene'john    = User john   johnBox
    , scene'master  = User master masterBox3
    }



