-- | Alice pays to Bob but also reserves the ability to get the money back
-- until certain height in the blockchain.
--
-- This is example of timed transaction.
module Hschain.Utxo.Test.Client.Scripts.PayForCofee where

import Prelude hiding ((<*))
import Control.Timeout

import Data.Boolean
import Data.Maybe
import Data.String
import Data.Text (Text)

import Text.Show.Pretty

import Hschain.Utxo.Test.Client.Monad (App, logTest, printTest, testCase, testTitle)
import Hschain.Utxo.Test.Client.Wallet
import Hschain.Utxo.Test.Client.Scripts.Utils

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Build

import qualified Hschain.Utxo.API.Client as C

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Hschain.Utxo.Test.Client.Monad as M

data SendBack = SendBack
  { sendBack'totalAmount  :: !Money
  , sendBack'backBox      :: !BoxId
  , sendBack'refundBox    :: !BoxId
  }

-- | Script function
-- In this scenario Bob gets his delayed money.
payForCofeeBob :: App ()
payForCofeeBob = do
  testTitle "Pay with delay: Bob gets his money."
  Scene{..} <- initUsers
  let (User alice  aliceBox1)  = scene'alice
      (User bob    bobBox1)    = scene'bob
      (User john   johnBox1)   = scene'john
      (User master masterBox1) = scene'master
  SendRes aliceBox2 aliceOrBobBox _ <- debugSendDelayed True
        "Message alice sends to bob 2 coins delayed by 2 steps of blockchain"
        alice aliceBox1 bob 2 2
  SendResult bobBox3 johnBox2 _ <- debugSend False
        "Bob tries to send 2 coins to john (it should fail on delayed condition)"
        bob aliceOrBobBox john 2
  SendResult masterBox2 johnBox3 _ <- debugSend True
        "Master sends to john 1 coin"
        master masterBox1 john 1
  SendResult masterBox3 johnBox4 _ <- debugSend True
        "Master sends to john 1 coin"
        master masterBox2 john 1
  SendResult bobBox4 johnBox6 _ <- debugSend True
        "Bob tries to send 1 coins to john (it should proceed, condition is ok now)"
        bob aliceOrBobBox john 1
  return ()

-- | Script function
-- In this scenario Alice gets her money back.
payForCofeeAlice :: App ()
payForCofeeAlice = do
  testTitle "Pay with delay: Alice gets her money back."
  Scene{..} <- initUsers
  let (User alice  aliceBox1)  = scene'alice
      (User bob    bobBox1)    = scene'bob
      (User john   johnBox1)   = scene'john
      (User master masterBox1) = scene'master
  SendRes aliceBox2 aliceOrBobBox _ <- debugSendDelayed True
        "Message alice sends to bob 2 coins delayed by 2 steps of blockchain"
        alice aliceBox1 bob 2 2
  SendResult bobBox3 johnBox2 _ <- debugSend False
        "Bob tries to send 2 coins to john (it should fail on delayed condition)"
        bob aliceOrBobBox john 2
  SendResult masterBox2 johnBox3 _ <- debugSend True
        "Master sends to john 1 coin"
        master masterBox1 john 1
  SendResult masterBox2 johnBox4 _ <- debugSend True
        "Alice sends to john 2 coins. It should proceed."
        alice aliceOrBobBox john 2
  SendResult bobBox4 johnBox6 _ <- debugSend False
        "Bob tries to send 1 coins to john (it should fail, Alice already taken the money)"
        bob aliceOrBobBox john 1
  return ()

data SendDelayed = SendDelayed
  { sendDelayed'from   :: !BoxId
  , sendDelayed'to     :: !BoxId
  , sendDelayed'back   :: !BoxId
  , sendDelayed'refund :: !BoxId
  , sendDelayed'amount :: !Money
  , sendDelayed'remain :: !Money
  , sendDelayed'height :: !Int
  , sendDelayed'recepientWallet :: !Wallet -- we need it while sigma-proofs are not implemented yet
  }

data SendResultDelayed = SendRes
  { sendRes'from        :: BoxId
  , sendRes'refundOrTo  :: BoxId
  , sendRes'txHash      :: !(Maybe TxHash)
  } deriving (Show, Eq)

----------------------------------------
-- transactions

debugSendDelayed :: Bool -> Text -> Wallet -> BoxId -> Wallet -> Int -> Money -> App SendResultDelayed
debugSendDelayed isSuccess msg from fromBox to heightDiff amount = do
  logTest msg
  res <- sendTxDelayed from fromBox to heightDiff amount
  printTest res
  st <- M.getState
  logTest $  renderText st
  wait
  testCase msg $ (isJust (sendRes'txHash res) == isSuccess)
  return res
  where
    wait = sleep 0.25


sendTxDelayed :: Wallet -> BoxId -> Wallet -> Int -> Money -> App SendResultDelayed
sendTxDelayed from fromBox to delayDiff amount = do
  toBox     <- allocAddress to
  backBox   <- allocAddress from
  refundBox <- allocAddress from
  currentHeight <- fmap fromInteger $ M.getHeight
  totalAmount <- fmap (fromMaybe 0) $ M.getBoxBalance fromBox
  let send = SendDelayed fromBox toBox backBox refundBox amount (totalAmount - amount) (currentHeight + delayDiff) to
      tx = toSendTxDelayed from send
  logTest $ renderText tx
  txResp <- M.postTx tx
  logTest $ fromString $ ppShow txResp
  return (SendRes backBox toBox $ getTxHash txResp)

toSendTxDelayed :: Wallet -> SendDelayed -> Tx
toSendTxDelayed wallet SendDelayed{..} = Tx
  { tx'inputs   = V.fromList [inputBox]
  , tx'outputs  = V.fromList $ catMaybes [senderUtxo, Just receiverUtxo]
  , tx'proof    = getOwnerProof wallet
  , tx'args     = M.empty
  }
  where
    inputBox = sendDelayed'from
    height = sendDelayed'height

    spendHeight = "spend-height"

    senderPk = pk (text $ wallet'publicKey wallet)

    senderUtxo
      | sendDelayed'remain > 0 = Just $ Box
                { box'id     = sendDelayed'back
                , box'value  = sendDelayed'remain
                , box'script = toScript backScript
                , box'args   = M.empty
                }
      | otherwise                 = Nothing

    -- sender can get all money back if height is less than equal to limit
    -- or just the rest of it if it's greater than the limit
    backScript = senderPk

    receiverUtxo = Box
      { box'id     = sendDelayed'to
      , box'value  = sendDelayed'amount
      , box'script = toScript $ receiverScript ||* refundScript
      , box'args   = M.fromList [(spendHeight, PrimInt height)]
      }

    -- receiver can get money only hieght is greater than specified limit
    receiverScript =
            pk (text $  wallet'publicKey sendDelayed'recepientWallet)
        &&* getBoxArg (getInput (int 0)) (text spendHeight) <* getHeight

    -- sender can get money back if hieght is less or equals to specified limit
    refundScript =
            senderPk
        &&* getBoxArg (getInput (int 0)) (text spendHeight) >=* getHeight

