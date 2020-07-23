-- | Alice pays to Bob but also reserves the ability to get the money back
-- until certain height in the blockchain.
--
-- This is example of timed transaction.
module Hschain.Utxo.Test.Client.Scripts.PayForCofee where

import Prelude hiding ((<*))
import Control.Monad.Except
import Control.Timeout

import Data.Boolean
import Data.Int
import Data.Maybe
import Data.String
import Data.Text (Text)

import Text.Show.Pretty

import Hschain.Utxo.Test.Client.Monad (App, logTest, printTest, testCase, testTitle, getTxSigma)
import Hschain.Utxo.Test.Client.Wallet
import Hschain.Utxo.Test.Client.Scripts.Utils

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

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
      (User bob    _bobBox1)    = scene'bob
      (User john   _johnBox1)   = scene'john
      (User master masterBox1) = scene'master
  mSendDelAlice <- debugSendDelayed True
        "Message alice sends to bob 2 coins delayed by 2 steps of blockchain"
        alice aliceBox1 bob 2 2
  forM_ (mSendDelAlice) $ \(SendRes _aliceBox2 aliceOrBobBox _) -> do
      SendResult _bobBox3 _johnBox2 _  <- debugSend False
            "Bob tries to send 2 coins to john (it should fail on delayed condition)"
            bob aliceOrBobBox john 2
      SendResult masterBox2 _johnBox3 _ <- debugSend True
            "Master sends to john 1 coin"
            master masterBox1 john 1
      SendResult _masterBox3 _johnBox4 _ <- debugSend True
            "Master sends to john 1 coin"
            master masterBox2 john 1
      SendResult _bobBox4 _johnBox6 _ <- debugSend True
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
      (User bob   _bobBox1)    = scene'bob
      (User john  _johnBox1)   = scene'john
      (User master masterBox1) = scene'master
  mDelSendAlice <- debugSendDelayed True
        "Message alice sends to bob 2 coins delayed by 2 steps of blockchain"
        alice aliceBox1 bob 2 2
  forM_ mDelSendAlice $ \(SendRes _aliceBox2 aliceOrBobBox _) -> do
    SendResult _bobBox3 _johnBox2 _ <- debugSend False
          "Bob tries to send 2 coins to john (it should fail on delayed condition)"
          bob aliceOrBobBox john 2
    SendResult _masterBox2 _johnBox3 _ <- debugSend True
          "Master sends to john 1 coin"
          master masterBox1 john 1
    SendResult _masterBox2 _johnBox4 _ <- debugSend True
          "Alice sends to john 2 coins. It should proceed."
          alice aliceOrBobBox john 2
    SendResult _bobBox4 _johnBox6 _ <- debugSend False
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
  , sendDelayed'height :: !Int64
  , sendDelayed'recepientWallet :: !Wallet -- we need it while sigma-proofs are not implemented yet
  }

data SendResultDelayed = SendRes
  { sendRes'from        :: BoxId
  , sendRes'refundOrTo  :: BoxId
  , sendRes'txHash      :: !(Maybe TxHash)
  } deriving (Show, Eq)

----------------------------------------
-- transactions

debugSendDelayed :: Bool -> Text -> Wallet -> BoxId -> Wallet -> Int64 -> Money -> App (Maybe SendResultDelayed)
debugSendDelayed isSuccess msg from fromBox to heightDiff amount = do
  logTest msg
  eRes <- sendTxDelayed from fromBox to heightDiff amount
  case eRes of
    Right res -> do
      printTest res
      st <- M.getState
      logTest $  renderText st
      wait
      testCase msg $ (isJust (sendRes'txHash res) == isSuccess)
      return (Just res)
    Left err -> do
      testCase msg (False == isSuccess)
      logTest err
      return Nothing
  where
    wait = sleep 0.25


sendTxDelayed :: Wallet -> BoxId -> Wallet -> Int64 -> Money -> App (Either Text SendResultDelayed)
sendTxDelayed from fromBox to delayDiff amount = do
  toBox     <- allocAddress to
  backBox   <- allocAddress from
  refundBox <- allocAddress from
  currentHeight <- M.getHeight
  totalAmount <- fmap (fromMaybe 0) $ M.getBoxBalance fromBox
  let sendTx = SendDelayed fromBox toBox backBox refundBox amount (totalAmount - amount) (currentHeight + delayDiff) to
      preTx = toSendTxDelayed from sendTx Nothing
      proofEnv = getProofEnv from
  eSigma <- getTxSigma preTx
  eProof <- fmap join $ mapM (liftIO . newProof proofEnv) eSigma
  case eProof of
    Right proof -> do
      let tx = toSendTxDelayed from sendTx (Just proof)
      logTest $ renderText tx
      txResp <- M.postTx tx
      logTest $ fromString $ ppShow txResp
      return $ Right $ SendRes backBox toBox $ getTxHash txResp
    Left err -> return $ Left err

toSendTxDelayed :: Wallet -> SendDelayed -> Maybe Proof -> Tx
toSendTxDelayed wallet SendDelayed{..} mProof = do
    Tx
      { tx'inputs   = V.fromList [inputBox]
      , tx'outputs  = V.fromList $ catMaybes [senderUtxo, Just receiverUtxo]
      , tx'proof    = mProof
      , tx'args     = mempty
      }
  where
    inputBox = sendDelayed'from
    height = sendDelayed'height

    spendHeightId = 0

    senderPk = pk' $ getWalletPublicKey wallet

    senderUtxo
      | sendDelayed'remain > 0 = Just $ Box
                { box'id     = sendDelayed'back
                , box'value  = sendDelayed'remain
                , box'script = mainScriptUnsafe backScript
                , box'args   = mempty
                }
      | otherwise                 = Nothing

    -- sender can get all money back if height is less than equal to limit
    -- or just the rest of it if it's greater than the limit
    backScript = senderPk

    receiverUtxo = Box
      { box'id     = sendDelayed'to
      , box'value  = sendDelayed'amount
      , box'script = mainScriptUnsafe $ receiverScript ||* refundScript
      , box'args   = intArgs [height]
      }

    getSpendHeight = vecAt (getBoxIntArgList (getInput (int 0))) (int spendHeightId)

    -- receiver can get money only hieght is greater than specified limit
    receiverScript =
            pk' (getWalletPublicKey sendDelayed'recepientWallet)
        &&* toSigma (getSpendHeight <* getHeight)

    -- sender can get money back if hieght is less or equals to specified limit
    refundScript =
            senderPk
        &&* toSigma (getSpendHeight >=* getHeight)

