module Hschain.Utxo.Test.Client.Scripts.AtomicSwap where

import Hex.Common.Delay
import Prelude hiding ((<*))

import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Vector     ((!))
import Data.Int
import Data.Maybe
import Data.Text (Text)
import Data.Time

import System.Random

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Utils.Hash(getSha256)

import Hschain.Utxo.Test.Client.Monad hiding (getHeight)
import Hschain.Utxo.Test.Client.Wallet (getWalletPublicKey, getProofEnv)
import Hschain.Utxo.Test.Client.Scripts.Utils

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

timeout :: NominalDiffTime -> Async a -> App ()
timeout dt proc = do
  sleep dt
  testCase "Test failed on timeout" False
  cancel proc

startProcWithTimeout :: App a -> App (Async (StMAppM a))
startProcWithTimeout act = do
  proc <- async act
  void $ async $ timeout maxTestTime proc
  return proc

-- | Run test no more than 60 seconds
maxTestTime :: NominalDiffTime
maxTestTime = 60

atomicSwap :: App ()
atomicSwap = do
  testTitle "Atomic swap: Alice exhanges with Bob."
  scene@Scene{..} <- initUsers
  dealSpec <- getDealSpec scene
  testCase "Spec is fair" $ isFairSpec dealSpec
  testCase "Both completed" =<< do
    aliceProc <- startProcWithTimeout $ startAliceProc dealSpec scene'alice
    bobProc   <- startProcWithTimeout $ startBobProc dealSpec scene'bob
    (aliceResult, bobResult) <- waitBoth aliceProc bobProc
    return $ aliceResult && bobResult

getDealSpec :: Scene -> App SwapSpec
getDealSpec scene = do
  (secret, hash) <- liftIO $ initSecret 24
  return $ SwapSpec
    { swapSpec'alice = SwapUser
        { swapUser'pk       = alicePk
        , swapUser'deadline = 30
        , swapUser'value    = 2
        }
    , swapSpec'bob = SwapUser
        { swapUser'pk       = bobPk
        , swapUser'deadline = 30
        , swapUser'value    = 1
        }
    , swapSpec'exchangeRate = 2
    , swapSpec'hash         = hash
    , swapSpec'secret       = secret
    }
  where
    alicePk = getPk scene'alice
    bobPk   = getPk scene'bob

    getPk extract = getWalletPublicKey $ user'wallet $ extract scene

isFairExchange :: SwapSpec -> Int -> Int -> Bool
isFairExchange SwapSpec{..} aliceValue bobValue =
  bobValue * swapSpec'exchangeRate == aliceValue

isFairSpec :: SwapSpec -> Bool
isFairSpec spec@SwapSpec{..} = isFairExchange spec aliceValue bobValue
  where
    aliceValue = swapUser'value swapSpec'alice
    bobValue   = swapUser'value swapSpec'bob


type SwapSecret = ByteString
type SwapHash   = ByteString

data SwapSpec = SwapSpec
  { swapSpec'alice        :: !SwapUser
  , swapSpec'bob          :: !SwapUser
  , swapSpec'exchangeRate :: !Int
  , swapSpec'secret       :: !SwapSecret
  , swapSpec'hash         :: !SwapHash
  }

data SwapUser = SwapUser
  { swapUser'pk        :: PublicKey
  , swapUser'deadline  :: Int
  , swapUser'value     :: Int
  }

initSecret :: Int -> IO (SwapSecret, SwapHash)
initSecret size = fmap (appendHash . B.pack) $ mapM (const randomIO) [1 .. size]
  where
    appendHash secret = (secret, getSha256 secret)


-----------------------------------------------------------
-- Alice scripts and transactions

aliceInitSwapScript :: SwapSpec -> Script
aliceInitSwapScript spec = [utxo|
    orSigma
      [ getHeight >* $(deadlineBob) &&* pk $(alicePubKey)
      , pk $(bobPubKey) &&* (sha256 getArgs ==* getBoxArgs getSelf)
      ]
|]
  where
    alicePubKey = swapUser'pk $ swapSpec'alice spec
    bobPubKey   = swapUser'pk $ swapSpec'bob spec
    deadlineBob = swapUser'deadline $ swapSpec'bob spec

-- | Alice initialises script which reveals the hash of the secret.
-- Bob can read the hash and create symmetrical TX wich reuqires Alice to reveal the secret.
aliceInitSwapTx :: ProofEnv -> BoxId -> SwapSpec -> App Tx
aliceInitSwapTx aliceKeys inputId spec = do
  Just totalValue <- getBoxBalance inputId
  newProofTx aliceKeys $ preTx totalValue
  where
    swapHash = swapSpec'hash spec

    preTx totalValue = Tx
      { tx'inputs     = [ singleOwnerInput inputId alicePubKey ]
      , tx'outputs    = [ swapBox, changeBox totalValue ]
      , tx'dataInputs = []
      }

    alicePubKey = swapUser'pk $ swapSpec'alice spec
    bobValue    = swapUser'value $ swapSpec'bob spec

    changeBox totalValue = getChangeBox (totalValue - fromIntegral bobValue) alicePubKey

    swapBox = Box
      { box'value  = fromIntegral bobValue
      , box'script = aliceInitSwapScript spec
      , box'args   = toArgs swapHash
      }

-- | Alice grabs the Bob's funds and reveals the secret.
-- She can not double spend her funds because of deadline of the script for her returns.
aliceGrabTx :: ProofEnv -> BoxId -> SwapSpec -> App Tx
aliceGrabTx aliceKeys inputId spec = newProofTx aliceKeys preTx
  where
    preTx = Tx
      { tx'inputs     = pure $ addSecret $ singleOwnerInput inputId alicePubKey
      , tx'outputs    = [ saveMoney ]
      , tx'dataInputs = []
      }

    saveMoney = getChangeBox (fromIntegral aliceValue) alicePubKey

    addSecret box = box { boxInputRef'args = toArgs aliceSecret }
    aliceSecret = swapSpec'secret spec
    aliceValue  = swapUser'value $ swapSpec'alice spec
    alicePubKey = swapUser'pk $ swapSpec'alice spec

-- | Attempt to doublespend (It should fail).
aliceDoubleSpendTx :: ProofEnv -> BoxId -> SwapSpec -> App Tx
aliceDoubleSpendTx aliceKeys inputId spec = newProofTx aliceKeys preTx
  where
    preTx = Tx
      { tx'inputs     = [ singleOwnerInput inputId alicePubKey ]
      , tx'outputs    = [ saveMoney ]
      , tx'dataInputs = []
      }

    saveMoney = getChangeBox (fromIntegral bobValue) alicePubKey

    alicePubKey = swapUser'pk $ swapSpec'alice spec
    bobValue    = swapUser'value $ swapSpec'bob spec

-----------------------------------------------------------
-- Bob scripts and transactions

bobInitSwapScript :: SwapHash -> SwapSpec -> Script
bobInitSwapScript swapHash spec = [utxo|
  orSigma
    [ getHeight >* $(deadlineAlice) &&* pk $(bobPubKey)
    , andSigma
        [ pk $(alicePubKey)
        , lengthBytes getArgs <* 33
        , sha256 getArgs ==* $(swapHash)
        ]
    ]
|]
  where
    deadlineAlice = swapUser'deadline $ swapSpec'alice spec
    alicePubKey   = swapUser'pk $ swapSpec'alice spec
    bobPubKey     = swapUser'pk $ swapSpec'bob spec


bobInitSwapTx :: ProofEnv -> SwapHash -> BoxId -> SwapSpec -> App Tx
bobInitSwapTx bobKeys swapHash inputId spec = do
  Just totalValue <- getBoxBalance inputId
  newProofTx bobKeys $ preTx totalValue
  where
    preTx totalValue = Tx
      { tx'inputs     = [ singleOwnerInput inputId bobPubKey ]
      , tx'outputs    = [ swapBox, changeBox totalValue ]
      , tx'dataInputs = []
      }

    bobPubKey  = swapUser'pk $ swapSpec'bob spec
    aliceValue = swapUser'value $ swapSpec'alice spec

    changeBox totalValue = getChangeBox (totalValue - fromIntegral aliceValue) bobPubKey

    swapBox = Box
      { box'value  = fromIntegral aliceValue
      , box'script = bobInitSwapScript swapHash spec
      , box'args   = mempty
      }

bobGrabTx :: ProofEnv -> SwapSecret -> BoxId -> SwapSpec -> App Tx
bobGrabTx bobKeys aliceSecret inputId spec = newProofTx bobKeys preTx
  where
    preTx = Tx
      { tx'inputs     = [addSecret $ singleOwnerInput inputId bobPubKey]
      , tx'outputs    = [ saveMoney ]
      , tx'dataInputs = []
      }

    saveMoney = getChangeBox (fromIntegral bobValue) bobPubKey

    addSecret box = box { boxInputRef'args = toArgs aliceSecret }

    bobValue  = swapUser'value $ swapSpec'bob spec
    bobPubKey = swapUser'pk $ swapSpec'bob spec

-------------------------------------------------------------
-- Alice process

startAliceProc :: SwapSpec -> User -> App Bool
startAliceProc spec user = do
  let Just aliceBox = user'box user
  tx <- aliceInitSwapTx (getProofEnv aliceWallet) aliceBox spec
  logTest "Alice Init Swap TX"
  logTest $ renderText tx
  postTxSuccess "Alice sends init swap tx" tx
  mAliceInputRef <- checkBobSwapTxIsFair
  case mAliceInputRef of
    Just aliceInputRef -> proceedWithAliceGrabTx aliceInputRef (getAliceSendId tx)
    Nothing -> do
      logTest noSwapForAliceMsg
      testCase noSwapForAliceMsg False
      return False
  where
    getAliceSendId tx = computeBoxId (computeTxId tx) 0

    noSwapForAliceMsg = "Bob has not posted swap for alice"

    aliceWallet = user'wallet user
    aliceKeys   = getProofEnv aliceWallet
    expectedValue = fromIntegral $ swapUser'value $ swapSpec'alice spec

    checkBobSwapTxIsFair = do
      mValueAndId <- aliceWaitForBobSwapValue spec
      logTest "Alice gets value:"
      printTest $ mValueAndId
      testCase "Alice gets expected value"  $ Just expectedValue == (fmap fst mValueAndId)
      testCase "Exchange is fair for Alice" $ maybe False ((\aliceValue -> isFairExchange spec aliceValue bobValue) . fromIntegral . fst) mValueAndId
      return $ fmap snd mValueAndId
      where
        bobValue = swapUser'value $ swapSpec'bob spec

    proceedWithAliceGrabTx aliceInputRef bobInputRef = do
      postTxSuccess "Alice sends grab Tx and reveals secret" =<< aliceGrabTx aliceKeys aliceInputRef spec
      aliceTriesToStealMoney bobInputRef
      return True

    aliceTriesToStealMoney bobInputRef =
      postTxFailure "Alice tries to steal (should fail on Bob deadline)" =<< aliceDoubleSpendTx aliceKeys bobInputRef spec

aliceWaitForBobSwapValue :: SwapSpec -> App (Maybe (Int64, BoxId))
aliceWaitForBobSwapValue spec = do
  bch <- newBlockChan 0.25 Nothing
  liftIO $ fmap (getSwapValue =<<) $ findTx bch isBobInitScript 20
  where
    getSwapValue tx@Tx{..} = do
      i <- V.findIndex isBobInitBox tx'outputs
      return ( box'value (tx'outputs ! i)
             , computeBoxId (computeTxId tx) (fromIntegral i)
             )

    isBobInitScript = V.any isBobInitBox . tx'outputs
    isBobInitBox = (bobInitScript == ) . box'script

    bobInitScript = bobInitSwapScript (swapSpec'hash spec) spec

------------------------------------------------
-- Bob process

startBobProc :: SwapSpec -> User -> App Bool
startBobProc spec user = do
  mSwapHashAndId <- readHash
  case mSwapHashAndId of
    Just swapHashAndId -> proceedWithHash swapHashAndId
    Nothing            -> logTest "Failed to get swap hash for Bob" >> return False
  where
    bobWallet = user'wallet user
    bobKeys = getProofEnv bobWallet

    expectedHash = swapSpec'hash spec

    readHash = do
      mSwapHashAndId <- bobWaitForHash spec
      logTest "Bob gets hash:"
      printTest $ Just expectedHash == (fmap fst mSwapHashAndId)
      testCase "Bob gets expected hash" $ Just expectedHash == (fmap fst mSwapHashAndId)
      return mSwapHashAndId

    proceedWithHash (swapHash, bobSpendBoxId) = do
      let Just bobBox = user'box user
      tx <- bobInitSwapTx bobKeys swapHash bobBox spec
      let aliceSpendBoxId = computeBoxId (computeTxId tx) 0
      postTxSuccess "Bob sends init swap tx" tx
      mAliceSecret <- bobWaitForSecret aliceSpendBoxId
      checkSecret mAliceSecret
      case mAliceSecret of
        Just aliceSecret -> proceedWithSecret aliceSecret bobSpendBoxId
        Nothing          -> testCase "Failed to get secret for Bob" False >> return False

    proceedWithSecret aliceSecret bobSpendBoxId = do
      postTxSuccess "Bob grabs his money" =<< bobGrabTx bobKeys aliceSecret bobSpendBoxId spec
      return True

    checkSecret mAliceSecret = do
      testCase "Bob gets correct secret" $ mAliceSecret == (Just $ swapSpec'secret spec)

bobWaitForHash ::  SwapSpec -> App (Maybe (SwapHash, BoxId))
bobWaitForHash spec = do
  bch <- newBlockChan 0.25 (Just 2)
  liftIO $ fmap (getTxSwapHash =<<) $ findTx bch isAliceInitScript 20
  where
    getTxSwapHash tx@Tx{..} = do
      i    <- V.findIndex isAliceInitBox tx'outputs
      let box = tx'outputs ! 0
      let hash = fromArgs $ box'args box
      return (hash, computeBoxId (computeTxId tx) (fromIntegral i))

    isAliceInitScript = V.any isAliceInitBox . tx'outputs
    isAliceInitBox = (aliceInitScript == ) . box'script

    aliceInitScript = aliceInitSwapScript spec

-- | Bob looks for box that spends his value to alice.
-- This box should contains Alices's secret.
bobWaitForSecret :: BoxId -> App (Maybe SwapSecret)
bobWaitForSecret aliceSpendBoxId = do
  bch <- newBlockChan 0.25 Nothing
  liftIO $ fmap (getSecret =<< ) $ findTx bch isAliceSecretTx 20
  where
    getSecret Tx{..} = do
      box <- V.find isAliceSecretBox tx'inputs
      let secret = fromArgs $ boxInputRef'args box
      return secret

    isAliceSecretTx Tx{..} = V.any isAliceSecretBox tx'inputs

    isAliceSecretBox BoxInputRef{..} = boxInputRef'id == aliceSpendBoxId

------------------------------------------------------------
-- test utils

listTxs :: App ()
listTxs = do
  chan <- fmap getBlockTChan $ newBlockChan 0.25 (Just 2)
  liftIO $ forever $ do
    tx <- atomically $ readTChan chan
    putStr "Got TX: "
    print tx

-- | On active verbose flag we get dump of the state.
-- Put it prior to every post of the transaction.
dumpState :: App ()
dumpState = do
  st <- getState
  logTest $ renderText st

checkTxResponce :: Bool -> Text -> PostTxResponse -> App ()
checkTxResponce isOk msg resp = do
  logTest msg
  printTest resp
  testCase msg $ (if isOk then isJust else isNothing) $ postTxResponse'value resp

------------------------------------------------------------
-- generic utils

singleOwnerScript :: PublicKey -> Script
singleOwnerScript pubKey = [utxo| pk $(pubKey) |]

getChangeBox :: Int64 -> PublicKey -> Box
getChangeBox value pubKey = Box
  { box'value  = value
  , box'script = singleOwnerScript pubKey
  , box'args   = mempty
  }

