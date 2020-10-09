-- | Alice and Bob create bidirectional channel and make series of safe exchanges off-chain.
--
-- The script is realised as it's described in the book "Mastering Bitcoin"
-- see pp 292, section "Asymmetric Revocable Commitments".
--
--
module Hschain.Utxo.Test.Client.Scripts.Channel(
  channelExchange
) where

import Hex.Common.Text

import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Extra

import Data.ByteString (ByteString)
import Data.Either.Extra
import Data.List.Extra (firstJust)
import Data.Int
import Data.Map.Strict (Map)
import Data.Tuple (swap)

import HSChain.Crypto.Classes (encodeBase58)

import Hschain.Utxo.Test.Client.Wallet
import Hschain.Utxo.Test.Client.Chan (BlockChan)

import Hschain.Utxo.Test.Client.Monad hiding (getHeight)
import Hschain.Utxo.Test.Client.Scripts.MultiSig (getSharedBoxTx, postTxDebug, changeBox, simpleSpendTo, spendCommonBoxTx)
import Hschain.Utxo.Test.Client.Scripts.Utils
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.State.React (react)
import Hschain.Utxo.Lang.Core.Compile.Expr

import System.Random

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Test.Client.Chan as C

type RevokeSecret = ByteString

initSecret :: Int -> IO RevokeSecret
initSecret size = fmap B.pack $ mapM (const randomIO) [1 .. size]

-- | Info that should not be available but
-- present for testing purposes only.
newtype Hidden a = Hidden a


-- | Game is a serises of off-chain transactions between two players: alice and bob.
data Game = Game
  { game'alice :: Player
  , game'bob   :: Player
  }

readGameBalance :: Game -> App Balance
readGameBalance = fmap playerEnv'balance . readPlayerEnv . game'alice

stopGame :: Game -> App ()
stopGame Game{..} = do
  stopPlayer game'alice
  stopPlayer game'bob

-- | Player internal state
data PlayerEnv = PlayerEnv
  { playerEnv'balance       :: !Balance
  -- ^ current balance
  , playerEnv'prevSignedTx  :: !(Maybe Tx)
  -- ^ signed tx that corresponds to revokation key
  , playerEnv'pending       :: !OffChain
  -- ^ current unsigned off-chain transaction
  , playerEnv'deals         :: ![Tx]
  -- ^ previous signed transactions
  , playerEnv'wallet        :: !Wallet
  -- ^ players' wallet
  , playerEnv'partnerPubKey :: !PublicKey
  -- ^ partner's public key
  , playerEnv'commonBoxId   :: !BoxId
  -- ^ shared boxId with initial balance
  , playerEnv'commonScript  :: !(Sigma PublicKey)
  -- ^ common sigma expression that guards shared balance box
  , playerEnv'revokeProc    :: !RevokeProc
  }

readPlayerEnv :: Player -> App PlayerEnv
readPlayerEnv (Player p) = liftIO $ readTVarIO p

setPlayerBalance :: Player -> Balance -> App ()
setPlayerBalance (Player tv) balance = liftIO $ atomically $
  modifyTVar' tv $ \env -> env { playerEnv'balance = balance }

stopPlayer :: Player -> App ()
stopPlayer (Player tv) = do
  env <- liftIO $ readTVarIO tv
  stopRevokeProc $ playerEnv'revokeProc env

data RevokeProc = RevokeProc
  { revokeProc'boxes  :: !RevokeBoxes
  , revokeProc'chan   :: !BlockChan
  , revokeProc'proc   :: !(Async (StMAppM ()))
  }

newRevokeProc :: Wallet -> App RevokeProc
newRevokeProc wallet = do
  boxes <- liftIO newRevokeBoxes
  chan <- newBlockChan 0.25 Nothing
  proc <- async $ do
    mTx <- liftIO $ C.findTxM chan (isRevokeTx boxes) maxBound
    forM_ mTx $ \tx -> do
      whenJustM  (liftIO $ getRevokeBoxForTx boxes tx) $ \revokeBox -> do
        (revokeTx, _boxId) <- getRevokeTx wallet revokeBox
        void $ postTxDebug True "Revoke box post" revokeTx
  return $ RevokeProc
    { revokeProc'boxes = boxes
    , revokeProc'chan  = chan
    , revokeProc'proc  = proc
    }

stopRevokeProc :: RevokeProc -> App ()
stopRevokeProc RevokeProc{..} = do
  liftIO $ C.stopBlockChan revokeProc'chan
  cancel revokeProc'proc

newtype RevokeBoxes = RevokeBoxes (TVar (Map BoxId RevokeBox))

newRevokeBoxes :: IO RevokeBoxes
newRevokeBoxes = fmap RevokeBoxes $ newTVarIO mempty

insertRevokeBox :: RevokeBoxes -> BoxId -> RevokeBox -> STM ()
insertRevokeBox (RevokeBoxes tv) key val = modifyTVar' tv $ M.insert key val

isRevokeTx :: RevokeBoxes -> Tx -> IO Bool
isRevokeTx (RevokeBoxes tv) tx = fmap containsRevokeBox $ readTVarIO tv
  where
    containsRevokeBox m = any (\boxId -> M.member boxId m) boxIds

    boxIds = fmap box'id $ tx'outputs tx

getRevokeBoxForTx :: RevokeBoxes -> Tx -> IO (Maybe RevokeBox)
getRevokeBoxForTx (RevokeBoxes tv) tx = do
  m <- readTVarIO tv
  return $ firstJust (\boxId -> M.lookup boxId m) boxIds
  where
    boxIds = V.toList $ fmap box'id $ tx'outputs tx



-- | Data that we can use to punish other party for unfair behavior (postage of previous signed TXs).
-- We listen to blockchain for any revoke box id being posted. If it's posted it means
-- that our partner has spent shared balance in unfair manner. We can use our revoke key to punish partner
-- and get all the money on shared account.
data RevokeBox = RevokeBox
  { revokeBox'id     :: !BoxId
  , revokeBox'secret :: !ByteString
  , revokeBox'value  :: !Int64
  , revokeBox'tx     :: !(Hidden Tx) -- ^ TX that should trigger the reply with revoke TX
                                     -- in the script it's only for debug purposes
                                     -- one should not reveal this TX
  }

newtype Player = Player (TVar PlayerEnv)

-- | Move of value from balance of ome player to another player
-- Positive move is for benefit of first player (alice).
-- Negative move is for benefit of second player (bob).
type Move = Int64

type Balance = (Int64, Int64)

data OffChain = OffChain
  { offChain'tx        :: !PreTx
  , offChain'revokeKey :: !ByteString
  } deriving (Show, Eq)

-- | Alice and Bob create bidirectional channel and make series of safe exchanges off-chain.
--
-- * the common box that contains 10 is created (5 from Alice and 5 from Bob)
-- * Alice trasfers 1 to Bob and balance becomes 4 for Alice, 6 for Bob
--     * they create assymetric TX and exchange revokation keys
-- * Alice transfers 1 more to bob and balance becomes 3 for Alice, 7 for Bob
-- * They post final commitment tx and spend the total balance.
channelExchange :: App ()
channelExchange = do
  testTitle "Bidirectional channel based on revokation key."
  Scene{..} <- initUsers
  let alice     = user'wallet scene'alice
      bob       = user'wallet scene'bob
      john     = user'wallet scene'john
      Just aliceBox1 = user'box scene'alice
      Just bobBox1   = user'box scene'bob
  (tx, commonBoxId, commonScript) <- getSharedBoxTx alice bob (5, 5) (5, 5) aliceBox1 bobBox1
  void $ postTxDebug True "Alice and Bob post joint multisig TX" tx
  game <- newGame commonBoxId commonScript initBalance alice bob
  mapM_ (signDeal game) $ scanl (flip move) initBalance moves
  (finalTx, aliceBox2, bobBox2) <- getCooperativeTx game
  finalBalance@(aliceShareValue, bobShareValue) <- readGameBalance game
  void $ postTxDebug True "Alice and bob spend final multi-sig proof and spend common box with it" finalTx
  stopGame game
  let johnPubKey = getWalletPublicKey john
  simpleSpendTo "Alice is able to spends everything to John from her part of shared box"
      alice aliceBox2 johnPubKey aliceShareValue
  simpleSpendTo "Bob is able to spends everything to John from her part of shared box"
      bob bobBox2 johnPubKey bobShareValue
  testCase "Final balance is right" (finalBalance == expectedFinalBalance)
  where
    expectedFinalBalance = foldr move initBalance moves

    moves = [1,1,-2,-1]

initBalance :: Balance
initBalance = (5, 5)

postDelay :: Int
postDelay = 1000

move :: Move -> Balance -> Balance
move n (a, b) = (a + n, b - n)

signDeal :: Game -> Balance -> App ()
signDeal (Game alice bob) balance = do
  logTest $ "Balance on round: " <> showt balance
  -- Alice & Bob create tx to sign and prepare secret from previous step.
  (preTxAlice, prevSecretAlice) <- makeNewOffChainTx alice balance
  (preTxBob,   prevSecretBob)   <- makeNewOffChainTx bob   (swap balance)
  -- Alice asks Bob to sign
  txAlice <- signOffChainTx alice bob preTxAlice
  testCase "Alice off-chain TX is valid" =<< txIsValid txAlice
  -- Bob ask Alice to sign
  txBob <- signOffChainTx bob alice preTxBob
  testCase "Bob off-chain TX is valid" =<< txIsValid txBob
  -- parties save signed tx and revokation keys
  setPlayerBalance alice balance
  setPlayerBalance bob   (swap balance)
  saveTx alice txAlice
  saveTx bob   txBob
  saveRevokeSecret alice prevSecretBob
  saveRevokeSecret bob   prevSecretAlice
  saveSignedTx bob txAlice
  saveSignedTx alice txBob

saveSignedTx :: Player -> Tx -> App ()
saveSignedTx (Player tv) tx = liftIO $ atomically $ modifyTVar' tv $ \env ->
  env { playerEnv'prevSignedTx = Just tx }

signOffChainTx :: Player -> Player -> PreTx -> App Tx
signOffChainTx (Player me) (Player other) preTx = liftIO $ do
  myEnv <- readTVarIO me
  otherEnv <- readTVarIO other
  let myPk = getWalletPublicKey $ playerEnv'wallet myEnv
      otherPk = playerEnv'partnerPubKey myEnv
      knownKeys = [myPk, otherPk]
      myKeys = [myPk]
      otherKeys = [otherPk]
      commonScript = playerEnv'commonScript myEnv
      message = getSigMessagePreTx SigAll preTx
      myProofEnv = getProofEnv $ playerEnv'wallet myEnv
      otherProofEnv = getProofEnv $ playerEnv'wallet otherEnv
  proof <- fmap eitherToMaybe $ runProve $ do
    comQueryExpr <- initMultiSigProof knownKeys commonScript
    (myCommitments,    mySecret)    <- queryCommitments myKeys    comQueryExpr
    (otherCommitments, otherSecret) <- queryCommitments otherKeys comQueryExpr
    commitments <- appendCommitments [(myKeys, myCommitments), (otherKeys, otherCommitments)]
    challenges <- getChallenges commitments message
    myResponses    <- queryResponses myProofEnv    mySecret challenges
    otherResponses <- queryResponses otherProofEnv otherSecret   challenges
    proof <- appendResponsesToProof [(myKeys, myResponses), (otherKeys, otherResponses)]
    return proof
  return $ newTx $ appendProof proof preTx
  where
    appendProof proof tx@Tx{..} = tx { tx'inputs = fmap (\ref -> ref { boxInputRef'proof = proof }) tx'inputs }

makeNewOffChainTx :: Player -> Balance -> App (PreTx, RevokeSecret)
makeNewOffChainTx (Player tv) balance = liftIO $ do
  revokeSecret <- generateRevokeSecret
  atomically $ do
    env <- readTVar tv
    let myPk = getWalletPublicKey $ playerEnv'wallet env
        partnerPk = playerEnv'partnerPubKey env
        offChain = offChainPreTx revokeSecret (playerEnv'commonBoxId env) balance myPk partnerPk
        prevRevokeKey = offChain'revokeKey $ playerEnv'pending env
    writeTVar tv $ env
          { playerEnv'pending = offChain
          , playerEnv'balance = balance
          }
    return (offChain'tx offChain, prevRevokeKey)

saveTx :: Player -> Tx -> App ()
saveTx (Player p) signedTx = liftIO $ atomically $ modifyTVar' p $ \st ->
  st { playerEnv'deals =  signedTx : playerEnv'deals st }

saveRevokeSecret :: Player -> RevokeSecret -> App ()
saveRevokeSecret p secret = mapM_ (saveRevoke p) =<< extractRevokeBox p secret

saveRevoke :: Player -> RevokeBox -> App ()
saveRevoke p@(Player tv) revoke = do
  liftIO $ atomically $ do
    env <- readTVar tv
    insertRevokeBox (revokeProc'boxes $ playerEnv'revokeProc env) (revokeBox'id revoke) revoke
  testCase "Revoke key is valid" =<< flip revokeKeyIsValid revoke . playerEnv'wallet =<< readPlayerEnv p

extractRevokeBox :: Player -> RevokeSecret -> App (Maybe RevokeBox)
extractRevokeBox p secret = do
  env <- readPlayerEnv p
  return $ extract env
  where
    extract PlayerEnv{..} = fmap txAndSecretToRevokeBox $ playerEnv'prevSignedTx
      where
        txAndSecretToRevokeBox tx@Tx{..} = RevokeBox
          { revokeBox'id     = box'id box
          , revokeBox'value  = box'value box
          , revokeBox'secret = secret
          , revokeBox'tx     = Hidden tx
          }
          where
            box = tx'outputs V.! 0



newGame :: BoxId -> Sigma PublicKey -> Balance -> Wallet -> Wallet -> App Game
newGame commonBoxId commonScript balance alice bob = do
  alicePlayer <- newPlayer commonBoxId commonScript balance alice (getWalletPublicKey bob)
  bobPlayer   <- newPlayer commonBoxId commonScript balance bob   (getWalletPublicKey alice)
  return $ Game
    { game'alice = alicePlayer
    , game'bob   = bobPlayer
    }

newPlayer :: BoxId -> Sigma PublicKey -> Balance -> Wallet -> PublicKey -> App Player
newPlayer commonBoxId commonScript balance wallet partnerPubKey = do
  proc <- newRevokeProc wallet
  player <- liftIO $ do
    revokeSecret <- generateRevokeSecret
    let offChain = offChainPreTx revokeSecret commonBoxId balance (getWalletPublicKey wallet) partnerPubKey
    fmap Player $ newTVarIO $ PlayerEnv
      { playerEnv'balance       = balance
      , playerEnv'prevSignedTx  = Nothing
      , playerEnv'pending       = offChain
      , playerEnv'deals         = []
      , playerEnv'wallet        = wallet
      , playerEnv'partnerPubKey = partnerPubKey
      , playerEnv'commonBoxId   = commonBoxId
      , playerEnv'commonScript  = commonScript
      , playerEnv'revokeProc    = proc
      }
  return player


generateRevokeSecret :: IO ByteString
generateRevokeSecret = initSecret 128

offChainPreTx ::  ByteString -> BoxId -> Balance -> PublicKey -> PublicKey -> OffChain
offChainPreTx revokeSecret commonBoxId (myValue, partnerValue) myPk partnerPk = do
  OffChain
    { offChain'tx = preTx
    , offChain'revokeKey = revokeSecret
    }
  where
    preTx = Tx
      { tx'inputs  = [inputRef Nothing]
      , tx'outputs = [myBox, partnerBox]
      }

    inputRef proof = BoxInputRef
      { boxInputRef'id = commonBoxId
      , boxInputRef'args  = mempty
      , boxInputRef'proof = proof
      , boxInputRef'sigMask = SigAll
      }

    -- | Pays to me delayed by postDelay and partner with revoke key can claim it
    myBox = PreBox
      { preBox'value  = myValue
      , preBox'script = mainScriptUnsafe revokeScript
      , preBox'args   = byteArgs [revokeSecret, getSha256 revokeSecret]
      }

    revokeScript =
          (pk' myPk &&* (toSigma $ getHeight >* int postDelay))
      ||* (pk' partnerPk &&* (toSigma $ sha256 readKey ==* bytes revokeHash))
      where
        revokeHash = getSha256 revokeSecret

    readKey = listAt getBytesVars 0

    -- | Pays to partner right away
    partnerBox = PreBox
      { preBox'value  = partnerValue
      , preBox'script = mainScriptUnsafe $ pk' partnerPk
      , preBox'args   = mempty
      }

getRevokeTx :: Wallet -> RevokeBox -> App (Tx, BoxId)
getRevokeTx wallet RevokeBox{..} =
  fmap appendBoxId $ newProofTx (getProofEnv wallet) preTx
  where
    appendBoxId tx@Tx{..} = (tx, box'id $ tx'outputs V.! 0)

    preTx = Tx
      { tx'inputs  = [inputRef]
      , tx'outputs = [changeBox revokeBox'value pubKey ]
      }

    inputRef = BoxInputRef
      { boxInputRef'id = revokeBox'id
      , boxInputRef'args = byteArgs [revokeBox'secret]
      , boxInputRef'proof = Just $ singleOwnerSigmaExpr wallet
      , boxInputRef'sigMask = SigAll
      }

    pubKey = getWalletPublicKey wallet

getCooperativeTx :: Game -> App (Tx, BoxId, BoxId)
getCooperativeTx Game{..} = do
  alice <- readPlayerEnv game'alice
  bob   <- readPlayerEnv game'bob
  let balance = playerEnv'balance alice
  spendCommonBoxTx (playerEnv'wallet alice) (playerEnv'wallet bob) (playerEnv'commonBoxId alice) balance

-- | To check that revoke TX is valid we transition BCh to the next state
-- with malicious TX that corresponds to revokation key. Then we try to
-- post TX that revokes the funds and punishies the unfair behavior.
revokeKeyIsValid :: Wallet -> RevokeBox -> App Bool
revokeKeyIsValid wallet revoke = do
  logTest "REVOKE:"
  printTest (revokeBox'id revoke)
  printTest (revokeBox'value revoke)
  printTest (reveal $ revokeBox'tx revoke)
  printTest (coreProgFromScript $ box'script $ tx'outputs (reveal $ revokeBox'tx revoke) V.! 0)
  logTest (renderText $ tx'outputs (reveal $ revokeBox'tx revoke) V.! 0)

  logTest (encodeBase58 $ revokeBox'secret revoke)
  logTest (encodeBase58 $ getSha256 $ revokeBox'secret revoke)
  bch <- getState
  let eFraudSt = react (reveal $ revokeBox'tx revoke) bch
  case eFraudSt of
    Left _        -> return False
    Right fraudSt -> do
      revokeTx <- fmap fst $ getRevokeTx wallet revoke
      case react revokeTx fraudSt of
        Right _  -> return True
        Left err -> do
          logTest $ "REVOKE ERROR: " <> err
          return False
  where
    reveal (Hidden a) = a

