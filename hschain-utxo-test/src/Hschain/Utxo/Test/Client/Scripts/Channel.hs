-- | Alice and Bob create bidirectional channel and make series of safe exchanges off-chain.
--
-- The script is realised as it's described in the book "Mastering Bitcoin"
-- see pp 292, section "Asymmetric Revocable Commitments".
--
-- Is summary we use secret keys to revoce previous off-chain transaction.
-- Posting such TXs is disadvantageous to poster because other party can grab everything in this case.
-- See the book for details.
module Hschain.Utxo.Test.Client.Scripts.Channel(
    channelExchange
  , channelExchangeUnfair
) where

import Hex.Common.Text
import Hex.Common.Delay

import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Extra

import Data.ByteString (ByteString)
import Data.Either.Extra
import Data.List.Extra (firstJust)
import Data.Int
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Tuple (swap)

import Hschain.Utxo.Test.Client.Wallet
import Hschain.Utxo.Test.Client.Chan (BlockChan)

import Hschain.Utxo.Test.Client.Monad hiding (getHeight)
import Hschain.Utxo.Test.Client.Scripts.MultiSig (getSharedBoxTx, postTxDebug, changeBox, simpleSpendTo, spendCommonBoxTx)
import Hschain.Utxo.Test.Client.Scripts.Utils
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.State.React (react)

import System.Random

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Test.Client.Chan as C

type RevoceSecret = ByteString

initSecret :: Int -> IO RevoceSecret
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
  , playerEnv'revoceProc    :: !RevoceProc
  }

readPlayerEnv :: Player -> App PlayerEnv
readPlayerEnv (Player p) = liftIO $ readTVarIO p

setPlayerBalance :: Player -> Balance -> App ()
setPlayerBalance (Player tv) balance = liftIO $ atomically $
  modifyTVar' tv $ \env -> env { playerEnv'balance = balance }

stopPlayer :: Player -> App ()
stopPlayer (Player tv) = do
  env <- liftIO $ readTVarIO tv
  stopRevoceProc $ playerEnv'revoceProc env

data RevoceProc = RevoceProc
  { revoceProc'boxes  :: !RevoceBoxes           -- ^ Watch list of revocable boxes
  , revoceProc'chan   :: !BlockChan             -- ^ Open channel to listen for new TXs
  , revoceProc'proc   :: !(Async (StMAppM ()))  -- ^ Process that listen blockchain for unfair behavior
  , revoceProc'boxId  :: TVar (Maybe BoxId)     -- ^ where to catch fraud output
  }

newRevoceProc :: Wallet -> App RevoceProc
newRevoceProc wallet = do
  boxes <- liftIO newRevoceBoxes
  chan <- newBlockChan 0.25 Nothing
  revoceBoxIdRef <- liftIO $ newTVarIO Nothing
  proc <- async $ do
    mTx <- liftIO $ C.findTxM chan (isRevoceTx boxes) maxBound
    forM_ mTx $ \tx -> do
      whenJustM  (liftIO $ getRevoceBoxForTx boxes tx) $ \revoceBox -> do
        (revoceTx, boxId) <- getRevoceTx wallet revoceBox
        void $ postTxDebug True "Revoce box post" revoceTx
        liftIO $ atomically $ writeTVar revoceBoxIdRef $ Just boxId
  return $ RevoceProc
    { revoceProc'boxes = boxes
    , revoceProc'chan  = chan
    , revoceProc'proc  = proc
    , revoceProc'boxId = revoceBoxIdRef
    }

stopRevoceProc :: RevoceProc -> App ()
stopRevoceProc RevoceProc{..} = do
  liftIO $ C.stopBlockChan revoceProc'chan
  cancel revoceProc'proc

newtype RevoceBoxes = RevoceBoxes (TVar (Map BoxId RevoceBox))

newRevoceBoxes :: IO RevoceBoxes
newRevoceBoxes = fmap RevoceBoxes $ newTVarIO mempty

insertRevoceBox :: RevoceBoxes -> BoxId -> RevoceBox -> STM ()
insertRevoceBox (RevoceBoxes tv) key val = modifyTVar' tv $ M.insert key val

isRevoceTx :: RevoceBoxes -> Tx -> IO Bool
isRevoceTx (RevoceBoxes tv) tx = fmap containsRevoceBox $ readTVarIO tv
  where
    containsRevoceBox m = any (\boxId -> M.member boxId m) boxIds
    txId   = computeTxId tx
    boxIds = V.imap (\i _ -> computeBoxId txId (fromIntegral i)) $ tx'outputs tx

getRevoceBoxForTx :: RevoceBoxes -> Tx -> IO (Maybe RevoceBox)
getRevoceBoxForTx (RevoceBoxes tv) tx = do
  m <- readTVarIO tv
  return $ firstJust (\boxId -> M.lookup boxId m)
         $ V.toList
         $ V.imap (\i _ -> computeBoxId txId (fromIntegral i))
         $ tx'outputs tx
  where
    txId = computeTxId tx




-- | Data that we can use to punish other party for unfair behavior (postage of previous signed TXs).
-- We listen to blockchain for any revoce box id being posted. If it's posted it means
-- that our partner has spent shared balance in unfair manner. We can use our revoce key to punish partner
-- and get all the money on shared account.
data RevoceBox = RevoceBox
  { revoceBox'id     :: !BoxId
  , revoceBox'secret :: !ByteString
  , revoceBox'value  :: !Int64
  , revoceBox'tx     :: !(Hidden Tx) -- ^ TX that should trigger the reply with revoce TX
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
  { offChain'tx        :: !Tx
  , offChain'revoceKey :: !ByteString
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
  mapM_ (void . signDeal game) $ scanl (flip move) initBalance moves
  (finalTx, aliceBox2, bobBox2) <- getCooperativeTx game
  finalBalance@(aliceShareValue, bobShareValue) <- readGameBalance game
  void $ postTxDebug True "Alice and bob spend final multi-sig proof and spend common box with it" finalTx
  stopGame game
  let johnPubKey = getWalletPublicKey john
  simpleSpendTo True "Alice is able to spends everything to John from her part of shared box"
      alice aliceBox2 johnPubKey aliceShareValue
  simpleSpendTo True "Bob is able to spends everything to John from her part of shared box"
      bob bobBox2 johnPubKey bobShareValue
  testCase "Final balance is right" (finalBalance == expectedFinalBalance)
  where
    expectedFinalBalance = foldr move initBalance moves

    moves = [1,1,-2,-1]

-- | Alice tries to cheat but Bob takes everything with revoce key.
channelExchangeUnfair :: App ()
channelExchangeUnfair = do
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
  (txAlice, _) <- signDeal game (6, 4)
  _ <- signDeal game (7, 3)
  void $ postTxDebug True "Alice posts unfair TX" txAlice
  sleep 1
  let txAliceId = computeTxId txAlice
      boxId1 = computeBoxId txAliceId 0
      boxId2 = computeBoxId txAliceId 1
      johnPubKey = getWalletPublicKey john
  logTest $ renderText txAlice
  simpleSpendTo False "alice can not spend output 1"
      alice boxId1 johnPubKey 6
  simpleSpendTo False "alice can not spend output 2"
      alice boxId2 johnPubKey 4
  simpleSpendTo False "Bob has already spent this output"
     bob boxId1 johnPubKey 6
  simpleSpendTo True "Bob can spend output 2"
    bob boxId2 johnPubKey 4
  testCase "Bob has caught frad output" =<< fraudCatchDetected (game'bob game)

fraudCatchDetected :: Player -> App Bool
fraudCatchDetected player = do
  boxIdRef <- fmap (revoceProc'boxId . playerEnv'revoceProc) $ readPlayerEnv player
  fmap isJust $ liftIO $ readTVarIO boxIdRef

initBalance :: Balance
initBalance = (5, 5)

postDelay :: Int
postDelay = 1000

move :: Move -> Balance -> Balance
move n (a, b) = (a + n, b - n)

signDeal :: Game -> Balance -> App (Tx, Tx)
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
  saveRevoceSecret alice prevSecretBob
  saveRevoceSecret bob   prevSecretAlice
  saveSignedTx bob txAlice
  saveSignedTx alice txBob
  return (txAlice, txBob)

saveSignedTx :: Player -> Tx -> App ()
saveSignedTx (Player tv) tx = liftIO $ atomically $ modifyTVar' tv $ \env ->
  env { playerEnv'prevSignedTx = Just tx }

signOffChainTx :: Player -> Player -> Tx -> App Tx
signOffChainTx (Player me) (Player other) preTx = liftIO $ do
  myEnv <- readTVarIO me
  otherEnv <- readTVarIO other
  let myPk = getWalletPublicKey $ playerEnv'wallet myEnv
      otherPk = playerEnv'partnerPubKey myEnv
      knownKeys = [myPk, otherPk]
      myKeys = [myPk]
      otherKeys = [otherPk]
      commonScript = playerEnv'commonScript myEnv
      message = getSigMessageTx SigAll preTx
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
  return $ appendProof proof preTx
  where
    appendProof proof tx@Tx{..} = tx { tx'inputs = fmap (\ref -> ref { boxInputRef'proof = proof }) tx'inputs }

makeNewOffChainTx :: Player -> Balance -> App (Tx, RevoceSecret)
makeNewOffChainTx (Player tv) balance = liftIO $ do
  revoceSecret <- generateRevoceSecret
  atomically $ do
    env <- readTVar tv
    let myPk = getWalletPublicKey $ playerEnv'wallet env
        partnerPk = playerEnv'partnerPubKey env
        offChain = offChainPreTx revoceSecret (playerEnv'commonBoxId env) balance myPk partnerPk
        prevRevoceKey = offChain'revoceKey $ playerEnv'pending env
    writeTVar tv $ env
          { playerEnv'pending = offChain
          , playerEnv'balance = balance
          }
    return (offChain'tx offChain, prevRevoceKey)

saveTx :: Player -> Tx -> App ()
saveTx (Player p) signedTx = liftIO $ atomically $ modifyTVar' p $ \st ->
  st { playerEnv'deals =  signedTx : playerEnv'deals st }

saveRevoceSecret :: Player -> RevoceSecret -> App ()
saveRevoceSecret p secret = mapM_ (saveRevoce p) =<< extractRevoceBox p secret

saveRevoce :: Player -> RevoceBox -> App ()
saveRevoce p@(Player tv) revoce = do
  liftIO $ atomically $ do
    env <- readTVar tv
    insertRevoceBox (revoceProc'boxes $ playerEnv'revoceProc env) (revoceBox'id revoce) revoce
  testCase "Revoce key is valid" =<< flip revoceKeyIsValid revoce . playerEnv'wallet =<< readPlayerEnv p

extractRevoceBox :: Player -> RevoceSecret -> App (Maybe RevoceBox)
extractRevoceBox p secret = do
  env <- readPlayerEnv p
  return $ extract env
  where
    extract PlayerEnv{..} = fmap txAndSecretToRevoceBox $ playerEnv'prevSignedTx
      where
        txAndSecretToRevoceBox tx@Tx{..} = RevoceBox
          { revoceBox'id     = computeBoxId (computeTxId tx) 0
          , revoceBox'value  = box'value box
          , revoceBox'secret = secret
          , revoceBox'tx     = Hidden tx
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
  proc <- newRevoceProc wallet
  player <- liftIO $ do
    revoceSecret <- generateRevoceSecret
    let offChain = offChainPreTx revoceSecret commonBoxId balance (getWalletPublicKey wallet) partnerPubKey
    fmap Player $ newTVarIO $ PlayerEnv
      { playerEnv'balance       = balance
      , playerEnv'prevSignedTx  = Nothing
      , playerEnv'pending       = offChain
      , playerEnv'deals         = []
      , playerEnv'wallet        = wallet
      , playerEnv'partnerPubKey = partnerPubKey
      , playerEnv'commonBoxId   = commonBoxId
      , playerEnv'commonScript  = commonScript
      , playerEnv'revoceProc    = proc
      }
  return player


generateRevoceSecret :: IO ByteString
generateRevoceSecret = initSecret 128

offChainPreTx ::  ByteString -> BoxId -> Balance -> PublicKey -> PublicKey -> OffChain
offChainPreTx revoceSecret commonBoxId (myValue, partnerValue) myPk partnerPk = do
  OffChain
    { offChain'tx = preTx
    , offChain'revoceKey = revoceSecret
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

    -- | Pays to me delayed by postDelay and partner with revoce key can claim it
    myBox = Box
      { box'value  = myValue
      , box'script = mainScriptUnsafe revoceScript
      , box'args   = mempty
      }

    revoceScript =
          (pk' myPk &&* (toSigma $ getHeight >* getBoxPostHeight getSelf + int postDelay))
      ||* (pk' partnerPk &&* (toSigma $ sha256 readKey ==* bytes revoceHash))
      where
        revoceHash = getSha256 revoceSecret

    readKey = listAt getBytesVars 0

    -- | Pays to partner right away
    partnerBox = Box
      { box'value  = partnerValue
      , box'script = mainScriptUnsafe $ pk' partnerPk
      , box'args   = mempty
      }

getRevoceTx :: Wallet -> RevoceBox -> App (Tx, BoxId)
getRevoceTx wallet RevoceBox{..} =
  fmap appendBoxId $ newProofTx (getProofEnv wallet) preTx
  where
    appendBoxId tx@Tx{..} = (tx, computeBoxId (computeTxId tx) 0)

    preTx = Tx
      { tx'inputs  = [inputRef]
      , tx'outputs = [changeBox revoceBox'value pubKey ]
      }

    inputRef = BoxInputRef
      { boxInputRef'id = revoceBox'id
      , boxInputRef'args = byteArgs [revoceBox'secret]
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

-- | To check that revoce TX is valid we transition BCh to the next state
-- with malicious TX that corresponds to revokation key. Then we try to
-- post TX that revoces the funds and punishies the unfair behavior.
revoceKeyIsValid :: Wallet -> RevoceBox -> App Bool
revoceKeyIsValid wallet revoce = do
  bch <- getState
  let eFraudSt = react (reveal $ revoceBox'tx revoce) bch
  case eFraudSt of
    Left _        -> return False
    Right fraudSt -> do
      revoceTx <- fmap fst $ getRevoceTx wallet revoce
      case react revoceTx fraudSt of
        Right _  -> return True
        Left err -> do
          logTest $ "revoce ERROR: " <> err
          return False
  where
    reveal (Hidden a) = a

