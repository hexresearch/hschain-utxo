-- | Alice and Bob create bidirectional channel and make series of safe exchanges off-chain.
module Hschain.Utxo.Test.Client.Scripts.Channel(
  channelExchange
) where

import Control.Concurrent.STM

import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Either.Extra
import Data.Int
import Data.Map.Strict (Map)
import Data.Tuple (swap)

import Hschain.Utxo.Test.Client.Wallet

import Hschain.Utxo.Test.Client.Monad hiding (getHeight)
import Hschain.Utxo.Test.Client.Scripts.MultiSig (getSharedBoxTx, postTxDebug, changeBox, simpleSpendTo)
import Hschain.Utxo.Test.Client.Scripts.Utils
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Build


import System.Random

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

-- | Game is a serises of off-chain transactions
data Game = Game
  { game'alice :: Player
  , game'bob   :: Player
  }

-- | Player internal state
data PlayerEnv = PlayerEnv
  { playerEnv'balance       :: !Balance
  -- ^ current balance
  , playerEnv'prevRevokeKey :: !RevokeSecret
  -- ^ revoke secret from previous step
  -- we exchange it for signature tx of the next step
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
  , playerEnv'revokes       :: Map BoxId RevokeBox
  -- ^ watch list of revoke boxes
  }

-- | Data that we can use to punish other party for unfair behavior (postage of previous signed TXs).
-- We listen to blockchain for any revoke box id being posted. If it's posted it means
-- that our partner has spent shared balance in unfair manner. We can use our revoke key to punish partner
-- and get all the money on shared account.
data RevokeBox = RevokeBox
  { revokeBox'id     :: !BoxId
  , revokeBox'secret :: !ByteString
  , revokeBox'value  :: !Int64
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
  mapM_ (signDeal game) $ scanr move initBalance moves
  (finalTx, aliceBox2, bobBox2, finalBalance@(aliceShareValue, bobShareValue)) <- getCooperativeTx game
  void $ postTxDebug True "Alice and bob spend final multi-sig proof and spend common box with it" finalTx
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
  -- Alice & Bob create tx to sign and prepare secret from previous step.
  (preTxAlice, secretAlice) <- makeNewOffChainTx alice balance
  (preTxBob,   secretBob)   <- makeNewOffChainTx bob   (swap balance)
  -- Alice asks Bob to sign
  (txAlice, bobRevokeBoxId, bobRevokeBoxValue) <- signOffChainTx alice bob preTxAlice
  -- Bob ask Alice to sign
  (txBob,  aliceRevokeBoxId, aliceRevokeBoxValue) <- signOffChainTx bob alice preTxBob
  -- parties save signed tx and revokation keys
  saveTx alice txAlice
  saveTx bob   txBob
  saveRevoke alice (RevokeBox
                      { revokeBox'id     = aliceRevokeBoxId
                      , revokeBox'value  = aliceRevokeBoxValue
                      , revokeBox'secret = secretBob
                      })
  saveRevoke bob   (RevokeBox
                      { revokeBox'id     = bobRevokeBoxId
                      , revokeBox'value  = bobRevokeBoxValue
                      , revokeBox'secret = secretAlice
                      })

signOffChainTx :: Player -> Player -> PreTx -> App (Tx, BoxId, Int64)
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
  return $ appendRevokeBoxId $ newTx $ appendProof proof preTx
  where
    appendProof proof tx@Tx{..} = tx { tx'inputs = fmap (\ref -> ref { boxInputRef'proof = proof }) tx'inputs }

    appendRevokeBoxId tx@Tx{..} = (tx, box'id box, box'value box)
      where
        box = tx'outputs V.! 0

makeNewOffChainTx :: Player -> Balance -> App (PreTx, RevokeSecret)
makeNewOffChainTx (Player tv) balance = liftIO $ do
  revokeSecret <- initSecret 1000
  atomically $ do
    env <- readTVar tv
    let myPk = getWalletPublicKey $ playerEnv'wallet env
        partnerPk = playerEnv'partnerPubKey env
        offChain = offChainPreTx revokeSecret (playerEnv'commonBoxId env) balance myPk partnerPk
        prevRevokeKey = playerEnv'prevRevokeKey env
        nextRevokeKey = offChain'revokeKey $ playerEnv'pending env
    writeTVar tv $ env
          { playerEnv'prevRevokeKey = nextRevokeKey
          , playerEnv'pending       = offChain
          }
    return (offChain'tx offChain, prevRevokeKey)

saveTx :: Player -> Tx -> App ()
saveTx (Player p) signedTx = liftIO $ atomically $ modifyTVar' p $ \st ->
  st { playerEnv'deals =  signedTx : playerEnv'deals st }

saveRevoke :: Player -> RevokeBox -> App ()
saveRevoke (Player p) revoke = liftIO $ atomically $ modifyTVar' p $ \st ->
  st { playerEnv'revokes = M.singleton (revokeBox'id revoke) revoke <> playerEnv'revokes st }

newGame :: BoxId -> Sigma PublicKey -> Balance -> Wallet -> Wallet -> App Game
newGame commonBoxId commonScript balance alice bob = do
  alicePlayer <- newPlayer commonBoxId commonScript balance alice (getWalletPublicKey bob)
  bobPlayer   <- newPlayer commonBoxId commonScript balance bob   (getWalletPublicKey alice)
  return $ Game
    { game'alice = alicePlayer
    , game'bob   = bobPlayer
    }

newPlayer :: BoxId -> Sigma PublicKey -> Balance -> Wallet -> PublicKey -> App Player
newPlayer commonBoxId commonScript balance wallet partnerPubKey = liftIO $ do
  revokeSecret <- generateRevokeSecret
  let offChain = offChainPreTx revokeSecret commonBoxId balance (getWalletPublicKey wallet) partnerPubKey
  fmap Player $ newTVarIO $ PlayerEnv
    { playerEnv'balance       = balance
    , playerEnv'prevRevokeKey = offChain'revokeKey offChain
    , playerEnv'pending       = offChain
    , playerEnv'deals         = []
    , playerEnv'wallet        = wallet
    , playerEnv'partnerPubKey = partnerPubKey
    , playerEnv'commonBoxId   = commonBoxId
    , playerEnv'commonScript  = commonScript
    , playerEnv'revokes       = []
    }

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
      , preBox'args   = mempty
      }

    revokeScript =
          (pk' myPk &&* (toSigma $ getHeight >* int postDelay))
      ||* (pk' partnerPk &&* (toSigma $ sha256 readKey ==* bytes revokeHash))
      where
        revokeHash = getSha256 revokeSecret

    readKey = listAt (getBoxBytesArgList getSelf) 0

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

getCooperativeTx :: Game -> App (Tx, BoxId, BoxId, Balance)
getCooperativeTx = undefined

type RevokeSecret = ByteString

initSecret :: Int -> IO RevokeSecret
initSecret size = fmap B.pack $ mapM (const randomIO) [1 .. size]

offChainTxIsValid :: Tx -> App Bool
offChainTxIsValid = undefined

revokeKeyIsValid :: RevokeSecret -> App Bool
revokeKeyIsValid = undefined

