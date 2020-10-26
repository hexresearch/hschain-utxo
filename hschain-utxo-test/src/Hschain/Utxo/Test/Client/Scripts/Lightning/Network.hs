-- | Toy network for lightning
module Hschain.Utxo.Test.Client.Scripts.Lightning.Network(
    Net(..)
  , newNetwork
  , closeNetwork
  , registerUser
  , openChan
  , closeChan
  , send
  , User(..)
  , Api(..)
) where


import Prelude hiding (read)

import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Tuple (swap)

import HSChain.Crypto.Classes (ByteRepr(..))

import Hschain.Utxo.Lang.Utils.ByteString (getSha256)
import Hschain.Utxo.Test.Client.Wallet (Wallet, getWalletPublicKey, getWalletPrivateKey)
import Hschain.Utxo.Lang hiding (Env)
import Hschain.Utxo.Test.Client.Monad (App, randomBS, txIsValid, testCase, getBoxBalance)
import Hschain.Utxo.Test.Client.Scripts.Lightning.Protocol
import Hschain.Utxo.Test.Client.Scripts.Lightning.Tx
import Hschain.Utxo.Test.Client.Scripts.Utils(postTxSuccess)

import Text.Show.Pretty

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Crypto.Signature as Crypto

-- Network state
data Env = Env
  { env'chans :: [Chan]
  , env'users :: [UserId]
  }
  deriving (Show, Eq)

-- | Channel internal state
data ChanSt = ChanSt
  { chanSt'spec             :: ChanSpec
  , chanSt'balance          :: (Money, Money)
  , chanSt'partnerPublicKey :: PublicKey
  , chanSt'commonBoxId      :: BoxId
  }

emptyEnv :: Env
emptyEnv = Env [] []

-- | Lightning network environment
data Net = Net
  { net'env      :: TVar Env
  , net'msgIn    :: TChan Msg
  , net'msgOut   :: TVar (Map UserId (TChan Msg))
  , net'redirect :: Async ()
  }

newNetwork :: App Net
newNetwork = liftIO $ do
  env      <- newTVarIO emptyEnv
  inChan   <- newTChanIO
  outChans <- newTVarIO mempty
  redirect <- async $ startRedirectProc inChan outChans
  return $ Net
    { net'env = env
    , net'msgIn = inChan
    , net'msgOut = outChans
    , net'redirect = redirect
    }

startRedirectProc :: TChan Msg -> TVar (Map UserId (TChan Msg)) -> IO ()
startRedirectProc inChan outTv = do
  msg <- atomically $ readTChan inChan
  outChans <- readTVarIO outTv
  forM_ (M.lookup (msg'to msg) outChans) $ \userChan -> atomically $ writeTChan userChan msg
  startRedirectProc inChan outTv

closeNetwork :: Net -> App ()
closeNetwork Net{..} = cancel net'redirect

registerUser :: Net -> UserId -> Wallet -> [BoxId] -> App User
registerUser net uid wallet boxes = liftIO $ do
  insertUserToEnv net uid
  userChan <- insertUserMsgChan net uid
  tvChans <- newTVarIO mempty
  return $ User
    { user'id  = uid
    , user'api = Api
        { api'send = sendDebug $ atomically . writeTChan (net'msgIn net)
        , api'read = readDebug $ atomically $ readTChan userChan
        }
    , user'wallet   = wallet
    , user'boxes    = boxes
    , user'channels = tvChans
    }
  where
    sendDebug f msg = do
      C.putStrLn $ mconcat [from, " sends to ", to, ":"]
      pPrint $ msg'act msg
      f msg
      where
        UserId from = msg'from msg
        UserId to   = msg'to   msg

    readDebug f = do
      msg <- f
      let UserId from = msg'from msg
          UserId to   = msg'to   msg
      C.putStrLn $ mconcat [to, " receives from ", from, ":" ]
      pPrint $ msg'act msg
      return msg


insertUserToEnv :: Net -> UserId -> IO ()
insertUserToEnv Net{..} uid = atomically $ modifyTVar' net'env $ \st ->
  st { env'users = uid : env'users st }

insertUserMsgChan :: Net -> UserId -> IO (TChan Msg)
insertUserMsgChan Net{..} uid = do
  userChan <- newTChanIO
  atomically $ modifyTVar' net'msgOut $ M.insert uid userChan
  return userChan

openChan :: User -> User -> Money -> App ChanId
openChan alice bob value = do
  a <- async aliceProc
  b <- async bobProc
  (chId, _) <- waitBoth a b
  return chId
  where
    spec chId = ChanSpec
            { chanSpec'id = chId
            , chanSpec'capacity = value
            , chanSpec'delay    = 50
            , chanSpec'minDepth = 5 }

    alicePk = getWalletPublicKey $ user'wallet alice
    bobPk   = getWalletPublicKey $ user'wallet bob

    aliceProc :: App ChanId
    aliceProc = do
      chId <- initChanId
      let chSpec = spec chId
      requestChan chSpec
      (fundTx, _revokeSecret) <- fundingCreated chSpec
      fundingLocked fundTx
      readFundingLocked =<< read
      insertChanSt alice chId (ChanSt
        { chanSt'spec = chSpec
        , chanSt'balance = (value, 0)
        , chanSt'partnerPublicKey = bobPk
        , chanSt'commonBoxId = computeBoxId (computeTxId fundTx) 0
        })
      return chId
      where
        write :: MonadIO io => Act -> io ()
        write = writeNet alice bob

        read :: MonadIO io => io Act
        read = readNet alice bob

        requestChan chanSpec = liftIO $ write (OpenChan chanSpec alicePk)

        fundingCreated chSpec = do
          totalValue <- fmap (sum . catMaybes) $ mapM getBoxBalance $ user'boxes alice
          act <- read
          liftIO $ case act of
            AcceptChan{..} -> do
              fundTx <- fundingTx (user'wallet alice) (value, totalValue - value) (user'boxes alice) act'publicKey
              aliceRevokeSecret <- randomBS 16
              let bobRevokeHash   = act'revokeHash
                  comTx = commitmentTx act'publicKey (getSharedBoxId fundTx) (0, value) alicePk (chanSpec'delay chSpec) bobRevokeHash
              signature <- Crypto.sign (getWalletPrivateKey $ user'wallet alice) (getSigMessage SigAll comTx)
              write $ FundingCreated
                { act'chanId            = act'chanId
                , act'fundingTxId       = computeTxId fundTx
                , act'signCommitmentTx  = encodeToBS signature
                , act'revokeHash        = getSha256 aliceRevokeSecret
                }
              return (fundTx, aliceRevokeSecret)
            _ -> unexpectedMsg act

        fundingLocked :: Tx -> App ()
        fundingLocked fundTx = do
          act <- read
          case act of
            FundingSigned{..} -> do
              postTxSuccess (msgForUsers "Funding TX is posted" alice bob) fundTx
              write $ FundingLocked act'chanId
            _ -> unexpectedMsg act

    bobProc :: App ()
    bobProc = do
      chSpec <- acceptChan
      commonBoxId <- fundingSigned chSpec
      readFundingLocked =<< read
      saveChan chSpec commonBoxId
      where
        write :: MonadIO io => Act -> io ()
        write = writeNet bob alice

        read :: MonadIO io => io Act
        read  = readNet  bob alice

        saveChan chSpec commonBoxId =
          insertChanSt bob (chanSpec'id chSpec) ChanSt
            { chanSt'spec = chSpec
            , chanSt'balance = (0, value)
            , chanSt'partnerPublicKey = alicePk
            , chanSt'commonBoxId = commonBoxId
            }

        acceptChan = do
          act <- read
          case act of
            OpenChan{..} -> do
              revokeSecret <- liftIO $ randomBS 16
              let revokeHash = getSha256 revokeSecret
              write $ AcceptChan (chanSpec'id act'spec) bobPk revokeHash
              return act'spec
            _ -> unexpectedMsg act

        fundingSigned :: ChanSpec -> App BoxId
        fundingSigned chSpec = do
          act <- read
          case act of
            FundingCreated{..} -> do
              let aliceRevokeHash = act'revokeHash
                  commonBoxId = computeBoxId act'fundingTxId 0
                  comTx = commitmentTx alicePk commonBoxId (value, 0) bobPk (chanSpec'delay chSpec) aliceRevokeHash
              signature <- liftIO $ Crypto.sign (getWalletPrivateKey $ user'wallet bob) (getSigMessage SigAll comTx)
              write $ FundingSigned act'chanId (encodeToBS signature)
              -- we should listen to Blockchain to wait for minDepth confirmations
              -- but let's skip this for know and send it right away
              fundingLocked act'chanId
              return commonBoxId
            _ -> unexpectedMsg act

        fundingLocked chanId = write $ FundingLocked chanId

    readFundingLocked :: Act -> App ()
    readFundingLocked act = case act of
      FundingLocked{..} -> return ()
      _                 -> unexpectedMsg act


unexpectedMsg :: MonadIO io => Act -> io a
unexpectedMsg act = liftIO $ do
  putStrLn "Unexpected message received:"
  pPrint act
  return $ error "Failed"


initChanId :: MonadIO io => io ChanId
initChanId = liftIO $ fmap ChanId $ randomBS 16

closeChan :: ChanId -> User -> User -> App ()
closeChan chanId alice bob = do
  procA <- async aliceProc
  procB <- async bobProc
  void $ waitBoth procA procB
  where
    alicePk = getWalletPublicKey $ user'wallet alice
    bobPk = getWalletPublicKey $ user'wallet bob

    aliceProc = do
      (closeTx, signA) <- shutdown
      signB <- readConfirm
      postCloseChanTx closeTx signA signB
      where
        write = writeNet alice bob
        read  = readNet alice bob

        shutdown = do
          write $ ShutdownChan chanId
          act <- read
          case act of
            ShutdownChan{..} | act'chanId == chanId -> closingSigned
            _ -> unexpectedMsg act

        closingSigned = do
          st <- fmap fromJust $ getChanSt alice chanId
          let closeTx = closeChanTx (chanSt'commonBoxId st) (chanSt'balance st) (alicePk, chanSt'partnerPublicKey st)
          sign <- fmap encodeToBS $ liftIO $ Crypto.sign (getWalletPrivateKey $ user'wallet alice) (getSigMessage SigAll closeTx)
          let fee = 1
          write $ ClosingSigned chanId sign fee
          return (closeTx, sign)

        readConfirm = do
          act <- read
          case act of
            ConfirmClosingSigned{..} -> return act'sign
            _ -> unexpectedMsg act

    postCloseChanTx tx signA signB = do
      testCase "close chan TX is valid" =<< txIsValid signedTx
      postTxSuccess (msgForUsers "Post close TX" alice bob) signedTx
      where
        signedTx = tx { tx'inputs = fmap setSigns $ tx'inputs tx }
        setSigns x = x { boxInputRef'sigs = V.fromList $ catMaybes $ fmap decodeFromBS [signA, signB] }

    bobProc = do
      shutdown
      where
        write = writeNet bob alice
        read  = readNet  bob alice

        shutdown = do
          act <- read
          case act of
            ShutdownChan{..} | act'chanId == chanId -> do
              write $ ShutdownChan chanId
              closingSigned
            _ -> unexpectedMsg act

        closingSigned = do
          act <- read
          case act of
            ClosingSigned{..} -> confirmClosingSigned
            _ -> unexpectedMsg act

        confirmClosingSigned = do
          st <- fmap fromJust $ getChanSt bob chanId
          let closeTx = closeChanTx (chanSt'commonBoxId st) (swap $ chanSt'balance st) (chanSt'partnerPublicKey st, bobPk)
          sign <- liftIO $ Crypto.sign (getWalletPrivateKey $ user'wallet bob) (getSigMessage SigAll closeTx)
          let fee = 1
          write $ ConfirmClosingSigned chanId (encodeToBS sign) fee

msgForUsers :: Text -> User -> User -> Text
msgForUsers msg alice bob =
  mconcat [msg, " for ", userIdToText $ user'id alice, " and ", userIdToText $ user'id bob]

getChanSt :: User -> ChanId -> App (Maybe ChanSt)
getChanSt User{..} key = liftIO $ fmap (M.lookup key) $ readTVarIO user'channels

insertChanSt :: User -> ChanId -> ChanSt -> App ()
insertChanSt User{..} key val =
  liftIO $  atomically $ modifyTVar user'channels $ M.insert key val

send :: User -> User -> Money -> App ()
send _ _ _ = return () -- undefined

writeNet :: MonadIO io => User -> User -> Act -> io ()
writeNet from to act = liftIO $ api'send (user'api from) Msg
  { msg'from = user'id from
  , msg'to   = user'id to
  , msg'act  = act
  }

readNet :: MonadIO io => User -> User -> io Act
readNet to from = liftIO $ do
  msg <- api'read (user'api to)
  if msg'from msg == user'id from
    then return $ msg'act msg
    else readNet to from

data User = User
  { user'id       :: UserId
  , user'api      :: Api
  , user'wallet   :: Wallet
  , user'boxes    :: [BoxId]
  , user'channels :: TVar (Map ChanId ChanSt)
  }

data Api = Api
  { api'send  :: Msg -> IO ()
  , api'read  :: IO Msg
  }

