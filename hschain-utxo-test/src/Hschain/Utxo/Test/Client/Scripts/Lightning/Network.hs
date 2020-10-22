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

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import Data.Map.Strict (Map)

import HSChain.Crypto.Classes (ByteRepr(..))

import Hschain.Utxo.Lang.Utils.ByteString (getSha256)
import Hschain.Utxo.Test.Client.Wallet (Wallet, getWalletPublicKey, getWalletPrivateKey)
import Hschain.Utxo.Lang hiding (Env)
import Hschain.Utxo.Test.Client.Monad (randomBS)
import Hschain.Utxo.Test.Client.Scripts.Lightning.Protocol
import Hschain.Utxo.Test.Client.Scripts.Lightning.Tx

import Text.Show.Pretty

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M

import qualified Hschain.Utxo.Lang.Crypto.Signature as Crypto

-- Network state
data Env = Env
  { env'chans :: [Chan]
  , env'users :: [UserId]
  }
  deriving (Show, Eq)

emptyEnv :: Env
emptyEnv = Env [] []

-- | Lightning network environment
data Net = Net
  { net'env      :: TVar Env
  , net'msgIn    :: TChan Msg
  , net'msgOut   :: TVar (Map UserId (TChan Msg))
  , net'redirect :: Async ()
  }

newNetwork :: IO Net
newNetwork = do
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

closeNetwork :: Net -> IO ()
closeNetwork Net{..} = cancel net'redirect

registerUser :: Net -> UserId -> Wallet -> [BoxId] -> IO User
registerUser net uid wallet boxes = do
  insertUserToEnv net uid
  userChan <- insertUserMsgChan net uid
  return $ User
    { user'id  = uid
    , user'api = Api
        { api'send = sendDebug $ atomically . writeTChan (net'msgIn net)
        , api'read = readDebug $ atomically $ readTChan userChan
        }
    , user'wallet = wallet
    , user'boxes  = boxes
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

openChan :: User -> User -> Money -> IO ChanId
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

    aliceProc = do
      chId <- initChanId
      let chSpec = spec chId
      requestChan chSpec
      fundingCreated chSpec
      fundingLocked
      readFundingLocked =<< read
      return chId
      where
        write = writeNet alice bob
        read  = readNet alice bob

        requestChan chanSpec = write (OpenChan chanSpec alicePk)

        fundingCreated chSpec = do
          act <- read
          case act of
            AcceptChan{..} -> do
              fundTx <- fundingTx (user'wallet alice) (5, 5) (user'boxes alice) act'publicKey
              revokeSecret <- randomBS 16
              let revokeHash = getSha256 revokeSecret
                  comTx = commitmentTx act'publicKey (getSharedBoxId fundTx) (0, 5) alicePk (chanSpec'delay chSpec) revokeHash
              signature <- Crypto.sign (getWalletPrivateKey $ user'wallet alice) (getSigMessage SigAll comTx)
              write $ FundingCreated
                { act'chanId            = act'chanId
                , act'fundingTxId       = computeTxId fundTx
                , act'signCommitmentTx  = encodeToBS signature
                , act'revokeHash        = getSha256 revokeSecret
                }
            _ -> unexpectedMsg act

        fundingLocked = do
          act <- read
          case act of
            FundingSigned{..} ->
              write $ FundingLocked act'chanId
            _ -> unexpectedMsg act

    bobProc = do
      chSpec <- acceptChan
      fundingSigned chSpec
      readFundingLocked =<< read
      return ()
      where
        write = writeNet bob alice
        read  = readNet  bob alice

        acceptChan = do
          act <- read
          case act of
            OpenChan{..} -> do
              write $ AcceptChan (chanSpec'id act'spec) bobPk
              return act'spec
            _ -> unexpectedMsg act

        fundingSigned chSpec = do
          act <- read
          case act of
            FundingCreated{..} -> do
              revokeSecret <- randomBS 16
              let revokeHash = getSha256 revokeSecret
                  comTx = commitmentTx alicePk (computeBoxId act'fundingTxId 0) (5, 0) bobPk (chanSpec'delay chSpec) revokeHash
              signature <- Crypto.sign (getWalletPrivateKey $ user'wallet bob) (getSigMessage SigAll comTx)
              write $ FundingSigned act'chanId (encodeToBS signature) revokeHash
              -- we should listen to Blockchain to wait for minDepth confirmations
              -- but let's skip this for know and send it right away
              fundingLocked act'chanId
            _ -> unexpectedMsg act

        fundingLocked chanId = write $ FundingLocked chanId

    readFundingLocked act = case act of
      FundingLocked{..} -> return ()
      _                 -> unexpectedMsg act


unexpectedMsg :: Act -> IO a
unexpectedMsg act = do
  putStrLn "Unexpected message received:"
  pPrint act
  return $ error "Failed"


initChanId :: IO ChanId
initChanId = fmap ChanId $ randomBS 16

closeChan :: ChanId -> User -> User -> IO ()
closeChan chanId alice bob = do
  procA <- async aliceProc
  procB <- async bobProc
  void $ waitBoth procA procB
  where
    aliceProc = do
      shutdown
      readConfirm
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
          sign <- randomBS 16
          let fee = 1
          write $ ClosingSigned chanId sign fee

        readConfirm = do
          act <- read
          case act of
            ConfirmClosingSigned{..} -> return ()
            _ -> unexpectedMsg act

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
          sign <- randomBS 16
          let fee = 1
          write $ ConfirmClosingSigned chanId sign fee


send :: User -> User -> Money -> IO ()
send _ _ _ = return () -- undefined

writeNet :: User -> User -> Act -> IO ()
writeNet from to act = api'send (user'api from) Msg
  { msg'from = user'id from
  , msg'to   = user'id to
  , msg'act  = act
  }

readNet :: User -> User -> IO Act
readNet to from = do
  msg <- api'read (user'api to)
  if msg'from msg == user'id from
    then return $ msg'act msg
    else readNet to from

data User = User
  { user'id     :: UserId
  , user'api    :: Api
  , user'wallet :: Wallet
  , user'boxes  :: [BoxId]
  }

data Api = Api
  { api'send  :: Msg -> IO ()
  , api'read  :: IO Msg
  }

