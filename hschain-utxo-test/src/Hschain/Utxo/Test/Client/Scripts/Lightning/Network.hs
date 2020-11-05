{-# Language TupleSections #-}
-- | Toy network for lightning
module Hschain.Utxo.Test.Client.Scripts.Lightning.Network(
    Net(..)
  , newNetwork
  , closeNetwork
  , registerUser
  , openChan
  , waitForChanToOpen
  , closeChan
  , send
  , User(..)
  , Link(..)
  , Api(..)
  , initTestRoute
  , initRoute
  , initTimeLimits
) where

import Hex.Common.Delay

import Prelude hiding (read)

import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except

import Data.ByteString (ByteString)
import Data.Int
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Time

import HSChain.Crypto.Classes (ByteRepr(..))

import Hschain.Utxo.Lang.Utils.ByteString (getSha256)
import Hschain.Utxo.Test.Client.Wallet (Wallet, getWalletPublicKey, getWalletPrivateKey)
import Hschain.Utxo.Lang hiding (Env)
import Hschain.Utxo.Test.Client.Monad (App, randomBS, txIsValid, testCase, getBoxBalance, getHeight)
import Hschain.Utxo.Test.Client.Scripts.Lightning.Protocol
import Hschain.Utxo.Test.Client.Scripts.Lightning.Tx
import Hschain.Utxo.Test.Client.Scripts.Lightning.User
import Hschain.Utxo.Test.Client.Scripts.Utils(postTxSuccess)

import Text.Show.Pretty

import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Crypto.Signature as Crypto

-- Network state
data Env = Env
  { env'chans :: Map ChanId Chan
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
registerUser net uid wallet boxes = do
  liftIO $ insertUserToEnv net uid
  userChan <- liftIO $ insertUserMsgChan net uid
  let api = Api
              { api'send = sendDebug $ atomically . writeTChan (net'msgIn net)
              , api'read = readDebug $ atomically $ readTChan userChan
              }
  funds <- mapM (\b -> fmap ((b, ) . fromMaybe 0) $ getBoxBalance b) boxes
  newUser api uid wallet funds
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
  chanId <- initChanId
  env <- liftIO $ readTVarIO $ user'env alice
  (boxes, change, rest) <- getChanFunds $ userEnv'funds env
  let env' = (insertChan chanId (ChanInit (spec chanId) boxes change) env) { userEnv'funds = rest }
  liftIO $ do
    atomically $ writeTVar (user'env alice) env'
    api'send (user'api alice) Msg
      { msg'from = user'id alice
      , msg'to   = user'id bob
      , msg'act  = OpenChan (spec chanId) ( getWalletPublicKey $ userEnv'wallet env)
      }
  return chanId
  where
    getChanFunds funds = case post of
      []              -> throwError "Not enough funds to open channel"
      (b, total) : ps -> return (fst b : (fmap (fst . fst) pre), total - value, fmap fst ps)
      where
        (pre, post) = L.span (( < value) . snd) $ accumSums funds

    accumSums = snd . L.mapAccumR (\total (box, val) -> let total' = total + val in (total', ((box, val), total')) ) 0

    spec chId = ChanSpec
            { chanSpec'id = chId
            , chanSpec'capacity = value
            , chanSpec'delay    = 50
            , chanSpec'minDepth = 5 }

waitForChanToOpen :: MonadIO io => NominalDiffTime -> ChanId -> User -> User -> io Bool
waitForChanToOpen maxTime chanId a b = liftIO $ do
  (resA, resB) <- join $ waitBoth <$> (async $ checkIsOpen 0.2 a) <*> (async $ checkIsOpen 0.2 b)
  return $ resA && resB
  where
    checkIsOpen dt user = check 0
      where
        check time = do
          env <- readTVarIO $ user'env user
          case lookupChan chanId env of
            Just ChanActive{..} -> (putStrLn $ "time to open chan: " <> show time) >> return True
            _                   ->  if time > maxTime
                                      then return False
                                      else sleep dt >> check (time + dt)

unexpectedMsg :: MonadIO io => Act -> io a
unexpectedMsg act = liftIO $ do
  putStrLn "Unexpected message received:"
  pPrint act
  return $ error "Failed"


initChanId :: MonadIO io => io ChanId
initChanId = liftIO $ fmap ChanId $ randomBS 16

closeChan :: ChanId -> User -> User -> App ()
closeChan chanId alice bob =
  liftIO $ api'send (user'api alice) Msg
    { msg'from = user'id alice
    , msg'to   = user'id bob
    , msg'act  = ShutdownChan chanId
    }

type TestRoute = [Link]      -- ^ route augmented with info on participants and secrets

data Link = Link
  { link'from    :: (User, PayInfo)
  , link'to      :: (User, PayInfo)
  , link'hop     :: Hop
  }

data PayInfo = PayInfo
  { payInfo'hash    :: Maybe ByteString
  -- ^ some of the users know only payHash
  , payInfo'secret  :: Maybe ByteString
  -- ^ some users know both. Secret propagates along the route backwards during the process of exchange
  }

-- | We assume that route is already defined and it has at least two hops.
-- In real system sender creates the route from network information.
send :: TestRoute -> App ()
send = mapConcurrently_ hopProc
  where
    hopProc Link{..} = do
      return ()

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

initTestRoute :: Money -> [(ChanId, User, User)] -> App TestRoute
initTestRoute value chanUsers = do
  secret <- liftIO $ randomBS 16
  initRoute secret value chanUsers

initRoute :: ByteString -> Money -> [(ChanId, User, User)] -> App [Link]
initRoute secret value chanUsers =
  fmap (zipWith (\((ch, from, to), (fromSecret, toSecret)) (index, time) -> Link (from, fromSecret) (to, toSecret) (Hop ch (Htlc value 0 time payHash) index)) (zip chanUsers secrets)) $ initTimeLimits
  where
    payHash = getSha256 secret

    secrets = [(firstUser, middleUser)] ++ replicate (length chanUsers - 2) (middleUser, middleUser) ++ [(middleUser, lastUser)]

    firstUser  = PayInfo (Just payHash) Nothing
    middleUser = PayInfo Nothing Nothing
    lastUser   = PayInfo (Just payHash) (Just secret)

initTimeLimits :: App [(Int64, Int64)]
initTimeLimits = do
  curHeight <- getHeight
  return $ reverse $ fmap (\n -> (n - 1, n * 20 + curHeight)) [1, 2 ..]

