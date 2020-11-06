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
  , initTestRoute
  , TestRoute
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
import Data.Time
import Safe (headMay)

import Hschain.Utxo.Lang.Utils.ByteString (getSha256)
import Hschain.Utxo.Test.Client.Wallet (Wallet, getWalletPublicKey)
import Hschain.Utxo.Lang hiding (Env)
import Hschain.Utxo.Test.Client.Monad (App, randomBS, getBoxBalance, getHeight)
import Hschain.Utxo.Test.Client.Scripts.Lightning.Protocol
import Hschain.Utxo.Test.Client.Scripts.Lightning.User

import Text.Show.Pretty

import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import qualified Data.Map.Strict as M

-- Network state
data Env = Env
  { env'chans :: Map ChanId Chan
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
    checkIsOpen dt user = go 0
      where
        go time = do
          env <- readTVarIO $ user'env user
          case lookupChan chanId env of
            Just ChanActive{..} -> (putStrLn $ "time to open chan: " <> show time) >> return True
            _                   ->  if time > maxTime
                                      then return False
                                      else sleep dt >> go (time + dt)

initChanId :: MonadIO io => io ChanId
initChanId = liftIO $ fmap ChanId $ randomBS 16

closeChan :: ChanId -> User -> User -> App ()
closeChan chanId alice bob =
  liftIO $ api'send (user'api alice) Msg
    { msg'from = user'id alice
    , msg'to   = user'id bob
    , msg'act  = ShutdownChan chanId
    }

-- | Route augmented with info on participants and secrets
data TestRoute = TestRoute
  { testRoute'hops    :: Route
  , testRoute'secret  :: ByteString
  }

-- | We assume that route is already defined and it has at least two hops.
-- In real system sender creates the route from network information.
--
-- Alice sends money to bob over route. Bob creates secret and shares revoke hash with alice.
send :: User -> User -> TestRoute -> App ()
send alice bob TestRoute{..} = do
  saveSecret testRoute'secret bob
  forM_ (headMay testRoute'hops) $ \nextHop -> do
    nextUser <- getChanPartner (hop'chanId nextHop) =<< liftIO (readTVarIO $ user'env alice)
    htlcId <- allocHtlc nextHop alice
    liftIO $ api'send (user'api alice) Msg
      { msg'from = user'id alice
      , msg'to   = nextUser
      , msg'act  = UpdateAddHtlc (hop'chanId nextHop) htlcId testRoute'hops
      }

saveSecret :: ByteString -> User -> App ()
saveSecret secret user = modifyUserEnv user $ \env ->
    env { userEnv'secrets = M.insert (getSha256 secret) secret $ userEnv'secrets env }

initTestRoute :: Money -> [ChanId] -> App TestRoute
initTestRoute value chanUsers = do
  secret <- liftIO $ randomBS 16
  fmap (\route -> TestRoute route secret) $ initRoute secret value chanUsers

initRoute :: ByteString -> Money -> [ChanId] -> App Route
initRoute secret value chanUsers =
  fmap (zipWith (\ch (index, time) -> Hop ch (Htlc value 0 time payHash) index) chanUsers) $ initTimeLimits (length chanUsers)
  where
    payHash = getSha256 secret

initTimeLimits :: Int -> App [(Int64, Int64)]
initTimeLimits size = do
  curHeight <- getHeight
  return $ reverse $ fmap (\n -> (n - 1, n * 20 + curHeight)) $ take size [1, 2 ..]

modifyUserEnv :: User -> (UserEnv -> UserEnv) -> App ()
modifyUserEnv User{..} f = liftIO $ atomically $ modifyTVar' user'env f

