module Hschain.Utxo.Test.Client.Monad(
    App(..)
  , StMAppM(..)
  , TestEnv(..)
  , TestSpec(..)
  , runApp
  , call
  , postTx
  , getTxSigma
  , getHeight
  , getBoxBalance
  , getState
  , getBoxChainEnv
  , getMasterSecret
  , getMasterBoxId
  , forceLogTest
  , logTest
  , printTest
  , testTitle
  , testCase
  , runTest
  , toHspec
  , initGenesis
  , newBlockChan
  , getBlockTChan
  , findTx
  , txIsValid
  , randomBS
) where

import Hex.Common.Text

import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Data.ByteString (ByteString)
import Data.Int
import Data.Time
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)

import Test.Hspec

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types
import Hschain.Utxo.State.React (react)
import Hschain.Utxo.Back.Config
import Hschain.Utxo.Test.Client.Chan (BlockChan, getBlockTChan, findTx)

import System.Random

import qualified Hschain.Utxo.API.Client as C

import qualified Hschain.Utxo.Test.Client.Chan as C

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString as B


data Test = Test
  { test'name  :: !Text
  , test'cases :: !(Seq TestCase)
  } deriving (Show, Eq)

data TestCase = TestCase
  { testCase'name   :: !Text
  , testCase'value  :: !Bool
  } deriving (Show, Eq)

newtype App a = App { unApp :: ReaderT TestEnv (ExceptT Text IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail,
            MonadReader TestEnv, MonadError Text, MonadBase IO)

newtype StMAppM a = StMAppM { unStMAppM :: StM (ReaderT TestEnv (ExceptT Text IO)) a }

instance MonadBaseControl IO App where
    type StM App a = StMAppM a
    liftBaseWith f = App $ liftBaseWith $ \q -> f (fmap StMAppM . q . unApp)
    restoreM = App . restoreM . unStMAppM


data TestEnv = TestEnv
  { testEnv'client         :: !C.ClientSpec
  , testEnv'verbose        :: !Bool
  , testEnv'log            :: TVar (Seq Text)
  , testEnv'test           :: TVar Test
  , testEnv'masterSecret   :: !Secret
  , testEnv'masterBoxId    :: !BoxId
  }

data TestSpec = TestSpec
  { testSpec'client  :: !C.ClientSpec
  , testSpec'verbose :: !Bool
  }

getMasterSecret :: App Secret
getMasterSecret = asks testEnv'masterSecret

getMasterBoxId :: App BoxId
getMasterBoxId = asks testEnv'masterBoxId

newBlockChan :: NominalDiffTime -> Maybe Int -> App BlockChan
newBlockChan dtime mHeight = do
  clientSpec <- asks testEnv'client
  liftIO $ C.newBlockChan clientSpec dtime mHeight

runTest :: TestSpec -> Secret -> BoxId -> App () -> IO Test
runTest TestSpec{..} masterSecret masterBoxId app = do
  testTv <- newTVarIO emptyTest
  logTv <- newTVarIO mempty
  res <- runApp (env testTv logTv masterSecret) app
  test <- readTVarIO testTv
  return $ case res of
    Left  err -> test { test'cases =  mappend (test'cases test) (pure (TestCase err False)) }
    Right _   -> test
  where
    env tv logTv masterPrivateKey = TestEnv
        { testEnv'client = testSpec'client
        , testEnv'verbose = testSpec'verbose
        , testEnv'log = logTv
        , testEnv'test = tv
        , testEnv'masterSecret = masterPrivateKey
        , testEnv'masterBoxId  = masterBoxId
        }

    emptyTest = Test "" mempty

testTitle :: Text -> App ()
testTitle name = do
  tv <- asks testEnv'test
  liftIO $ atomically $ modifyTVar' tv $ \st -> st { test'name = name }

testCase :: Text -> Bool -> App ()
testCase name val = do
  tv <- asks testEnv'test
  liftIO $ atomically $ modifyTVar' tv $ \st ->
      st { test'cases = mappend (test'cases st)  (pure $ TestCase name val) }

logTest :: Text -> App ()
logTest msg = do
  logTv <- asks testEnv'log
  isVerbose <- asks testEnv'verbose
  liftIO $ when isVerbose $ T.putStrLn msg
  liftIO $ atomically $ modifyTVar' logTv $ \xs -> mappend xs (pure msg)

forceLogTest :: Text -> App ()
forceLogTest msg = do
  logTv <- asks testEnv'log
  liftIO $ T.putStrLn msg
  liftIO $ atomically $ modifyTVar' logTv $ \xs -> mappend xs (pure msg)

printTest :: Show a => a -> App ()
printTest = logTest . showt

runApp :: TestEnv -> App a -> IO (Either Text a)
runApp env (App a) = runExceptT $ runReaderT a env

call :: C.ClientM a -> App a
call act = join $ fmap liftEither $ (\env -> C.call (testEnv'client env) act) =<< ask

postTx :: Tx -> App PostTxResponse
postTx = call . C.postTx

getHeight :: App Int64
getHeight = call C.getHeight

getBoxBalance :: BoxId -> App (Maybe Money)
getBoxBalance = call . C.getBoxBalance

getState :: App BoxChain
getState = call C.getState

getBoxChainEnv :: App Env
getBoxChainEnv = fmap unGetEnvResponse $ call C.getEnv

getTxSigma :: Tx -> App (Either Text (Vector (Sigma ProofInput)))
getTxSigma tx = do
  resp <- call $ C.getTxSigma tx
  logTest $ T.unlines ["PRE TX SIGMA:", showt resp]
  return $ mapM extractSigma =<< sigmaTxResponse'value resp
  where
    extractSigma val = case val of
      SigmaResult sigma -> Right sigma
      ConstBool b       -> Left $ mconcat ["Not a sigma-expression from result, got ", showt b]

-------------------------
-- test to hspec
--

toHspec :: Test -> Spec
toHspec Test{..} =
  describe (T.unpack test'name) $ mapM_ fromCase test'cases
  where
    fromCase TestCase{..} =
      it (T.unpack testCase'name) $ testCase'value `shouldBe` True

-- | returns genesis and the identifier of the master root box.
initGenesis :: Secret -> (Genesis, BoxId)
initGenesis secret = ([tx], masterBoxId)
  where
    masterBoxId = computeBoxId txId 0
    txId        = computeTxId tx
    tx = Tx
      { tx'inputs     = []
      , tx'outputs    = [box]
      , tx'dataInputs = []
      }

    publicKey = toPublicKey secret

    box = Box
      { box'value  = initMoney
      , box'script = [utxo| pk $(publicKey) |]
      , box'args   = mempty
      }

    initMoney = 1000000

-- | Checks that TX is valid on current blockchain state without commiting it.
txIsValid :: Tx -> App Bool
txIsValid tx = (either onErr (const $ pure True) . react tx) =<< getState
  where
    onErr txt = do
      logTest $ mappend "TX is invalid: " txt
      return False


randomBS :: Int -> IO ByteString
randomBS size = fmap B.pack $ mapM (const randomIO) [1 .. size]
