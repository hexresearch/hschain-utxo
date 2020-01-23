module Hschain.Utxo.Test.Client.Monad(
    App(..)
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
  , forceLogTest
  , logTest
  , printTest
  , testTitle
  , testCase
  , runTest
  , toHspec
  , initMasterBox
  , initGenesis
) where

import Hex.Common.Text

import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Fix
import Data.Sequence (Seq)
import Data.Text (Text)

import Test.Hspec

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build (pk')
import Hschain.Utxo.State.Types
import Hschain.Utxo.Back.Config

import qualified Hschain.Utxo.API.Client as C

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Test = Test
  { test'name  :: !Text
  , test'cases :: !(Seq TestCase)
  } deriving (Show, Eq)

data TestCase = TestCase
  { testCase'name   :: !Text
  , testCase'value  :: !Bool
  } deriving (Show, Eq)

newtype App a = App { unApp :: ReaderT TestEnv (ExceptT Text IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv, MonadError Text)

data TestEnv = TestEnv
  { testEnv'client         :: !C.ClientSpec
  , testEnv'verbose        :: !Bool
  , testEnv'log            :: TVar (Seq Text)
  , testEnv'test           :: TVar Test
  , testEnv'masterSecret   :: !Secret
  }

data TestSpec = TestSpec
  { testSpec'client  :: !C.ClientSpec
  , testSpec'verbose :: !Bool
  }

getMasterSecret :: App Secret
getMasterSecret = asks testEnv'masterSecret

runTest :: TestSpec -> App () -> IO Test
runTest TestSpec{..} app = do
  testTv <- newTVarIO emptyTest
  logTv <- newTVarIO mempty
  masterSecret <- newSecret
  res <- runApp (env testTv logTv masterSecret) app
  test <- readTVarIO testTv
  return $ case res of
    Left  err -> test { test'cases =  mappend (test'cases test) (pure (TestCase err False)) }
    Right _   -> test
  where
    env tv logTv masterSecret = TestEnv
        { testEnv'client = testSpec'client
        , testEnv'verbose = testSpec'verbose
        , testEnv'log = logTv
        , testEnv'test = tv
        , testEnv'masterSecret = masterSecret
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

getHeight :: App Integer
getHeight = call C.getHeight

getBoxBalance :: BoxId -> App (Maybe Money)
getBoxBalance = call . C.getBoxBalance

getState :: App BoxChain
getState = call C.getState

getBoxChainEnv :: App Env
getBoxChainEnv = fmap unGetEnvResponse $ call C.getEnv

getTxSigma :: Tx -> App (Either Text (Sigma PublicKey))
getTxSigma tx = do
  resp <- call $ C.getTxSigma tx
  case sigmaTxResponse'value resp of
    Right boolRes -> return $ case boolRes of
      SigmaBool sigma -> Right sigma
      ConstBool b     -> Left $ mconcat ["Not a sigma-expression from result, got ", showt b]
    Left err -> return $ Left err

-------------------------
-- test to hspec
--

toHspec :: Test -> Spec
toHspec Test{..} =
  describe (T.unpack test'name) $ mapM_ fromCase test'cases
  where
    fromCase TestCase{..} =
      it (T.unpack testCase'name) $ testCase'value `shouldBe` True


initGenesis :: App Genesis
initGenesis = fmap withSecret $ getMasterSecret
  where
    withSecret secret = [tx]
      where
        publicKey = getPublicKey secret
        env = proofEnvFromKeys [getKeyPair secret]

        box = Box
          { box'id     = initMasterBox
          , box'value  = initMoney
          , box'script = toScript $ pk' publicKey
          , box'args   = M.empty
          }

        tx = Tx
          { tx'inputs  = V.empty
          , tx'outputs = V.fromList [box]
          , tx'proof   = Nothing
          , tx'args    = mempty
          }

        initMoney = 1000000

-- | Initial master box for default genesis.
-- All funds belong to master-user.
initMasterBox :: BoxId
initMasterBox = BoxId "master:box-0"


