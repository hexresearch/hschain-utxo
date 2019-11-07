module Hschain.Utxo.Back.Monad(
    ServerM
  , runServerM
  , readBoxChain
  , updateBoxChain
) where

import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Text (Text)

import Servant.Server

import Hschain.Utxo.Lang.Types
import Hschain.Utxo.State.Types
import Hschain.Utxo.Back.Env

-- | Server monad that holds internal environment
newtype ServerM a = ServerM { unServerM :: ReaderT AppEnv Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadReader AppEnv
           , MonadError ServantErr, MonadThrow, MonadCatch)

newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT AppEnv Handler) a }

instance MonadBaseControl IO ServerM where
    type StM ServerM a = StMServerM a
    liftBaseWith f = ServerM $ liftBaseWith $ \q -> f (fmap StMServerM . q . unServerM)
    restoreM = ServerM . restoreM . unStMServerM

-- | Execution of 'ServerM'
runServerM :: AppEnv -> ServerM a -> Handler a
runServerM e = flip runReaderT e . unServerM

-- | Execution of 'ServerM' in IO monad
runServerMIO :: AppEnv -> ServerM a -> IO a
runServerMIO env m = do
  ea <- runHandler $ runServerM env m
  case ea of
    Left e -> fail $ "runServerMIO: " <> show e
    Right a -> return a

-- | Reads current state of the block chain
readBoxChain :: ServerM BoxChain
readBoxChain = do
  tv <- asks appEnv'boxChain
  liftIO $ readTVarIO tv

updateBoxChain :: (BoxChain -> (Either Text (TxHash, BoxChain), Text)) -> ServerM ((Either Text TxHash), Text)
updateBoxChain react = do
  tv <- asks appEnv'boxChain
  liftIO $ atomically $ do
    bch <- readTVar tv
    case react bch of
      (Left err, msg) -> return $ (Left err, msg)
      (Right (txHash, nextBch), msg) -> do
        writeTVar tv nextBch
        return $ (Right txHash, msg)

