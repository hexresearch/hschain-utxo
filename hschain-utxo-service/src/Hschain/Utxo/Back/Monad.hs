{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
-- | Monad for hschain-utxo service
module Hschain.Utxo.Back.Monad(
    ServerM
  , AppEnv(..)
  , HSChainT(..)
  , runHSChainT
  , runServerM
  , runServerMIO
  , readBoxChain
  , initEnv
) where

import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Trans.Control

import Data.Maybe
import Data.String
import qualified Data.Text as T
import Katip
import Servant.Server
import System.FilePath
import System.Directory
import GHC.Generics (Generic)

import HSChain.Control.Class
import HSChain.Logger
import HSChain.Store
import HSChain.Monitoring
import Hschain.Utxo.Blockchain
import Hschain.Utxo.State.Types
import Hschain.Utxo.Lang.Types

----------------------------------------------------------------
-- Monads
----------------------------------------------------------------

-- | Application environment
data AppEnv = AppEnv
  { appEnv'bchain :: Bchain IO  -- ^ bridge to hschain internals
  }

-- | Server monad that holds internal environment
newtype ServerM a = ServerM { unServerM :: ReaderT AppEnv Handler a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadIO, MonadBase IO, MonadReader AppEnv
                   , MonadThrow, MonadCatch)
  deriving MonadTMMonitoring via NoMonitoring (ReaderT AppEnv Handler)

newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT AppEnv Handler) a }

instance MonadBaseControl IO ServerM where
    type StM ServerM a = StMServerM a
    liftBaseWith f = ServerM $ liftBaseWith $ \q -> f (fmap StMServerM . q . unServerM)
    restoreM = ServerM . restoreM . unStMServerM

instance MonadBChain ServerM where
  askBchain = asks appEnv'bchain

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
readBoxChain = readBoxChainState


data HSDict a = HSDict
  { dictConn      :: Connection 'RW
  , dictCache     :: Cached a
  , dictLogEnv    :: LogEnv
  , dictNamespace :: Namespace
  }
  deriving (Generic)

newtype HSChainT a m x = HSChainT (ReaderT (HSDict a) m x)
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadFail)
  deriving newtype (MonadThrow,MonadCatch,MonadMask,MonadFork)
  -- HSChain instances
  deriving MonadTMMonitoring      via NoMonitoring   (ReaderT (HSDict a) m)
  deriving MonadLogger            via LoggerByTypes  (ReaderT (HSDict a) m)
  deriving (MonadReadDB, MonadDB) via DatabaseByType (ReaderT (HSDict a) m)
  deriving (MonadCached a)        via CachedByType a (ReaderT (HSDict a) m)

runHSChainT :: MonadIO m => LogEnv -> Connection 'RW -> HSChainT a m x -> m x
runHSChainT logEnv conn (HSChainT m) = do
  cache <- newCached
  runReaderT m HSDict
    { dictConn      = conn
    , dictCache     = cache
    , dictLogEnv    = logEnv
    , dictNamespace = ""
    }

----------------------------------------------------------------
-- Initialization
----------------------------------------------------------------

-- | Setup service environment.
--
-- It takes node-specification and genesis.
initEnv :: (MonadIO m, MonadMask m) => NodeSpec -> [Tx] -> ContT r m (AppEnv, [IO ()])
initEnv nspec genesis = do
  (conn, logEnv) <- allocNode nspec
  liftIO $ initEnvBy conn logEnv nspec genesis

initEnvBy :: Connection 'RW -> LogEnv -> NodeSpec -> [Tx] -> IO (AppEnv, [IO ()])
initEnvBy conn logenv nspec genesis = do
  cache          <- newCached
  let run :: HSChainT BData m a -> m a
      run (HSChainT m) = runReaderT m HSDict
        { dictConn      = conn
        , dictCache     = cache
        , dictLogEnv    = logenv
        , dictNamespace = ""
        }
  (bchain, acts) <- run $ interpretSpec (const $ pure ()) nspec genesis
  return $ (AppEnv $ hoistBchain run bchain, fmap run acts)

-- | Allocate resources for node
allocNode
  :: ( MonadIO m, MonadMask m)
  => NodeSpec
  -> ContT r m (Connection 'RW, LogEnv)
allocNode NodeSpec{..} = do
  mapM_ (\db -> liftIO $ createDirectoryIfMissing True $ takeDirectory db) nspec'dbName
  conn   <- ContT $ withDatabase $ fromMaybe ":memory:" nspec'dbName
  logenv <- ContT $ withLogEnv namespace "DEV" [ makeScribe s | s <- logSpec'files nspec'logs ]
  return (conn,logenv)
  where
    namespace = fromString $ T.unpack $ logSpec'namespace nspec'logs

