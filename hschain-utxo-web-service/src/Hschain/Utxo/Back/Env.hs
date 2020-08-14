-- | Defines state of the node service

{-# LANGUAGE DerivingStrategies, DerivingVia #-}
module Hschain.Utxo.Back.Env where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Catch

import Data.Text (Text)
import Data.Maybe                (fromMaybe)
import Data.Time.Clock           (getCurrentTime)
import Data.String

import System.FilePath
import System.Directory

import HSChain.Store

import HSChain.Control.Class
import HSChain.Logger
import HSChain.Mempool
import HSChain.Monitoring

import Hschain.Utxo.Lang
import Hschain.Utxo.Back.Config
import Hschain.Utxo.State.React
import Hschain.Utxo.State.Types
import Hschain.Utxo.Blockchain

import GHC.Generics (Generic)
import Katip
import qualified Data.Text as T

import HSChain.Logger

import Hschain.Utxo.Back.Config (LogSpec(..))

-- | Application environment
data AppEnv = AppEnv
  { appEnv'bchain :: Bchain IO  -- ^ bridge to hschain internals
  }

-- | Setup service environment.
--
-- It takes node-specification and genesis.
initEnv :: (MonadIO m, MonadMask m) => NodeSpec -> Genesis -> ContT r m (AppEnv, [IO ()])
initEnv nspec genesis = do
  (conn, logEnv) <- allocNode nspec
  liftIO $ initEnvBy conn logEnv nspec genesis

initEnvBy :: Connection 'RW -> LogEnv -> NodeSpec -> [Tx] -> IO (AppEnv, [IO ()])
initEnvBy conn logenv nspec genesis = do
  (bchain, acts) <- run $ interpretSpec (const $ pure ()) nspec genesis
  return $ (AppEnv $ hoistBchain run bchain, fmap run acts)
  where
    run :: AppT m x -> m x
    run = undefined
          --runLoggerT logenv . runDBT conn

runAppT :: AppStateM -> AppT m a -> m a
runAppT d = flip runReaderT d . unAppT

data AppStateM = AppStateM
  { appNamespace :: !Namespace
  , appLogEnv    :: !LogEnv
  , appConn      :: !(Connection 'RW)
  , appCached    :: !(Cached BData)
  }
  deriving stock (Generic)

newtype AppT m a = AppT { unAppT :: ReaderT AppStateM m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving newtype (MonadThrow, MonadCatch, MonadMask, MonadFork)
  deriving newtype (MonadReader AppStateM)
  -- HSChain instances
  deriving MonadTMMonitoring      via NoMonitoring   (AppT m)
  deriving MonadLogger            via LoggerByTypes  (AppT m)
  deriving (MonadReadDB, MonadDB) via DatabaseByType (AppT m)
  deriving (MonadCached BData)    via CachedByType BData (AppT m)


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

