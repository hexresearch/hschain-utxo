module Hschain.Utxo.Back.Env where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Catch

import Data.Text (Text)
import Data.Maybe                (fromMaybe)
import Data.Time.Clock           (getCurrentTime)
import Data.String

import System.FilePath
import System.Directory

import HSChain.Store

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

data AppEnv = AppEnv
  { appEnv'bchain :: Bchain IO
  }

initEnv :: (MonadIO m, MonadMask m) => NodeSpec -> [Tx] -> ContT r m (AppEnv, [IO ()])
initEnv nspec genesis = do
  (conn, logEnv) <- allocNode nspec
  liftIO $ initEnvBy conn logEnv nspec genesis

initEnvBy :: Connection 'RW BData -> LogEnv -> NodeSpec -> [Tx] -> IO (AppEnv, [IO ()])
initEnvBy conn logenv nspec genesis = do
  (bchain, acts) <- run $ interpretSpec (const $ pure ()) nspec genesis
  return $ (AppEnv $ hoistBchain run bchain, fmap run acts)
  where
    run :: DBT 'RW BData (LoggerT m) x -> m x
    run = runLoggerT logenv . runDBT conn

-- | Allocate resources for node
allocNode
  :: ( MonadIO m, MonadMask m)
  => NodeSpec
  -> ContT r m (Connection 'RW a, LogEnv)
allocNode NodeSpec{..} = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory nspec'dbName
  conn   <- ContT $ withDatabase nspec'dbName
  logenv <- ContT $ withLogEnv namespace "DEV" [ makeScribe s | s <- logSpec'files nspec'logs ]
  return (conn,logenv)
  where
    namespace = fromString $ T.unpack $ logSpec'namespace nspec'logs

