-- | Defines state of the node service
module Hschain.Utxo.Back.Env where

import Control.Monad.Cont
import Control.Monad.Catch

import Data.Maybe                (fromMaybe)
import Data.String

import System.FilePath
import System.Directory

import HSChain.Store

import Hschain.Utxo.Lang
import Hschain.Utxo.Back.Config
import Hschain.Utxo.Blockchain

import Katip
import qualified Data.Text as T

import HSChain.Logger


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
  mapM_ (\db -> liftIO $ createDirectoryIfMissing True $ takeDirectory db) nspec'dbName
  conn   <- ContT $ withDatabase $ fromMaybe ":memory:" nspec'dbName
  logenv <- ContT $ withLogEnv namespace "DEV" [ makeScribe s | s <- logSpec'files nspec'logs ]
  return (conn,logenv)
  where
    namespace = fromString $ T.unpack $ logSpec'namespace nspec'logs

