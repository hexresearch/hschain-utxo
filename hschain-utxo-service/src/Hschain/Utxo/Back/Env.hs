module Hschain.Utxo.Back.Env where

import Control.Concurrent.STM
import Control.Monad

import Data.Text (Text)
import Data.Maybe                (fromMaybe)
import Data.Time.Clock           (getCurrentTime)

import Hschain.Utxo.Lang
import Hschain.Utxo.Back.Config
import Hschain.Utxo.State.React
import Hschain.Utxo.State.Types

import GHC.Generics (Generic)
import Katip
import qualified Data.Text as T

import HSChain.Logger

import Hschain.Utxo.Back.Config (LogSpec(..))

data AppEnv = AppEnv
  { appEnv'boxChain :: TVar BoxChain
  }

emptyAppEnv :: IO AppEnv
emptyAppEnv = fmap AppEnv $ newTVarIO emptyBoxChain

initEnv  :: Genesis -> IO AppEnv
initEnv genesis = do
  let bch = case applyTxs genesis emptyBoxChain of
        Left err -> error $ T.unpack $ mconcat ["Genesis is wrong: ", err]
        Right nextEnv -> nextEnv
  env <- fmap AppEnv $ newTVarIO bch
  return env
  where

applyTxs :: [Tx] -> BoxChain -> Either Text BoxChain
applyTxs txs = foldl (>=>) pure $ fmap (fmap fst . react) txs


data KatipEnv = KatipEnv
  { katipEnv'namespace :: Namespace
  , katipEnv'logEnv    :: LogEnv
  } deriving (Generic)

newKatipEnv :: LogSpec -> Text -> IO KatipEnv
newKatipEnv LogSpec{..} appName = do
  logEnv <- do le <- initLogEnv appNameSpace $ Environment $ fromMaybe "" logSpec'clusterId
               return le { _logEnvTimer = getCurrentTime
                         , _logEnvHost  = case logSpec'hostnameSuffix of
                                            Nothing -> id
                                            Just s  -> (<>s)
                                        $ _logEnvHost le
                         }
  let names = [T.pack ("log_" ++ show i) | i <- [0::Int ..]]
  logEnvWithScribes <- foldM (\le (nm,s) -> makeScribe s >>= (\scribe -> registerScribe nm scribe defaultScribeSettings le))
    logEnv (zip names logSpec'logFiles)
  return $ KatipEnv appNameSpace logEnvWithScribes
  where
    appNameSpace = Namespace [appName]


