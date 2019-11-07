module Hschain.Utxo.Back.Env where

import Control.Concurrent.STM
import Control.Monad

import Data.Text (Text)

import Hschain.Utxo.Back.Config
import Hschain.Utxo.State.React
import Hschain.Utxo.State.Types

import qualified Data.Text as T

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

applyTxs :: Genesis -> BoxChain -> Either Text BoxChain
applyTxs txs = foldl (>=>) pure $ fmap (fmap ((fmap snd) . fst) . react) txs
