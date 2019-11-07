module Hex.Common.TvMap(
    TvMap(..)
  , newTvMap
  , getValue
  , setValue
  , deleteValue
  , modifyValue
  , isEmpty

  , newTvMapIO
  , getValueIO
  , setValueIO
  , deleteValueIO
  , modifyValueIO
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class

import Data.Map.Strict
import qualified Data.Map.Strict as M

newtype TvMap k v = TvMap { unTvMap :: TVar (Map k v) }

newTvMap :: STM (TvMap k v)
newTvMap = fmap TvMap $ newTVar M.empty

getValue :: Ord k => TvMap k v -> k -> STM (Maybe v)
getValue (TvMap tv) k = fmap (M.lookup k) $ readTVar tv

setValue :: Ord k => TvMap k v -> k -> v -> STM ()
setValue (TvMap tv) k v = modifyTVar' tv $ M.insert k v

deleteValue :: Ord k => TvMap k v -> k -> STM ()
deleteValue (TvMap tv) k = modifyTVar' tv $ M.delete k

modifyValue :: Ord k => TvMap k v -> k -> (v -> v) -> STM ()
modifyValue (TvMap tv) k f = modifyTVar' tv $ M.adjust f k

isEmpty :: TvMap k v -> STM Bool
isEmpty (TvMap tv) = fmap M.null $ readTVar tv

---------------------
-- IO lifts

newTvMapIO :: MonadIO m => m (TvMap k v)
newTvMapIO = stm $ newTvMap

getValueIO :: (MonadIO io, Ord k) => TvMap k v -> k -> io (Maybe v)
getValueIO tv k = stm $ getValue tv k

setValueIO :: (MonadIO io, Ord k) => TvMap k v -> k -> v -> io ()
setValueIO tv k v = stm $ setValue tv k v

deleteValueIO :: (MonadIO io, Ord k) => TvMap k v -> k -> io ()
deleteValueIO tv k = stm $ deleteValue tv k

modifyValueIO :: (MonadIO io, Ord k) => TvMap k v -> k -> (v -> v) -> io ()
modifyValueIO tv k f = stm $ modifyValue tv k f

---------------------

stm :: MonadIO io => STM a -> io a
stm = liftIO . atomically
