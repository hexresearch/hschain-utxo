module Hex.Common.TimeStampMap where

import Control.Concurrent.STM

import Data.Ord
import Data.Time
import Data.VectorSpace
import qualified Data.List as L
import qualified Data.IntervalMap.Interval as M
import qualified Data.IntervalMap.Strict as M

data TimeStampMap k v = TimeStampMap
  { timeStampMap'store     :: !(M.IntervalMap k (v, v))
  , timeStampMap'lastTime  :: !(Maybe (k, v))
  } deriving (Show, Eq)

empty :: TimeStampMap k v
empty = TimeStampMap M.empty Nothing

insert :: (Ord k) => k -> v -> TimeStampMap k v -> TimeStampMap k v
insert k v tm@(TimeStampMap store mlast) = case mlast of
  Just (lastTime, lastValue) ->
    if lastTime < k
      then TimeStampMap (M.insert (M.IntervalCO lastTime k) (lastValue, v) store) (Just (k, v))
      else tm
  Nothing -> TimeStampMap M.empty (Just (k, v))

intersecting :: Ord k => TimeStampMap k v -> M.Interval k -> M.IntervalMap k (v, v)
intersecting tm k = M.intersecting (timeStampMap'store tm) k

within :: Ord k => TimeStampMap k v -> M.Interval k -> M.IntervalMap k (v, v)
within tm k = M.within (timeStampMap'store tm) k

linInter :: forall k step v . (Ord k, Real step, Fractional step, Fractional (Scalar v), VectorSpace v)
  => (k -> k -> step) -> (step -> k -> k) -> (k, k) -> step -> TimeStampMap k v -> [(k, v)]
linInter distTime  nextTime (tMin, tMax) step tm =
  reverse $ snd $ L.foldl' go (tMin, []) $ fmap (\(x, v) -> ((M.lowerBound x, fst v), (M.upperBound x, snd v))) intervals
  where
    intervals = L.sortBy (comparing fst) $ M.toList $ intersecting tm (M.IntervalCO tMin tMax)

    go (t, res) val@((ta, _), (tb, _))
      | t > tMax = (t, res)
      | t > tb = (t, res)
      | t < ta = go (nextTime step t, res) val
      | otherwise = go (nextTime step t, (t, val `at` t) : res) val

    at :: ((k, v), (k, v)) -> k -> v
    at ((ta, va), (tb, vb)) t = va ^+^ (k *^ n)
      where
        n = vb ^-^ va
        k = realToFrac $ (distTime t ta) / (distTime tb ta)

---------------------------------------------------------

type TimeMap v = TimeStampMap UTCTime v

linInterTime :: (Fractional (Scalar v), VectorSpace v)
  => (UTCTime, UTCTime) -> NominalDiffTime -> TimeMap v -> [(UTCTime, v)]
linInterTime = linInter (\a b -> abs $ diffUTCTime a b) addUTCTime

---------------------------------------------------------

newtype MutableTimeMap v = MutableTimeMap {
  unMutableTimeStampMap :: TVar (TimeMap v) }

newTimeMap :: IO (MutableTimeMap v)
newTimeMap = fmap MutableTimeMap $ newTVarIO empty

insertTimeMap :: MutableTimeMap v -> UTCTime -> v -> IO ()
insertTimeMap (MutableTimeMap tv) k v = do
  atomically $ modifyTVar' tv $ insert k v

insertNowTimeMap :: MutableTimeMap v -> v -> IO ()
insertNowTimeMap tm v =
  (\now -> insertTimeMap tm now v) =<< getCurrentTime

readLinInterTime :: (Fractional (Scalar v), VectorSpace v)
  => (UTCTime, UTCTime) -> NominalDiffTime -> MutableTimeMap v -> IO [(UTCTime, v)]
readLinInterTime interval step (MutableTimeMap tv) =
  fmap (linInterTime interval step) $ readTVarIO tv
