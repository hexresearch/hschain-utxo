module Hex.Common.ClockBuffer(
    ClockBuffer
  , new
  , append
  , appendLastAgain
  , getLastValue
  , getResolution
  , readRange
  , null
  , length
) where

import Prelude hiding (null, length)

import Control.Applicative
import Control.Monad

import Data.IORef
import Data.Time

import Hex.Common.Time

import qualified Hex.Common.CircularBuffer as C
import qualified Data.Vector.Unboxed.Mutable as V

data ClockBuffer a = ClockBuffer
  { cbuffer'resolution       :: !IntDiffTime
  , cbuffer'start            :: !(IORef IntTime)
  , cbuffer'content          :: !(C.CircularBuffer a)
  , cbuffer'lastValue        :: !(IORef (Maybe a))
  , cbuffer'extendUpperBound :: Maybe (a -> a)
  }

-- | Allocates space for new clock buffer.
--
-- > new resolution start lifeSpan
--
-- * resolution - is minimal step of the timeinterval.
-- * start - is time for first value (In posix seconds).
-- * lifeSpan - time span for the the whole set of values.
--    it specifies how many values to allocate per buffer.
-- * extend upper bound function if supplied it fills
--     the upper bound with values that are sult of this function
--     applied to the last value.
new :: V.Unbox a => NominalDiffTime -> IntTime -> NominalDiffTime -> Maybe (a -> a) -> IO (ClockBuffer a)
new resolution start totalDuration mUpperBoundFun = do
  buffer <- C.new totalSize
  startRef <- newIORef start
  lastValueRef <- newIORef Nothing
  return $ ClockBuffer
    { cbuffer'resolution = floor resolution
    , cbuffer'start = startRef
    , cbuffer'content = buffer
    , cbuffer'lastValue = lastValueRef
    , cbuffer'extendUpperBound = mUpperBoundFun
    }
  where
    totalSize = floor $ (realToFrac totalDuration :: Double) / (realToFrac resolution)


-- | Appends value to the buffer. It's assumed that timestamp equals to last value timestamp
-- plus buffer resolution.
append :: V.Unbox a => ClockBuffer a -> a -> IO ()
append buf !val = do
  C.append (cbuffer'content buf) val
  updateLastValue buf
  updateStartTime buf
  where
    updateLastValue ClockBuffer{..} =
      writeIORef cbuffer'lastValue $ Just val

    updateStartTime ClockBuffer{..} = do
      needsBump <- C.isFull cbuffer'content
      when needsBump $ atomicModifyIORef' cbuffer'start $ \x -> (cbuffer'resolution + x, ())

-- | Appends last value again, if the extendUpperBound function is defined
-- it appends the value that is applied to this function.
appendLastAgain :: V.Unbox a => ClockBuffer a -> IO ()
appendLastAgain buf = mapM_ (append buf . extendLast . snd) =<< getLastValue buf
  where
    extendLast = maybe id id $ cbuffer'extendUpperBound buf

getResolution :: ClockBuffer a -> IntDiffTime
getResolution = cbuffer'resolution

getLastValue :: V.Unbox a => ClockBuffer a -> IO (Maybe (IntTime, a))
getLastValue ClockBuffer{..} = do
  len <- C.length cbuffer'content
  start <- readIORef cbuffer'start
  if (len == 0)
    then return Nothing
    else fmap (Just . (start + (fromIntegral $ len - 1) * cbuffer'resolution, )) $ C.read cbuffer'content (len - 1)

-- | Reads values within given time range with buffer inherent resolution.
-- reads from the interval [from, to). Includes lower bound but not the upper bound.
readRange :: V.Unbox a => ClockBuffer a -> (IntTime, IntTime) -> IO [(IntTime, a)]
readRange buf@ClockBuffer{..} (from, to) = do
  isNull <- null buf
  if isNull
    then return []
    else do
      start <- readIORef cbuffer'start
      (from', to') <- getBounds buf
      getContent start (max from' from, min to' to)
  where
    getBounds = liftA2 (liftA2 (,)) getStartTime getEndTime

    getContent start (a, b) = fmap (zip (getRangePoints cbuffer'resolution (a, b))) getPoints
      where
        getPoints = C.readRange cbuffer'content (toIndex start a, toIndex start b)

        toIndex t0 t = fromIntegral $ (t - t0) `div` cbuffer'resolution

    getRangePoints dt (a, b) = takeWhile (< b) $ iterate ( + dt) a

-- | Reads the start time of the time span
getStartTime :: ClockBuffer a -> IO IntTime
getStartTime ClockBuffer{..} = readIORef cbuffer'start

-- | Reads the end time of the time span
getEndTime :: ClockBuffer a -> IO IntTime
getEndTime buf@ClockBuffer{..} = do
  start <- getStartTime buf
  size <- C.length cbuffer'content
  return $ start + fromIntegral size * cbuffer'resolution

null :: V.Unbox a => ClockBuffer a -> IO Bool
null ClockBuffer{..} = C.null cbuffer'content

length :: ClockBuffer a -> IO Int
length ClockBuffer{..} = C.length cbuffer'content
