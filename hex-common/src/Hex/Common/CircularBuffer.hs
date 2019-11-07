-- | Data structure for circular buffer.
-- It's not thread safe. Meant to be used with single thread.
module Hex.Common.CircularBuffer(
    CircularBuffer
  , new
  , append
  , length
  , null
  , isFull
  , read
  , readRange
  , foldRange
) where

import Prelude hiding (read, length, null)

import Data.IORef
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.List as L

-- | Circular buffer
data CircularBuffer a = CircularBuffer
  { cbuffer'totalSize    :: !Int
  , cbuffer'currentSize  :: !(IORef Int)
  , cbuffer'content      :: !(V.IOVector a)
  , cbuffer'lastIx       :: !(IORef Int)
  }

-- | Allocates space for circular buffer of the given size.
new :: V.Unbox a => Int -> IO (CircularBuffer a)
new size = do
  lastIx <- newIORef 0
  currentSize <- newIORef 0
  content <- V.new size
  return $ CircularBuffer
    { cbuffer'totalSize = size
    , cbuffer'currentSize = currentSize
    , cbuffer'content = content
    , cbuffer'lastIx = lastIx
    }

-- | Appends element to the tail of the buffer.
-- If buffer is full the oldest element is erased.
append :: V.Unbox a => CircularBuffer a -> a -> IO ()
append buf el = do
  writeElem buf el
  bumpSize buf
  where
    writeElem CircularBuffer{..} x = do
      lastIx <- readIORef cbuffer'lastIx
      V.write cbuffer'content lastIx x
      writeIORef cbuffer'lastIx $! (lastIx + 1) `mod` cbuffer'totalSize

    bumpSize CircularBuffer{..} =
      atomicModifyIORef' cbuffer'currentSize $ \size -> ((size + 1) `min` cbuffer'totalSize, ())

-- | Returns current size of the buffer.
length :: CircularBuffer a -> IO Int
length CircularBuffer{..} = readIORef cbuffer'currentSize

null :: CircularBuffer a -> IO Bool
null = fmap (== 0) . length

isFull :: CircularBuffer a -> IO Bool
isFull CircularBuffer{..} = fmap (cbuffer'totalSize == ) $ readIORef cbuffer'currentSize

-- |Reads element in the range. If element is beyond the size
-- it returns the elemnts by mod of the totalSize.
--
-- Reading empty list produces exception.
read :: V.Unbox a => CircularBuffer a -> Int -> IO a
read buf ix = do
  firstIx <- getFirstIx buf
  currentSize <- readIORef $ cbuffer'currentSize buf
  V.read (cbuffer'content buf) ((firstIx + (ix `mod` currentSize)) `mod` cbuffer'totalSize buf)

getFirstIx :: CircularBuffer a -> IO Int
getFirstIx CircularBuffer{..} = do
  lastIx <- readIORef cbuffer'lastIx
  currentSize <- readIORef cbuffer'currentSize
  return $ (lastIx - currentSize) `mod` cbuffer'totalSize

-- | Reads all elements in the range.
readRange :: V.Unbox a => CircularBuffer a -> (Int, Int) -> IO [a]
readRange buf (from, to) = do
  firstIx <- getFirstIx buf
  currentSize <- readIORef $ cbuffer'currentSize buf
  mapM (\ix -> V.read (cbuffer'content buf) ((firstIx + (ix `mod` currentSize)) `mod` cbuffer'totalSize buf)) [from .. to]

-- | Folds
foldRange :: V.Unbox a => CircularBuffer a -> (Int, Int) -> (b -> a -> b) -> b -> IO b
foldRange buf range f z = fmap (L.foldl' f z) $ readRange buf range
