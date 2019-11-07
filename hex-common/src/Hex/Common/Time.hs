module Hex.Common.Time(
    subdivideInterval
  , distanceUTCTime
  -- * Integer time in seconds
  , IntTime
  , IntDiffTime
  , toIntTime
  , fromIntTime
  , addIntTime
  , getCurrentIntTime
  , distanceIntTime
) where

import Data.Int

import Data.Time
import Data.Time.Clock.POSIX

distanceUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
distanceUTCTime a b = abs (diffUTCTime a b)

subdivideInterval :: NominalDiffTime -> (UTCTime, UTCTime) -> [(UTCTime, UTCTime)]
subdivideInterval dividerDur (from, to) = takeWhile ((<= to) . snd) $
  iterate (\(_, t) -> nextInterval t) (nextInterval from)
  where
    nextInterval a = (a, addUTCTime dividerDur a)

------------------------------------------------

-- | POSIX-time
type IntTime = Int64

-- | POSIX-Time in integer seconds
type IntDiffTime = Int64

toIntTime :: UTCTime -> IntTime
toIntTime = floor . utcTimeToPOSIXSeconds

fromIntTime :: IntTime -> UTCTime
fromIntTime = posixSecondsToUTCTime . fromIntegral

distanceIntTime :: IntTime -> IntTime -> IntDiffTime
distanceIntTime a b = abs (b - a)

getCurrentIntTime :: IO IntTime
getCurrentIntTime = fmap toIntTime getCurrentTime

addIntTime :: IntDiffTime -> IntTime -> IntTime
addIntTime = (+)
