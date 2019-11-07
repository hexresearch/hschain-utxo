module Hex.Common.Time.Const(
  minute, hour, day, week, month, year,
  minutes, hours, days, weeks, months, years
) where

import Data.Time hiding (months)

minute, hour, day, week, month, year :: NominalDiffTime

minute = 60
hour   = 60 * minute
day    = 24 * hour
week   = 7  * day
month  = 30 * day
year   = 365 * day

minutes, hours, days, weeks, months, years :: Double -> NominalDiffTime

minutes = period minute
hours   = period hour
days    = period day
weeks   = period week
months  = period month
years   = period year

period :: NominalDiffTime -> Double -> NominalDiffTime
period a n = realToFrac n * a
