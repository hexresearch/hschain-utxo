module Hschain.Utxo.Test.Client.Proc(
    Event(..)
  , Proc(..)
  , runProc
) where

import Control.Monad
import Control.Timeout

import Data.Ord

import qualified Data.List as L

data Event a = Event
  { event'time      :: NominalDiffTime
  , event'content   :: a
  }

newtype Proc a = Proc [Event a]

runProc :: (a -> IO ()) -> Proc a -> IO ()
runProc eval (Proc es) = execEvents . makeDiffTimeList . sortEvents $ es
  where
    sortEvents = L.sortBy (comparing event'time)

    makeDiffTimeList xs = case xs of
      []   -> []
      a:as -> a : zipWith go xs as
      where
        go a b = Event (event'time b - event'time a) (event'content b)

    execEvents = mapM_ $ \Event{..} -> do
      sleep event'time
      eval event'content

