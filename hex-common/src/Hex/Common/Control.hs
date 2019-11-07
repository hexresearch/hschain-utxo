module Hex.Common.Control(
    periodic
  , fmap2
  , fmap3
  , catchAny
  , zoomState
  , funzip
  , funzip3
) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Time
import Hex.Common.Delay

{-# NOINLINE periodic #-}
periodic :: MonadIO m => NominalDiffTime -> m () -> m ()
periodic time proc = fix $ \next -> proc >> sleep time >> next

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f x = fmap (fmap f) x

fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 f x = fmap (fmap2 f) x

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

zoomState :: (cont -> el) -> (cont -> el -> cont) -> State el a -> State cont a
zoomState getter setter f = StateT $ \st ->
  let (a, el) = runState f (getter st)
  in  pure (a, setter st el)

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip xs = (fmap fst xs, fmap snd xs)

funzip3 :: Functor f => f (a, b, c) -> (f a, f b, f c)
funzip3 xs = (fmap get1 xs, fmap get2 xs, fmap get3 xs)
  where
    get1 (a, _, _) = a
    get2 (_, a, _) = a
    get3 (_, _, a) = a
