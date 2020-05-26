module Hschain.Utxo.Lang.Core.Data.Vstack(
    Vstack
  , put
  , pop
) where

import Data.Sequence (Seq, (<|), ViewL(..))
import qualified Data.Sequence as S

newtype Vstack = Vstack (Seq Int)
  deriving newtype (Show, Eq, Semigroup, Monoid)

put :: Int -> Vstack -> Vstack
put n (Vstack seq) = Vstack (n <| seq)

pop :: Vstack -> (Maybe Int, Vstack)
pop (Vstack seq) =
  case S.viewl seq of
    EmptyL  -> (Nothing, Vstack seq)
    a :< as -> (Just a, Vstack as)


