module Hschain.Utxo.Lang.Core.Data.Vstack(
    Vstack
  , put
  , pop
) where

import Hschain.Utxo.Lang.Core.Data.Prim

import Data.Sequence (Seq, (<|), ViewL(..))
import qualified Data.Sequence as S

newtype Vstack = Vstack (Seq Prim)
  deriving newtype (Show, Eq, Semigroup, Monoid)

put :: Prim -> Vstack -> Vstack
put n (Vstack seq) = Vstack (n <| seq)

pop :: Vstack -> (Maybe Prim, Vstack)
pop (Vstack seq) =
  case S.viewl seq of
    EmptyL  -> (Nothing, Vstack seq)
    a :< as -> (Just a, Vstack as)


