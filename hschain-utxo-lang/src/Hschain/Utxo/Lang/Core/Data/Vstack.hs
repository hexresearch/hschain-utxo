module Hschain.Utxo.Lang.Core.Data.Vstack(
    Vstack
  , put
  , pop
) where

import Control.DeepSeq
import Data.Sequence (Seq, (<|), ViewL(..))
import qualified Data.Sequence as S

import Hschain.Utxo.Lang.Core.Data.Prim

newtype Vstack = Vstack (Seq Prim)
  deriving newtype (Show, Eq, Semigroup, Monoid, NFData)

put :: Prim -> Vstack -> Vstack
put n (Vstack xs) = Vstack (n <| xs)

pop :: Vstack -> (Maybe Prim, Vstack)
pop (Vstack xs) =
  case S.viewl xs of
    EmptyL  -> (Nothing, Vstack xs)
    a :< as -> (Just a, Vstack as)


