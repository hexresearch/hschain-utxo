module Hschain.Utxo.Lang.Core.Data.Output(
    Output
  , put
  , toList
) where

import Data.Sequence (Seq, (|>))

import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Foldable as F(toList)

newtype Output = Output (Seq Prim)
  deriving newtype (Semigroup, Monoid, Eq, Show)

put :: Prim -> Output -> Output
put elem (Output seq) = Output $ seq |> elem

toList :: Output -> [Prim]
toList (Output seq) = F.toList seq

