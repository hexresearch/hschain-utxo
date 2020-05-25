module Hschain.Utxo.Lang.Core.Data.Output(
    Output
  , put
  , toList
) where

import Data.Sequence (Seq, (|>))

import qualified Data.Foldable as F(toList)

newtype Output = Output (Seq Int)
  deriving newtype (Semigroup, Monoid, Eq, Show)

put :: Int -> Output -> Output
put elem (Output seq) = Output $ seq |> elem

toList :: Output -> [Int]
toList (Output seq) = F.toList seq

