module Hschain.Utxo.Lang.Core.Data.Output(
    Output
  , put
  , toList
) where

import Control.DeepSeq

import Data.Sequence (Seq, (|>))

import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Foldable as F(toList)

newtype Output = Output (Seq Prim)
  deriving newtype (Semigroup, Monoid, Eq, Show, NFData)

put :: Prim -> Output -> Output
put el (Output xs) = Output $ xs |> el

toList :: Output -> [Prim]
toList (Output xs) = F.toList xs
