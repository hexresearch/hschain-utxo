module Hschain.Utxo.Lang.Core.Data.Output(
    Output
  , put
  , toList
  , simplifySigmas
) where

import Control.DeepSeq

import Data.Sequence (Seq, (|>))

import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Sigma

import qualified Data.Foldable as F(toList)

newtype Output = Output (Seq Prim)
  deriving newtype (Semigroup, Monoid, Eq, Show, NFData)

put :: Prim -> Output -> Output
put el (Output xs) = Output $ xs |> el

toList :: Output -> [Prim]
toList (Output xs) = F.toList xs

simplifySigmas :: Output -> Output
simplifySigmas (Output prims) = Output $ fmap onPrim prims
  where
    onPrim = \case
      PrimSigma expr -> either PrimBool PrimSigma $ eliminateSigmaBool expr
      other          -> other

