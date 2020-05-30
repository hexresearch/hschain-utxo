-- | Gmachine definition and evaluation.
module Hschain.Utxo.Lang.Core.Gmachine(
    Gmachine(..)
  , eval
  , Error(..)
) where

import Hschain.Utxo.Lang.Core.Gmachine.Monad
import Hschain.Utxo.Lang.Core.Gmachine.Eval

