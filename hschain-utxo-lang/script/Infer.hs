-- | Script to test type-inference
module Infer where

import Hschain.Utxo.Lang.Infer
import qualified Hschain.Utxo.Lang.Parser.Hask as P
import Hschain.Utxo.Lang.Lib.Base
import Hschain.Utxo.Lang.Exec.Module

infer = do
  P.ParseOk m <- fmap (P.parseModule Nothing) $ readFile "../test2.hs"
  print $ evalModule langTypeContext m

