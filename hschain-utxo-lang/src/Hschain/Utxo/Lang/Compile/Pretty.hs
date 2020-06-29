{-# OPTIONS_GHC -Wno-orphans #-}
-- | Pretty printer for extended lambda-calculus language.
module Hschain.Utxo.Lang.Compile.Pretty(
) where

import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Hask.ToHask
import Hschain.Utxo.Lang.Core.Data.Prim (Name)

import qualified Language.Haskell.Exts.Pretty as H

instance Pretty CoreProg where
  pretty = pretty . H.prettyPrint . toHaskProg

instance Pretty (Comb Name) where
  pretty = pretty . H.prettyPrint . toHaskDecl

instance Pretty (Expr Name) where
  pretty = pretty . H.prettyPrint . toHaskExpr

