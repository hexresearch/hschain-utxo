{-# OPTIONS_GHC -Wno-orphans #-}
-- | Pretty printer for extended lambda-calculus language.
module Hschain.Utxo.Lang.Compile.Pretty(
) where

import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Hask.ToHask
import Hschain.Utxo.Lang.Core.Data.Prim (Name)

import qualified Language.Haskell.Exts.Pretty as H
import qualified Hschain.Utxo.Lang.Compile.Hask.TypedToHask as TypedToHask

instance Pretty CoreProg where
  pretty = pretty . H.prettyPrint . toHaskProg

instance Pretty (Comb Name) where
  pretty = pretty . H.prettyPrint . toHaskDecl

instance Pretty (Expr Name) where
  pretty = pretty . H.prettyPrint . toHaskExpr

instance Pretty TypedProg where
  pretty = pretty . H.prettyPrint . TypedToHask.toHaskProg

instance Pretty TypedDef where
  pretty = vcat . fmap (pretty . H.prettyPrint) . TypedToHask.toHaskDecl

instance Pretty TypedExpr where
  pretty = pretty . H.prettyPrint . TypedToHask.toHaskExpr

