{-# OPTIONS_GHC -Wno-orphans #-}
-- | Pretty printer for extended lambda-calculus language.
module Hschain.Utxo.Lang.Compile.Pretty(
) where

import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Hask.ToHask
import Hschain.Utxo.Lang.Core.Types (Name)

import qualified Language.Haskell.Exts.Pretty as H
import qualified Hschain.Utxo.Lang.Compile.Hask.TypedToHask as TypedToHask

instance Pretty LamProg where
  pretty = pretty . H.prettyPrint . toHaskProg

instance Pretty (Comb Name) where
  pretty = pretty . H.prettyPrint . toHaskDecl

instance Pretty (ExprLam Name) where
  pretty = pretty . H.prettyPrint . toHaskExpr

instance Pretty TypedLamProg where
  pretty = pretty . H.prettyPrint . TypedToHask.toHaskProg

instance Pretty TypedDef where
  pretty = vcat . fmap (pretty . H.prettyPrint) . TypedToHask.toHaskDecl

instance Pretty TypedExprLam where
  pretty = pretty . H.prettyPrint . TypedToHask.toHaskExpr

