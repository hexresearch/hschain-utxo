module Hschain.Utxo.Lang.Parser.Hask(
    ParseResult(..)
  , parseExp
  , parseModule
  , parseBind
  , prettyExp
  , prettyModule
) where

import Control.Applicative
import Control.Monad

import Data.Fix

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Language.Haskell.Exts.Pretty

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Parser.Hask.Dependencies
import Hschain.Utxo.Lang.Parser.Hask.FromHask
import Hschain.Utxo.Lang.Parser.Hask.ToHask
import Hschain.Utxo.Lang.Parser.Hask.Utils

import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Parser as H

parseExp :: String -> ParseResult Lang
parseExp = fromHaskExp <=< H.parseExp

parseModule :: String -> ParseResult Module
parseModule = fromHaskModule <=< H.parseModule

prettyExp :: Lang -> String
prettyExp = prettyPrint . toHaskExp

prettyModule :: Module -> String
prettyModule = prettyPrint . toHaskModule


