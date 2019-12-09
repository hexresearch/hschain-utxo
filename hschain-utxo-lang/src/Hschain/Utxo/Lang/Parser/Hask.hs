module Hschain.Utxo.Lang.Parser.Hask(
    ParseResult(..)
  , parseExp
  , parseModule
  , prettyExp
  , prettyModule
) where

import Control.Monad

import Data.Fix

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc

import Type.Loc
import Type.Type

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Parser.Hask.FromHask
import Hschain.Utxo.Lang.Parser.Hask.ToHask

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

