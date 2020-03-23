module Hschain.Utxo.Lang.Parser.Hask(
    ParseResult(..)
  , SrcLoc(..)
  , parseExp
  , parseModule
  , parseBind
  , prettyExp
  , prettyModule
) where

import Control.Applicative
import Control.Monad

import Data.Fix

import Language.Haskell.Exts.SrcLoc (
    SrcLoc(..))

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

parseExp :: Maybe FilePath -> String -> ParseResult Lang
parseExp mFile = withFile mFile (\mode -> fromHaskExp <=< H.parseExpWithMode mode)

parseModule :: Maybe FilePath -> String -> ParseResult Module
parseModule mFile = withFile mFile (\mode -> fromHaskModule <=< H.parseModuleWithMode mode)

parseBind :: Maybe FilePath -> String -> ParseResult (VarName, Lang)
parseBind mFile = withFile mFile (\mode -> getBind <=< H.parseDeclWithMode mode)
  where
    getBind x = do
      decl <- toDecl x
      case decl of
        FunDecl _ binds -> case binds of
          [(var, alt)] -> return (var, altToExpr alt)
          _            -> err
        _ -> err

    err = parseFailed noLoc "Failed to parse bind"


withFile :: Maybe FilePath -> (H.ParseMode -> String -> ParseResult a) -> (String -> ParseResult a)
withFile mFile parseWith = parseWith (setFile H.defaultParseMode)
  where
    setFile = maybe id (\file mode -> mode { H.parseFilename = file } ) mFile


prettyExp :: Lang -> String
prettyExp = prettyPrint . toHaskExp

prettyModule :: Module -> String
prettyModule = prettyPrint . toHaskModule


