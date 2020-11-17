-- | This module defines parser for our language.
--
-- Language looks exactly like simplified version of the Haskell.
-- So instead of writing our own parser we reuse package for Haskell parser haskell-src-exts.
-- And we just define convertion functions from and to Haskell AST to use it with our language.
module Hschain.Utxo.Lang.Parser.Hask(
    ParseResult(..)
  , SrcLoc(..)
  , parseExp
  , parseModule
  , parseBind
  , prettyExp
  , prettyModule
) where

import Control.Monad

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

import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Extension as H

-- | Parse expression.
-- First argument is name of the file (if defined) where expression is defined.
-- The name is used in source code locations for error reports.
parseExp :: Maybe FilePath -> String -> ParseResult Lang
parseExp mFile = withFile mFile (\mode -> fromHaskExp <=< H.parseExpWithMode mode)

-- | Parse modules.
-- First argument is name of the file (if defined) where expression is defined.
-- The name is used in source code locations for error reports.
parseModule :: Maybe FilePath -> String -> ParseResult Module
parseModule mFile = withFile mFile (\mode -> fromHaskModule <=< H.parseModuleWithMode mode)

-- | Parse bind declarations (@a = expr@).
parseBind :: Maybe FilePath -> String -> ParseResult (VarName, Lang)
parseBind mFile = withFile mFile (\mode -> getBind <=< H.parseDeclWithMode mode)
  where
    getBind x = do
      decl <- toDecl x
      case decl of
        FunDecl _ binds -> case binds of
          [(var, [alt])] -> return (var, altToExpr alt)
          _              -> err
        _ -> err

    err = parseFailed noLoc "Failed to parse bind"


withFile :: Maybe FilePath -> (H.ParseMode -> String -> ParseResult a) -> (String -> ParseResult a)
withFile mFile parseWith = parseWith (setFile H.defaultParseMode)
  where
    setFile = maybe id (\file mode -> mode { H.parseFilename = file
                                           , H.extensions = [H.EnableExtension H.TemplateHaskell, H.EnableExtension H.QuasiQuotes] } ) mFile


-- | Pretty-print expression
prettyExp :: Lang -> String
prettyExp = prettyPrint . toHaskExp

-- | Pretty-print module
prettyModule :: Module -> String
prettyModule = prettyPrint . toHaskModule

