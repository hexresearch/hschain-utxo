-- | Various parsing utils.
module Hschain.Utxo.Lang.Parser.Hask.Utils where

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Hschain.Utxo.Lang.Expr

import qualified Data.Text as Text

import qualified Language.Haskell.Exts.SrcLoc as H
import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Pretty as H

parseFailed :: Loc -> String -> ParseResult a
parseFailed loc msg = ParseFailed (H.fromSrcInfo loc) msg

parseFailedBy :: (H.Annotated f, H.Pretty (f Loc)) => String -> f Loc -> ParseResult b
parseFailedBy msg expr = ParseFailed (H.fromSrcInfo $ H.ann expr) $ mconcat [msg, ": ", H.prettyPrint expr]

parseFailedVar :: String -> VarName -> ParseResult a
parseFailedVar msg v = ParseFailed (H.fromSrcInfo $ varName'loc v) $ mconcat [msg, ": ", Text.unpack $ varName'name v]


