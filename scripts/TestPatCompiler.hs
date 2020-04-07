module TestPatCompiler where

import Data.Text.Prettyprint.Doc
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Parser.Hask

test :: String -> IO ()
test file = do
	ParseOk alts <- fmap (fmap (bind'alts . head . module'binds) . parseModule Nothing) $ readFile file
	let Right expr = runInferM (altGroupToExpr alts)
	print $ pretty expr




