{-# Language NoMonomorphismRestriction #-}
module CompileLC where

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Pretty()
import Hschain.Utxo.Lang.Desugar.ExtendedLC
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Infer
import qualified Hschain.Utxo.Lang.Parser.Hask as P

import qualified Data.Text.IO as T

compileShow :: String -> IO ()
compileShow file = do
  eProg <- compileModule file
  either printRes printRes eProg
  where
    printRes = T.putStrLn . renderText


compileModule :: String -> IO (Either Error CoreProg)
compileModule file = do
  P.ParseOk m <- fmap (P.parseModule (Just "test2.hs")) $ readFile file
  return $ runInferM (toExtendedLC m)

