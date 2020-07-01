{-# Language NoMonomorphismRestriction #-}
module CompileLC where

import Control.Monad

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Pretty()
import Hschain.Utxo.Lang.Desugar (altGroupToExpr)
import Hschain.Utxo.Lang.Desugar.Case
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

printRes = T.putStrLn . renderText


compileModule :: String -> IO (Either Error CoreProg)
compileModule file = do
  P.ParseOk m <- fmap (P.parseModule (Just "test2.hs")) $ readFile file
  return $ runInferM (toExtendedLC m)

compileSingleSteps :: String -> IO ()
compileSingleSteps file = do
  P.ParseOk m <- fmap (P.parseModule (Just "test2.hs")) $ readFile file
  let defn = head $ module'binds m
  either printRes printRes $ runInferM (altGroupToExpr $ bind'alts defn)
  putStrLn "-----------------------"
  either printRes printRes $ runInferM (desugarCaseExpr (module'userTypes m) <=< altGroupToExpr $ bind'alts defn)



