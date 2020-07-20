{-# Language NoMonomorphismRestriction #-}
module CompileLC where

import Control.Monad

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Compile
import Hschain.Utxo.Lang.Compile.Infer
import Hschain.Utxo.Lang.Compile.LambdaLifting
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Pretty()
import Hschain.Utxo.Lang.Desugar (altGroupToExpr, altGroupToTupleExpr)
import Hschain.Utxo.Lang.Desugar.Case
import Hschain.Utxo.Lang.Desugar.ExtendedLC
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Infer

import Text.Show.Pretty

import qualified Hschain.Utxo.Lang.Parser.Hask as P

import qualified Data.Text.IO as T

compileShow :: String -> IO ()
compileShow file = do
  eProg <- compileModule file
  either printRes printRes eProg

compileLiftShow :: String -> IO ()
compileLiftShow file = do
  eProg <- compileModule file
  either printRes printRes eProg
  startSection
  either printRes printRes (fmap lambdaLifting eProg)
  startSection
  either printRes (pPrint) (fmap lambdaLifting eProg)
  startSection
  either printRes (pPrint) (fmap (runInferM . annotateTypes . lambdaLifting) eProg)
  startSection
  m <- readModule file
  either printRes (pPrint) ((runInferM . compile) m)

printRes = T.putStrLn . renderText


compileModule :: String -> IO (Either Error LamProg)
compileModule file = do
  P.ParseOk m <- fmap (P.parseModule (Just "test2.hs")) $ readFile file
  return $ runInferM (toExtendedLC m)

readModule :: String -> IO Module
readModule file = do
  P.ParseOk m <- fmap (P.parseModule (Just "test2.hs")) $ readFile file
  return m

compileSingleSteps :: String -> IO ()
compileSingleSteps file = do
  P.ParseOk m <- fmap (P.parseModule (Just "test2.hs")) $ readFile file
  let defn = head $ module'binds m
  either printRes printRes $ runInferM (altGroupToTupleExpr $ bind'alts defn)
  startSection
  either printRes printRes $ runInferM (desugarCaseExpr (module'userTypes m) <=< altGroupToExpr $ bind'alts defn)


startSection = putStrLn "-----------------------"

