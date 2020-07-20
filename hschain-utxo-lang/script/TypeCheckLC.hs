{-# Language NoMonomorphismRestriction #-}
module TypeCheckLC where

import Control.Monad

import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Compile.Infer
import Hschain.Utxo.Lang.Compile.Monomorphize
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Pretty()
import Hschain.Utxo.Lang.Desugar (altGroupToExpr, altGroupToTupleExpr)
import Hschain.Utxo.Lang.Desugar.Case
import Hschain.Utxo.Lang.Desugar.ExtendedLC
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Infer

import qualified Hschain.Utxo.Lang.Parser.Hask as P

import qualified Data.Text.IO as T
import Text.Show.Pretty

main = compileShow "../test2.hs"

compileShow :: String -> IO ()
compileShow file = do
  fileTxt <- readFile file
  let m = compileModule fileTxt
  pr (runInferM m)
--  putStrLn "----------------------"
--  pPrint (fmap unCoreProg $ runInferM m)
  putStrLn "----------------------"
  pr $ runInferM $ annotateTypes =<< m
  putStrLn "----------------------"
  pr $ runInferM $ specifyCompareOps =<< annotateTypes =<< m
  where
    pr eProg = either printRes printRes eProg

printRes :: Pretty a => a -> IO ()
printRes = T.putStrLn . renderText

compileModule :: MonadLang m => String -> m CoreProg
compileModule file = toExtendedLC m
  where
    P.ParseOk m = P.parseModule (Just "<test>") file

compileSingleSteps :: String -> IO ()
compileSingleSteps file = do
  P.ParseOk m <- fmap (P.parseModule (Just "test2.hs")) $ readFile file
  let defn = head $ module'binds m
  either printRes printRes $ runInferM (altGroupToTupleExpr $ bind'alts defn)
  putStrLn "-----------------------"
  either printRes printRes $ runInferM (desugarCaseExpr (module'userTypes m) <=< altGroupToExpr $ bind'alts defn)



