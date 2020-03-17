module Hschain.Utxo.Repl.Eval(
    evalExpr
  , evalBind
  , parseExpr
  , parseBind
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.IO.Class

import Data.Fix
import Data.String

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Lib.Base

import System.Console.Repline
import System.Console.Haskeline.MonadException
import System.Process
import Data.List (isPrefixOf)
import Data.Text (Text)

import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Repl.Monad

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Parser.Hask as P

getClosureExpr :: Lang -> Repl Lang
getClosureExpr expr = do
  closure <- fmap replEnv'closure get
  return $ closure expr

noTypeCheck :: Lang -> (Lang -> Repl ()) -> Repl ()
noTypeCheck expr cont = cont expr

withTypeCheck :: Lang -> (Lang -> Repl ()) -> Repl ()
withTypeCheck expr cont = do
  eTy <- checkType expr
  case eTy of
    Right _  -> cont expr
    Left err -> liftIO $ T.putStrLn $ renderText err

evalExpr :: Lang -> Repl ()
evalExpr lang = do
  closedExpr <- getClosureExpr lang
  withTypeCheck closedExpr $ \expr -> do
    tx    <- fmap replEnv'tx get
    ctx   <- getExecContext
    let res = runExec ctx (txArg'args tx) (env'height $ txArg'env tx) (txArg'inputs tx) (txArg'outputs tx) $ execLang expr
    liftIO $ case res of
      Right (expr, debugTxt) -> do
        T.putStrLn $ renderText expr
        when (not $ T.null debugTxt) $ T.putStrLn debugTxt
      Left err   -> T.putStrLn $ renderText err


evalBind :: VarName -> Lang -> Repl ()
evalBind var lang = do
  closure <- fmap replEnv'closure get
  let closedExpr = closure lang
  withTypeCheck closedExpr $ \expr -> do
      tx  <- fmap replEnv'tx get
      ctx <- getExecContext
      let res = runExec ctx (txArg'args tx) (env'height $ txArg'env tx) (txArg'inputs tx) (txArg'outputs tx) $ execLang expr
      case res of
        Right (expr, _) -> do
          modify' $ \st -> st { replEnv'closure = closure . (\next -> singleLet Nothing var expr next)
                              , replEnv'words   = varName'name var : replEnv'words st }
          return ()
        Left err   -> liftIO $ T.putStrLn $ renderText err

parseExpr :: String -> Either String ParseRes
parseExpr input = fromParseResult $ fmap ParseExpr $ P.parseExp (Just "<repl>") input

parseBind :: String -> Either String ParseRes
parseBind input =
  case P.parseBind input of
    P.ParseOk (var, expr) -> Right $ ParseBind var expr
    P.ParseFailed _ msg   -> Left msg

fromParseResult :: P.ParseResult a -> Either String a
fromParseResult = \case
  P.ParseOk a           -> Right a
  P.ParseFailed loc err -> Left $ toError loc err
  where
    toError _ msg = msg

