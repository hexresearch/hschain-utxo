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

import Type.Type

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


evalExpr :: Lang -> Repl ()
evalExpr lang = do
  eTy <- checkType lang
  case eTy of
    Right _ -> do
      closure  <- fmap replEnv'closure get
      tx       <- fmap replEnv'tx get
      let res = runExec (txArg'proof tx) (txArg'args tx) (env'height $ txArg'env tx) (txArg'inputs tx) (txArg'outputs tx) $ execLang $ closure lang
      liftIO $ case res of
        Right (expr, debugTxt) -> do
          T.putStrLn $ renderText expr
          when (not $ T.null debugTxt) $ T.putStrLn debugTxt
        Left err   -> T.putStrLn $ renderText err
    Left err -> liftIO $ T.putStrLn $ renderText err

evalBind :: VarName -> Lang -> Repl ()
evalBind var lang = do
  eTy <- checkType lang
  case eTy of
    Right _ -> do
      closure <- fmap replEnv'closure get
      tx      <- fmap replEnv'tx get
      let res = runExec (txArg'proof tx) (txArg'args tx) (env'height $ txArg'env tx) (txArg'inputs tx) (txArg'outputs tx) $ execLang $ closure lang
      case res of
        Right (expr, _) -> do
          modify' $ \st -> st { replEnv'closure = (\next -> singleLet noLoc var expr next) . closure
                              , replEnv'words   = varName'name var : replEnv'words st }
          return ()
        Left err   -> liftIO $ T.putStrLn $ renderText err
    Left err -> liftIO $ T.putStrLn $ renderText err

parseExpr :: String -> Either String ParseRes
parseExpr input = fromParseResult $ fmap ParseExpr $ P.parseExp input

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




