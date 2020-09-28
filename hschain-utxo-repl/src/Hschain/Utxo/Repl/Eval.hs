-- | This module defines function to execute user expressions
-- and bindings.
module Hschain.Utxo.Repl.Eval(
    evalExpr
  , evalBind
  , parseExpr
  , parseBind
  , withTypeCheck
  , noTypeCheck
) where

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Maybe

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Repl.Monad

import qualified Data.ByteString as BS
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

-- | Evaluate user expression
evalExpr :: Lang -> Repl ()
evalExpr lang = do
  closedExpr <- getClosureExpr lang
  withTypeCheck closedExpr $ \expr -> do
    tx    <- fmap replEnv'tx get
    ctx   <- getExecContext
    types <- getUserTypes
    let env = fromMaybe defaultInputEnv $ fmap snd $ splitInputs tx V.!? 0
    let res = runExec ctx env $ execLang =<< desugar types expr
    liftIO $ case res of
      Right (e, debugTxt) -> do
        T.putStrLn $ renderText e
        when (not $ T.null debugTxt) $ T.putStrLn debugTxt
      Left err   -> T.putStrLn $ renderText err

defaultInputEnv :: InputEnv
defaultInputEnv = InputEnv
  { inputEnv'height  = 0
  , inputEnv'self    = self
  , inputEnv'inputs  = V.fromList [self]
  , inputEnv'outputs = V.fromList [self]
  , inputEnv'args    = mempty
  }
  where
    self = Box
      { box'id     = BoxId $ hashBlob "default-input-box"
      , box'value  = 1
      , box'script = Script BS.empty
      , box'args   = mempty
      }

-- | Evaluate user bind. Bind is construct to assign name to variable
--
-- > var = expr
evalBind :: VarName -> Lang -> Repl ()
evalBind var lang = do
  closure <- fmap replEnv'closure get
  let closedExpr = closure lang
  withTypeCheck closedExpr $ \expr -> do
      tx  <- fmap replEnv'tx get
      ctx <- getExecContext
      types <- getUserTypes
      let env = fromMaybe defaultInputEnv $ fmap snd $ splitInputs tx V.!? 0
      let res = runExec ctx env $ execLang =<< desugar types expr
      case res of
        Right (e, _) -> do
          modify' $ \st -> st { replEnv'closure = closure . (\next -> singleLet noLoc var e next)
                              , replEnv'words   = varName'name var : replEnv'words st }
          return ()
        Left err   -> liftIO $ T.putStrLn $ renderText err

parseExpr :: String -> Either String ParseRes
parseExpr input = fromParseResult $ fmap ParseExpr $ P.parseExp (Just "<repl>") input

parseBind :: String -> Either String ParseRes
parseBind input =
  case P.parseBind (Just "<repl>") input of
    P.ParseOk (var, expr) -> Right $ ParseBind var expr
    P.ParseFailed loc msg   -> Left $ mconcat [T.unpack $ renderText loc, ": ", msg]

fromParseResult :: P.ParseResult a -> Either String a
fromParseResult = \case
  P.ParseOk a             -> Right a
  P.ParseFailed loc msg   -> Left $ mconcat [T.unpack $ renderText loc, ": ", msg]

