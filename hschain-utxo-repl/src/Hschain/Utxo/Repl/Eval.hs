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

import qualified Hschain.Utxo.Lang.Parser.Parser as P


evalExpr :: Lang -> Repl ()
evalExpr lang = do
  closure  <- fmap replEnv'closure get
  tx       <- fmap replEnv'tx get
  let res = runExec (txArg'proof tx) (txArg'args tx) (env'height $ txArg'env tx) (txArg'inputs tx) (txArg'outputs tx) $ execLang $ closure lang
  liftIO $ case res of
    Right (expr, debugTxt) -> do
      T.putStrLn $ renderText expr
      when (not $ T.null debugTxt) $ T.putStrLn debugTxt
    Left err   -> T.putStrLn $ renderText err

evalBind :: VarName -> Lang -> Repl ()
evalBind varName lang = do
  closure <- fmap replEnv'closure get
  tx      <- fmap replEnv'tx get
  let res = runExec (txArg'proof tx) (txArg'args tx) (env'height $ txArg'env tx) (txArg'inputs tx) (txArg'outputs tx) $ execLang $ closure lang
  case res of
    Right (expr, _) -> do
      modify' $ \st -> st { replEnv'closure = (\next -> singleLet varName expr next) . closure
                          , replEnv'words   = varName : replEnv'words st }
      return ()
    Left err   -> liftIO $ T.putStrLn $ renderText err

parseExpr :: String -> Either Text ParseRes
parseExpr input = either (Left . fromString) (Right . ParseExpr) $ P.parseExpr input

parseBind :: String -> Either Text ParseRes
parseBind input =
  case P.parseExpr $ mconcat ["let ", input, " in undefined"] of
    -- Right (Fix (Let varName expr _)) -> Right $ ParseBind varName expr
    -- Right (Fix (LetArg varName args expr _)) -> Right $ ParseBind varName (Fix $ LamList (fmap (, Fix UknownType) args) expr)
    Left err -> Left $ fromString err




