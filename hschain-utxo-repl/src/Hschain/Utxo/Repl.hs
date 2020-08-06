-- | Life-cycle of the REPL
module Hschain.Utxo.Repl(
  runRepl
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict


import Hschain.Utxo.Lang

import System.Console.Repline hiding (options)
import System.Process

import qualified Data.Text as T

import Hschain.Utxo.Repl.Cmd
import Hschain.Utxo.Repl.Eval
import Hschain.Utxo.Repl.Monad

-- | Evaluation : handle each line user inputs
eval :: String -> Repl ()
eval input = either (liftIO . putStrLn) evalInput $ parseInput input
  where

-- | Evaluates user input.
evalInput :: ParseRes -> Repl ()
evalInput = \case
  ParseExpr expr     -> evalExpr expr
  ParseBind var expr -> evalBind var expr
  ParseCmd cmd args  -> evalCmd cmd args

parseInput :: String -> Either String ParseRes
parseInput input =
      parseCmd input
  <|> parseExpr input
  <|> parseBind input

-- | Tab Completion: return a completion for partial words entered
completer :: WordCompleter ReplM
completer n = do
  names <- fmap getEnvWords get
  return $ fmap T.unpack $ filter (T.isPrefixOf n') names
  where
    n' = T.pack n

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

say :: [String] -> Repl ()
say args = do
  _ <- liftIO $ system $ "cowsay" ++ " " ++ (unwords args)
  return ()

options :: [(String, [String] -> Repl ())]
options = [
    ("help", help)  -- :help
  , ("say", say)    -- :say
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to hschain-utxo-lang REPL!"

runRepl :: IO ()
runRepl = runReplM txArg $ evalRepl (pure " > ") eval options Nothing (Word completer) ini
  where
    txArg = TxArg
        { txArg'inputs  = mempty
        , txArg'outputs = mempty
        , txArg'env     = Env 0
        , txArg'txBytes = getTxContentBytes $ TxContent mempty mempty
        }

