module Hschain.Utxo.Repl(
  runRepl
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.IO.Class

import Data.Fix

import Hschain.Utxo.Lang

import System.Console.Repline hiding (options)
import System.Console.Haskeline.MonadException
import System.Process
import Data.List (isPrefixOf)
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import Hschain.Utxo.Repl.Cmd
import Hschain.Utxo.Repl.Eval
import Hschain.Utxo.Repl.Monad

-- Evaluation : handle each line user inputs
eval :: String -> Repl ()
eval input = either (liftIO . putStrLn) evalInput $ parseInput input
  where

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

-- Tab Completion: return a completion for partial words entered
completer :: WordCompleter ReplM
completer n = do
  names <- fmap replEnv'words get
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
        , txArg'proof   = Nothing
        , txArg'args    = mempty
        , txArg'env     = Env 0
        }


