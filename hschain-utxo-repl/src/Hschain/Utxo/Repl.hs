-- | Life-cycle of the REPL
--
module Hschain.Utxo.Repl(
    runReplApp
  , eval
  , defaultTxArg
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Text (Text)

import Hschain.Utxo.Lang

import System.Console.Repline hiding (options)
import System.Process

import qualified Data.Text as T
import qualified Data.Text.IO as T
import HSChain.Crypto (hash)
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
  ParseBind bind     -> evalBind bind
  ParseUserType ut   -> evalUserType ut
  ParseCmd cmd args  -> evalCmd cmd args
  ParseErr loc err   -> showParseError loc err

showParseError :: Loc -> Text -> Repl ()
showParseError loc err = liftIO $ T.putStrLn $ renderText $ ParseError loc err

parseInput :: String -> Either String ParseRes
parseInput input =
      parseCmd input
  <|> parseExpr input
  <|> parseUserType input
  <|> parseBind input

-- | Tab Completion: return a completion for partial words entered
completer :: WordCompleter Repl
completer n = do
  names <- fmap getEnvWords get
  return $ fmap T.unpack $ filter (T.isPrefixOf n') names
  where
    n' = T.pack n

-- Commands
help :: [String] -> ReplM ()
help args = liftIO $ print $ "Help: " ++ show args

say :: [String] -> ReplM ()
say args = do
  _ <- liftIO $ system $ "cowsay" ++ " " ++ (unwords args)
  return ()

options :: [(String, [String] -> ReplM ())]
options = [
    ("help", help)  -- :help
  , ("say", say)    -- :say
  ]

ini :: ReplM ()
ini = liftIO $ putStrLn "Welcome to hschain-utxo-lang REPL!"

runReplApp :: IO ()
runReplApp = runRepl defaultTxArg $ evalRepl (pure " > ") (lift . eval) options Nothing (Word completer) ini
  where

defaultTxArg :: TxArg
defaultTxArg = TxArg
  { txArg'inputs     = mempty
  , txArg'outputs    = mempty
  , txArg'dataInputs = mempty
  , txArg'env        = Env 0
  , txArg'id         = TxId (hash ())
  }
