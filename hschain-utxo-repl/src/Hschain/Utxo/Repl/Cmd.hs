module Hschain.Utxo.Repl.Cmd(
    evalCmd
  , parseCmd
) where

import Hex.Common.Aeson

import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.String
import Data.Text (Text)
import Hschain.Utxo.Repl.Eval
import Hschain.Utxo.Repl.Monad

import Safe

import System.Console.Repline
import System.Exit

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Lib.Base

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Hschain.Utxo.Lang.Parser.Parser as P

evalCmd :: String -> String -> Repl ()
evalCmd x args = case x of
  "q"      -> quit
  "quit"   -> quit
  "s"      -> set args
  "set"    -> set args
  "set-height" -> setHeight args
  "l"      -> load args
  "load"   -> load args
  "r"      -> reload
  "reload" -> reload
  "t"      -> showType args
  "type"   -> showType args
  "h"      -> help
  "help"   -> help
  other    -> uknownOption other

echo :: String -> Repl ()
echo = liftIO . putStrLn

quit :: Repl ()
quit = liftIO $ do
  putStrLn "Leaving Hschain REPL."
  exitSuccess

set :: String -> Repl ()
set _ = echo "set"

setHeight :: String -> Repl ()
setHeight arg =
  maybe showErr onHeight $ readMay arg
  where
    showErr = liftIO $ putStrLn $ mconcat ["Error: height should be integer, but got ", arg]

    onHeight n = modify' $ \st -> st { replEnv'tx = update (replEnv'tx st) }
      where
        update tx = tx { txArg'env = setEnv $ txArg'env tx }
        setEnv env = env { env'height = n }

load :: String -> Repl ()
load args = liftIO $ mapM_ loadScript $ getFiles args
  where
    getFiles str = headMay $ words str

    loadScript file =
      evalScript =<< readFile file

    evalScript = undefined

loadScript :: FilePath -> Repl ()
loadScript file = do
  resetEvalCtx
  saveScriptFile file
  str <- liftIO $ readFile file
  either showErr evalExpr $ P.parseExpr str
  where
    showErr msg = liftIO $ putStrLn $ unlines
      [ mconcat ["Failed to load script ", file]
      , "Parsing exited with error:"
      , msg
      ]

resetEvalCtx :: Repl ()
resetEvalCtx = modify' $ \st ->
  st { replEnv'closure   = id
     , replEnv'words     = baseNames }

saveScriptFile :: FilePath -> Repl ()
saveScriptFile file =
  modify' $ \st -> st { replEnv'scriptFile = Just file }

loadTx :: FilePath -> Repl ()
loadTx file = do
  saveTxFile file
  maybe showErr saveTx =<< (liftIO $ readJson file)
  where
    saveTx tx = modify' $ \st -> st { replEnv'tx = tx }

    showErr = liftIO $ putStrLn $ mconcat ["Failed to load tx-file ", file]

saveTxFile :: FilePath -> Repl ()
saveTxFile file =
  modify' $ \st -> st { replEnv'txFile = Just file }

reload :: Repl ()
reload = do
  resetEvalCtx
  mapM_ loadScript =<< getScriptFile
  mapM_ loadTx     =<< getTxFile

showType :: String -> Repl ()
showType str = case parseExpr str of
  Right (ParseExpr e) -> onExpr e
  _                   -> liftIO $ putStrLn "Error: not a valid expression"
  where
    onExpr expr = do
      closure  <- fmap ((. importBase) . replEnv'closure) get
      liftIO $ T.putStrLn $ case inferExpr mempty $ closure expr of
        Right scheme -> renderText scheme
        Left err     -> renderText err

help :: Repl ()
help = liftIO $ mapM_ putStrLn
  [ "Welcome to hschain REPL help."
  , "Available commands:"
  , ""
  , "  :set-height     - sets current height of blockchain"
  , "  :load file.hs   - loads script from file"
  , "  :load tx.json   - loads TX from JSON-file"
  , "  :reload         - reloads script and TX"
  , "  :quit           - quit the repl"
  , "  :type expr      - shows the type of expression"
  ]

uknownOption :: String -> Repl ()
uknownOption cmd = echo $ mconcat ["Error: Uknown command ", cmd, "."]

-----------------------------------------
-- parsing

parseCmd :: String -> Either Text ParseRes
parseCmd input =
  case input of
    ':' : rest ->
      case words rest of
        cmd : args -> Right $ ParseCmd cmd (unwords args)
        _          -> err
    _            -> err
  where
    err = Left "No command found"


