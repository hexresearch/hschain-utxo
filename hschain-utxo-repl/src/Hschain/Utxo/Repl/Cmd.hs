-- | This moule defines execution of special commands of the REPL
-- like type query or show help or load script etc.
--
-- commands start with colon @:@.
module Hschain.Utxo.Repl.Cmd(
    evalCmd
  , parseCmd
) where

import Hex.Common.Aeson

import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.String
import Data.Text.Prettyprint.Doc
import Hschain.Utxo.Repl.Monad
import Hschain.Utxo.Repl.Imports (ImportError(..))

import Safe

import System.Exit

import Hschain.Utxo.Lang

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Hschain.Utxo.Lang.Parser.Hask as P

import qualified Hschain.Utxo.Repl.Imports as I

-- | List of possible commands
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
load args = mapM_ loadScript $ getFiles args
  where
    getFiles str = headMay $ words str

loadScript :: FilePath -> Repl ()
loadScript file = do
  st <- get
  eRes <- I.load file $ replEnv'imports st
  case eRes of
    Right imp -> put $ st { replEnv'imports = imp }
    Left err  -> printErr err
  where
    printErr err = liftIO $
      case err of
        ImportTypeError tyErr    -> T.putStrLn $ renderText tyErr
        ImportParseError loc msg -> showParseErr loc msg
        ImportFileMissing missingFile   -> T.putStrLn $ mconcat ["File not found: ", fromString missingFile]

    showParseErr loc msg = T.putStrLn $ T.unlines
      [ mconcat ["Failed to load script ", T.pack file]
      , mconcat [(renderText loc), " Parsing exited with error: ", fromString msg]
      ]

resetEvalCtx :: Repl ()
resetEvalCtx = modify' $ \st ->
  st { replEnv'closure   = mempty
     , replEnv'words     = [] }

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
  mapM_ loadScript =<< getImportFiles
  mapM_ loadTx     =<< getTxFile

showType :: String -> Repl ()
showType str = case P.parseExp (Just "<repl>") str of
  P.ParseOk expr      -> do
    eTy <- checkType expr
    liftIO $ case eTy of
      Right ty -> T.putStrLn $ renderDoc $ pretty ty
      Left err -> T.putStrLn $ renderText err
  P.ParseFailed _ msg -> liftIO $ putStrLn msg


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

-- | Parses special command
parseCmd :: String -> Either String ParseRes
parseCmd input =
  case input of
    ':' : rest ->
      case words rest of
        cmd : args -> Right $ ParseCmd cmd (unwords args)
        _          -> err
    _            -> err
  where
    err = Left "No command found"


