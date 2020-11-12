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

import Data.Default
import Data.Maybe

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Compile (compile)
import Hschain.Utxo.Repl.Monad
import Hschain.Utxo.Lang.Exec    (runExec)
import Hschain.Utxo.Lang.Error   (Error)
import Hschain.Utxo.Lang.Core.RefEval

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Parser.Hask as P
import qualified Language.Haskell.Exts.SrcLoc as P

noTypeCheck :: Lang -> (Lang -> Repl ()) -> Repl ()
noTypeCheck expr cont = cont expr

withTypeCheck :: Lang -> (Lang -> Repl ()) -> Repl ()
withTypeCheck expr cont = do
  eTy <- checkType expr
  case eTy of
    Right _  -> cont expr
    Left err -> do
      logError err
      liftIO $ T.putStrLn $ renderText err

-- | Evaluate user expression
evalExpr :: Lang -> Repl ()
evalExpr lang = do
  closedExpr <- getClosureExpr lang
  withTypeCheck closedExpr $ \expr -> do
    tx    <- fmap replEnv'tx get
    types <- getUserTypes
    let env = fromMaybe defaultInputEnv $ getInputEnv tx <$> txArg'inputs tx V.!? 0
    case evaluate env types expr of
      Right (res, debugTxt) -> liftIO $ do
        T.putStrLn $ renderText res
        when (not $ T.null debugTxt) $ T.putStrLn debugTxt
      Left err   -> do
        logError err
        liftIO $ T.putStrLn $ renderText err

evaluate :: InputEnv -> UserTypeCtx -> Lang -> Either Error (EvalResult, T.Text)
evaluate env types expr = runExec $ do
  core <- compile main
  return $ evalProg env core
  where
    main = Module
      { module'loc       = noLoc
      , module'userTypes = types
      , module'binds     =
          [ Bind { bind'name = "main"
                 , bind'type = Nothing
                 , bind'alts =
                     [ Alt { alt'pats  = []
                           , alt'expr  = UnguardedRhs expr
                           , alt'where = Nothing
                           }
                     ]
                 }
          ]
      }

defaultInputEnv :: InputEnv
defaultInputEnv = InputEnv
  { inputEnv'height  = 0
  , inputEnv'self    = BoxInput
    { boxInput'id      = bid
    , boxInput'box     = post self
    , boxInput'args    = mempty
    , boxInput'proof   = Nothing
    , boxInput'sigs    = mempty
    , boxInput'sigMask = SigAll
    , boxInput'sigMsg  = SigMessage $ hashBlob "SIGNME"
    }
  , inputEnv'inputs  = mempty
  , inputEnv'outputs = mempty
  , inputEnv'sigs    = mempty
  , inputEnv'args    = mempty
  , inputEnv'sigMsg  = def
  }
  where
    defHeight = 0
    post box = PostBox box defHeight

    bid  = BoxId $ hashBlob "default-input-box"

    self = Box
      { box'value  = 1
      , box'script = Script BS.empty
      , box'args   = mempty
      }

-- | Evaluate user bind. Bind is construct to assign name to variable
--
-- > var = expr
evalBind :: VarName -> Lang -> Repl ()
evalBind var lang = do
  closure <- fmap replEnv'closure get
  modify' $ \st -> st { replEnv'closure = tail $ insertClosure var lang closure }
  withTypeCheck lang $ \_ -> do
    modify' $ \st -> st { replEnv'closure = insertClosure var lang $ replEnv'closure st
                        , replEnv'words   = varName'name var : replEnv'words st
                        }

parseExpr :: String -> Either String ParseRes
parseExpr input = case P.parseExp (Just "<repl>") input of
  P.ParseOk a           -> Right $ ParseExpr a
  P.ParseFailed loc msg | msg == "Parse error: =" -> Left $ mconcat [T.unpack $ renderText loc, ": ", msg]
  P.ParseFailed loc msg -> Right $ ParseErr (P.toSrcInfo loc [] loc) (T.pack msg)

parseBind :: String -> Either String ParseRes
parseBind input =
  case P.parseBind (Just "<repl>") input of
    P.ParseOk (var, expr) -> Right $ ParseBind var expr
    P.ParseFailed loc msg   -> Left $ mconcat [T.unpack $ renderText loc, ": ", msg]

