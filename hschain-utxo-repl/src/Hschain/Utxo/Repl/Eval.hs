-- | This module defines function to execute user expressions
-- and bindings.
module Hschain.Utxo.Repl.Eval(
    evalExpr
  , evalBind
  , evalUserType
  , parseExpr
  , parseBind
  , parseUserType
  , withTypeCheck
  , noTypeCheck
) where

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Maybe

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Exec.Module (checkUserTypeInCtx)
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Module
import Hschain.Utxo.Lang.UserType
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Compile (compile)
import Hschain.Utxo.Repl.Monad
import Hschain.Utxo.Lang.Exec    (runExec)
import Hschain.Utxo.Lang.Error   (Error(..), InternalError(..), unexpected)
import Hschain.Utxo.Lang.Core.RefEval
import Hschain.Utxo.Lang.Core.Compile.Expr (TermVal)
import Safe

import qualified Data.ByteString as BS
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Parser.Hask as P

noTypeCheck :: Lang -> (Lang -> Repl ()) -> Repl ()
noTypeCheck expr cont = cont expr

withTypeCheck :: Lang -> (Lang -> Repl ()) -> Repl ()
withTypeCheck expr cont = do
  eTy <- checkType expr
  case eTy of
    Right _  -> cont expr
    Left err -> do
      reportError err

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
        reportError err

evaluate :: InputEnv -> UserTypeCtx -> Lang -> Either Error (TermVal, T.Text)
evaluate env types expr = runExec $ do
  core <- compile main
  either (unexpected . renderText) pure $ evalProg env core
  where
    main = Module
      { module'loc       = noLoc
      , module'userTypes = types
      , module'binds     = Binds mempty $
          [ FunBind
                 { bind'name = "main"
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
  , inputEnv'inputs     = mempty
  , inputEnv'outputs    = mempty
  , inputEnv'dataInputs = mempty
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
evalBind :: Bind Lang -> Repl ()
evalBind bind = do
  closure <- gets replEnv'closure
  modify' $ \st -> st { replEnv'closure = trimClosure closure }
  case bindFirstRhs bind of
    Just lang -> withTypeCheck lang $ \_ -> do
      modify' $ \st -> st { replEnv'closure = replEnv'closure st S.|> bind }
      updateWords
    Nothing -> do
      let msg = InternalError $ Unexpected "No rhs expression"
      reportError msg
  where
    trimClosure cl =
      case S.viewr (insertClosure bind cl) of
        S.EmptyR    -> S.empty
        rest S.:> _ -> rest

bindFirstRhs :: Bind Lang -> Maybe Lang
bindFirstRhs = fmap altToExpr . \case
  FunBind _ alts -> headMay alts
  PatBind _ alt  -> Just alt

fromParseResult :: P.ParseResult a -> Either String a
fromParseResult = \case
  P.ParseOk a           -> Right a
  P.ParseFailed loc msg -> Left $ mconcat [T.unpack $ renderText loc, ": ", msg]

onRepl :: (Maybe FilePath -> a) -> a
onRepl f = f (Just "<repl>")

parseExpr :: String -> Either String ParseRes
parseExpr input = fmap ParseExpr $ fromParseResult $ onRepl P.parseExp input

parseBind :: String -> Either String ParseRes
parseBind input = fmap ParseBind $ fromParseResult $ onRepl P.parseBind input

evalUserType :: UserType -> Repl ()
evalUserType ut = do
  prevClosure <- fmap trimClosure $ gets replEnv'typeClosure
  definedValues <- fmap (Set.fromList . fmap (VarName noLoc) . getEnvWords) get
  case checkUserTypeInCtx definedValues (fromTypeClosure prevClosure) ut of
    Nothing  -> do
      modify' $ \st -> st { replEnv'typeClosure = insertTypeClosure ut prevClosure }
      updateWords
    Just err -> do
      reportError err
  where
    trimClosure closure = case S.viewr (insertTypeClosure ut closure) of
      S.EmptyR -> S.empty
      rest S.:> _ -> rest


parseUserType :: String -> Either String ParseRes
parseUserType input = fmap ParseUserType $ fromParseResult $ onRepl P.parseUserType input

