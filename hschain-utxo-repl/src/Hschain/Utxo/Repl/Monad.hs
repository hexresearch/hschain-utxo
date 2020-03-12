module Hschain.Utxo.Repl.Monad(
    ReplEnv(..)
  , ReplM(..)
  , runReplM
  , Repl
  , ParseRes(..)
  , CmdName
  , Arg
  , getScriptFile
  , getTxFile
  , checkType
  , hasType
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.IO.Class

import Data.Fix
import Data.Either

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Lib.Base

import System.Console.Repline
import System.Console.Haskeline.MonadException
import Data.Text (Text)

data ParseRes
  = ParseExpr Lang
  | ParseBind VarName Lang
  | ParseCmd  CmdName Arg

type CmdName = String
type Arg = String

data ReplEnv = ReplEnv
  { replEnv'tx          :: !TxArg
  , replEnv'closure     :: Lang -> Lang
  , replEnv'words       :: ![Text]
  , replEnv'scriptFile  :: Maybe FilePath
  , replEnv'txFile      :: Maybe FilePath
  }

newtype ReplM a = ReplM { unReplM :: StateT ReplEnv IO a }
  deriving (Functor, Applicative, Monad, MonadState ReplEnv, MonadIO, MonadException)

type Repl a = HaskelineT ReplM a


runReplM :: TxArg -> ReplM a -> IO a
runReplM tx (ReplM app) = evalStateT app defEnv
  where
    defEnv =
      ReplEnv
        { replEnv'tx          = tx
        , replEnv'closure     = importBase
        , replEnv'words       = baseNames
        , replEnv'scriptFile  = Nothing
        , replEnv'txFile      = Nothing
        }

getScriptFile :: Repl (Maybe FilePath)
getScriptFile = fmap replEnv'scriptFile get

getTxFile :: Repl (Maybe FilePath)
getTxFile = fmap replEnv'txFile get

checkType :: Lang -> Repl (Either TypeError Type)
checkType expr = do
  closure  <- fmap replEnv'closure get
  return $ inferExpr langTypeContext $ closure expr

hasType :: Lang -> Repl Bool
hasType = fmap isRight . checkType

