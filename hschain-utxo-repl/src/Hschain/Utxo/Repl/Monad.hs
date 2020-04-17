module Hschain.Utxo.Repl.Monad(
    ReplEnv(..)
  , ReplM(..)
  , runReplM
  , Repl
  , ParseRes(..)
  , CmdName
  , Arg
  , getEnvWords
  , getImportFiles
  , getTxFile
  , getTypeContext
  , getExecContext
  , getUserTypes
  , checkType
  , hasType
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.IO.Class

import Data.Default
import Data.Fix
import Data.Either
import Data.Text (Text)

import System.Console.Repline
import System.Console.Haskeline.MonadException

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Lib.Base
import Hschain.Utxo.Repl.Imports

data ParseRes
  = ParseExpr Lang
  | ParseBind VarName Lang
  | ParseCmd  CmdName Arg

type CmdName = String
type Arg = String

data ReplEnv = ReplEnv
  { replEnv'tx             :: !TxArg
  , replEnv'imports        :: Imports
  , replEnv'closure        :: Lang -> Lang
  , replEnv'words          :: ![Text]
  , replEnv'txFile         :: Maybe FilePath
  }


newtype ReplM a = ReplM { unReplM :: StateT ReplEnv IO a }
  deriving (Functor, Applicative, Monad, MonadState ReplEnv, MonadIO, MonadException)

type Repl a = HaskelineT ReplM a

runReplM :: TxArg -> ReplM a -> IO a
runReplM tx (ReplM app) = evalStateT app defEnv
  where
    defEnv =
      ReplEnv
        { replEnv'tx            = tx
        , replEnv'imports       = def
        , replEnv'closure       = id
        , replEnv'words         = baseNames
        , replEnv'txFile        = Nothing
        }

getInferCtx :: Repl InferCtx
getInferCtx =
  fmap (moduleCtx'types . imports'current . replEnv'imports) get

getUserTypes :: Repl UserTypeCtx
getUserTypes = fmap inferCtx'types getInferCtx

getTypeContext :: Repl TypeContext
getTypeContext =
  fmap (inferCtx'binds . moduleCtx'types . imports'current . replEnv'imports) get

getExecContext :: Repl ExecCtx
getExecContext =
  fmap (moduleCtx'exprs . imports'current . replEnv'imports) get

getImportFiles :: Repl [FilePath]
getImportFiles = fmap (getLoadedFiles . replEnv'imports) get

getEnvWords :: ReplEnv -> [Text]
getEnvWords ReplEnv{..} = mappend replEnv'words (imports'names replEnv'imports)

getTxFile :: Repl (Maybe FilePath)
getTxFile = fmap replEnv'txFile get

checkType :: Lang -> Repl (Either Error Type)
checkType expr = do
  ctx <- getInferCtx
  closure <- fmap replEnv'closure get
  return $ runInferM $ inferExpr ctx $ closure expr

hasType :: Lang -> Repl Bool
hasType = fmap isRight . checkType

