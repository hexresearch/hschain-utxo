-- | REPL monad.
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

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Default
import Data.Either
import Data.Text (Text)

import System.Console.Repline
import System.Console.Haskeline.MonadException

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Repl.Imports

-- | Parse user input in the repl
data ParseRes
  = ParseExpr Lang           -- ^ user input is expression
  | ParseBind VarName Lang   -- ^ user input is binding value to a variable
  | ParseCmd  CmdName Arg    -- ^ user input is special command
  deriving (Show, Eq)

type CmdName = String
type Arg = String

-- | Context of REPL execution
data ReplEnv = ReplEnv
  { replEnv'tx             :: !TxArg
  -- ^ Arguments to execute transaction
  , replEnv'imports        :: Imports
  -- ^ modules imported to the REPL session
  , replEnv'closure        :: Lang -> Lang
  -- ^ Local variables or bindings
  -- defined by the user so far.
  , replEnv'words          :: ![Text]
  -- ^ Words for tab auto-completer
  , replEnv'txFile         :: Maybe FilePath
  -- ^ File with the transaction to execute script
  }


-- | REPL monad.
newtype ReplM a = ReplM { unReplM :: StateT ReplEnv IO a }
  deriving (Functor, Applicative, Monad, MonadState ReplEnv, MonadIO, MonadException)

type Repl a = HaskelineT ReplM a

-- | Run REPL monad
runReplM :: TxArg -> ReplM a -> IO a
runReplM tx (ReplM app) = evalStateT app defEnv
  where
    defEnv =
      ReplEnv
        { replEnv'tx            = tx
        , replEnv'imports       = def
        , replEnv'closure       = id
        , replEnv'words         = mempty
        , replEnv'txFile        = Nothing
        }

-- | Get context for type inference.
getInferCtx :: Repl InferCtx
getInferCtx =
  fmap (moduleCtx'types . imports'current . replEnv'imports) get

-- | Get definitions for user types
getUserTypes :: Repl UserTypeCtx
getUserTypes = fmap inferCtx'types getInferCtx

-- | Get type-context or bindings of local variables to signatures.
getTypeContext :: Repl TypeContext
getTypeContext =
  fmap (inferCtx'binds . moduleCtx'types . imports'current . replEnv'imports) get

-- | Get execution context
getExecContext :: Repl ExecCtx
getExecContext =
  fmap (moduleCtx'exprs . imports'current . replEnv'imports) get

-- | Get list of files that are loaded in the REPL session
getImportFiles :: Repl [FilePath]
getImportFiles = fmap (getLoadedFiles . replEnv'imports) get

-- | Get words for tab completion
getEnvWords :: ReplEnv -> [Text]
getEnvWords ReplEnv{..} = mappend replEnv'words (imports'names replEnv'imports)

-- | Get file that contains transaction for the current REPL session
-- to execute script in the context of transaction
getTxFile :: Repl (Maybe FilePath)
getTxFile = fmap replEnv'txFile get

-- | Check type of the expression
checkType :: Lang -> Repl (Either Error Type)
checkType expr = do
  ctx <- getInferCtx
  closure <- fmap replEnv'closure get
  return $ runInferM $ inferExpr ctx $ closure expr

-- | Check that expression has correct type
hasType :: Lang -> Repl Bool
hasType = fmap isRight . checkType

