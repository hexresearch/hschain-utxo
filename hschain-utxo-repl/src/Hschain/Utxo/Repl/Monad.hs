-- | REPL monad.
module Hschain.Utxo.Repl.Monad(
    ReplEnv(..)
  , Repl(..)
  , runRepl
  , ReplM
  , ParseRes(..)
  , CmdName
  , Arg
  , getEnvWords
  , getImportFiles
  , getTxFile
  , getTypeContext
  , getUserTypes
  , checkType
  , hasType
  , getClosureExpr
  , insertClosure
  , closureToExpr
  , logError
) where

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Default
import Data.Either
import Data.Text (Text)

import System.Console.Repline
import System.Console.Haskeline.MonadException

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Desugar (singleLet)
import Hschain.Utxo.Repl.Imports
import Hschain.Utxo.Lang.Exec.Module (appendExecCtx)
import qualified Data.List as L

-- | Parse user input in the repl
data ParseRes
  = ParseExpr Lang           -- ^ user input is expression
  | ParseBind VarName Lang   -- ^ user input is binding value to a variable
  | ParseCmd  CmdName Arg    -- ^ user input is special command
  | ParseErr Loc Text
  deriving (Show, Eq)

type CmdName = String
type Arg = String

-- | Context of REPL execution
data ReplEnv = ReplEnv
  { replEnv'tx             :: !TxArg
  -- ^ Arguments to execute transaction
  , replEnv'imports        :: Imports
  -- ^ modules imported to the REPL session
  , replEnv'closure        :: [(VarName, Lang)]
  -- ^ Local variables or bindings
  -- defined by the user so far.
  , replEnv'words          :: ![Text]
  -- ^ Words for tab auto-completer
  , replEnv'txFile         :: Maybe FilePath
  -- ^ File with the transaction to execute script
  , replEnv'errors         :: [Error]
  }

-- | REPL monad.
newtype Repl a = Repl { unRepl :: StateT ReplEnv IO a }
  deriving (Functor, Applicative, Monad, MonadState ReplEnv, MonadIO, MonadException)

type ReplM a = HaskelineT Repl a

-- | Run REPL monad
runRepl :: TxArg -> Repl a -> IO a
runRepl tx (Repl app) = evalStateT app defEnv
  where
    defEnv =
      ReplEnv
        { replEnv'tx            = tx
        , replEnv'imports       = def
        , replEnv'closure       = []
        , replEnv'words         = mempty
        , replEnv'txFile        = Nothing
        , replEnv'errors        = []
        }

getClosureExpr :: Lang -> Repl Lang
getClosureExpr expr = do
  closure <- fmap (closureToExpr . replEnv'closure) get
  ctx <- getExecCtx
  return $ appendExecCtx ctx $ closure expr

closureToExpr :: [(VarName, Lang)] -> Lang -> Lang
closureToExpr defs body =
  foldr (\(name, dfn) res -> singleLet noLoc name dfn res) body (L.reverse defs)

insertClosure :: VarName -> Lang -> [(VarName, Lang)] -> [(VarName, Lang)]
insertClosure var lang defs = ((var, lang) : ) $
  case post of
    []     -> pre
    _:rest -> rest
  where
    (pre, post) = L.break ((== var) . fst) defs

-- | Get context for execution. Bindings defined in local moduules and base library.
getExecCtx :: Repl ExecCtx
getExecCtx =
  fmap (moduleCtx'exprs . imports'current . replEnv'imports) get

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
  closure <- fmap (closureToExpr . replEnv'closure) get
  return $ runInferM $ inferExpr ctx $ closure expr

-- | Check that expression has correct type
hasType :: Lang -> Repl Bool
hasType = fmap isRight . checkType

logError :: Error -> Repl ()
logError err = modify' $ \st -> st { replEnv'errors = err : replEnv'errors st }


