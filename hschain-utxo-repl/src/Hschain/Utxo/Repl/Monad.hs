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
  , updateWords
  , insertClosure
  , closureToExpr
  , reportError
  , insertTypeClosure
  , fromTypeClosure
  , getReplTypeCtx
) where

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Default
import Data.Fix
import Data.Foldable
import Data.Either
import Data.Sequence (Seq)
import Data.Text (Text)

import System.Console.Repline
import System.Console.Haskeline.MonadException

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Module
import Hschain.Utxo.Lang.UserType
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Exec.Module (toUserTypeCtx)
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Repl.Imports
import Hschain.Utxo.Lang.Exec.Module (appendExecCtx)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text.IO as T

-- | Parse user input in the repl
data ParseRes
  = ParseExpr Lang           -- ^ user input is expression
  | ParseBind (Bind Lang)    -- ^ user input is binding value to a variable
  | ParseUserType UserType   -- ^ user type declaration
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
  , replEnv'closure        :: Seq (Bind Lang)
  -- ^ Local variables or bindings
  -- defined by the user so far.
  , replEnv'typeClosure    :: Seq UserType
  -- ^ Local user types defined in the module
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
        , replEnv'closure       = S.empty
        , replEnv'typeClosure   = S.empty
        , replEnv'words         = mempty
        , replEnv'txFile        = Nothing
        , replEnv'errors        = []
        }

getClosureExpr :: Lang -> Repl Lang
getClosureExpr expr = do
  closure <- fmap (closureToExpr . replEnv'closure) get
  ctx <- getExecCtx
  return $ appendExecCtx ctx $ closure expr

closureToExpr :: Seq (Bind Lang) -> Lang -> Lang
closureToExpr defs body =
  Fix $ Let noLoc (Binds mempty (toList defs)) body

insertClosure :: Bind Lang -> Seq (Bind Lang) -> Seq (Bind Lang)
insertClosure bind defs = (S.|> bind ) $
  case S.viewr post of
    S.EmptyR    -> pre
    rest S.:> _ -> rest
  where
    (pre, post) = S.breakr (not . Set.null . Set.intersection names . bindNamesLhs) defs
    names = bindNamesLhs bind

insertTypeClosure :: UserType -> Seq UserType -> Seq UserType
insertTypeClosure ut defs = (S.|> ut) $
  case S.viewr post of
    S.EmptyR    -> pre
    rest S.:> _ -> rest
  where
    (pre, post) = S.breakr ((== name) . userType'name) defs
    name = userType'name ut

fromTypeClosure :: Seq UserType -> UserTypeCtx
fromTypeClosure = toUserTypeCtx . toList

getReplTypeCtx :: Repl UserTypeCtx
getReplTypeCtx = gets (fromTypeClosure . replEnv'typeClosure)

-- | Get context for execution. Bindings defined in local moduules and base library.
getExecCtx :: Repl ExecCtx
getExecCtx =
  fmap (moduleCtx'exprs . imports'current . replEnv'imports) get

-- | Get context for type inference.
getInferCtx :: Repl InferCtx
getInferCtx = do
  inferCtx <- fmap (moduleCtx'types . imports'current . replEnv'imports) get
  replCtx  <- getReplTypeCtx
  return $ inferCtx <> userTypesToInferCtx replCtx

-- | Get definitions for user types
getUserTypes :: Repl UserTypeCtx
getUserTypes = do
  inferCtx <- fmap inferCtx'types getInferCtx
  replCtx  <- getReplTypeCtx
  return $ inferCtx <> replCtx

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

updateWords :: Repl ()
updateWords = do
  cl <- gets replEnv'closure
  tyCl <- gets replEnv'typeClosure
  modify' $ \st -> st { replEnv'words = closureWords cl <> typeClosureWords tyCl }
  where
    closureWords x = fmap varName'name $ bindNames =<< toList x
    typeClosureWords x = getUserTypeNames =<< toList x

    getUserTypeNames UserType{..} =
      varName'name userType'name : (fmap consName'name $ M.keys userType'cases)


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

reportError :: Error -> Repl ()
reportError err = do
  logError err
  liftIO $ T.putStrLn $ renderText err



