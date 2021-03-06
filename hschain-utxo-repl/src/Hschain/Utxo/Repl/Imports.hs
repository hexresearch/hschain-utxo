-- | Module to handle imports to the REPL session.
module Hschain.Utxo.Repl.Imports(
    Imports(..)
  , ImportError(..)
  , load
  , reload
  , getLoadedFiles
) where

import Control.Exception.Base
import Control.Monad.IO.Class

import Hschain.Utxo.Lang.Module
import Hschain.Utxo.Lang.Lib.Base
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Exec (evalModule)

import Data.Default
import Data.Map.Strict (Map)
import Data.Text (Text)

import qualified Data.Map.Strict as M

import qualified Hschain.Utxo.Lang.Parser.Hask as P

-- | Errors that can gappen on import
data ImportError
  = ImportParseError P.SrcLoc String     -- ^ failed to parse the module
  | ImportTypeError Error                -- ^ module has type errors
  | ImportFileMissing FilePath           -- ^ no file found at the requested path

-- | Context that holds data of imported modules
data Imports = Imports
  { imports'base    :: !ModuleCtx
  -- ^ Module context of base library (Prelude)
  , imports'current :: !ModuleCtx
  -- ^ Module context for modules loaded by the user
  , imports'names   :: ![Text]
  -- ^ Names of the top-levek variables defined in the module
  , imports'loaded  :: !(Map FilePath ModuleCtx)
  -- ^ Module context of each individual loaded module
  }

instance Default Imports where
  def = updateCurrent $ loadBase $ Imports
          { imports'base = mempty
          , imports'current = mempty
          , imports'names = mempty
          , imports'loaded = mempty
          }

loadCtx :: FilePath -> ModuleCtx -> Imports -> Imports
loadCtx file ctx imp@Imports{..} = updateCurrent $ imp
  { imports'loaded = M.insert file ctx imports'loaded
  }

rmLoaded :: FilePath -> Imports -> Imports
rmLoaded file imp@Imports{..} = imp
  { imports'loaded = M.delete file imports'loaded
  }

-- | Load module to the session.
--
-- > load file context
--
-- It takes filepath to the module and current import context and returns
-- updated context or error.
load :: MonadIO io => FilePath -> Imports -> io (Either ImportError Imports)
load file imp0 = liftIO $ do
  let imp = updateCurrent $ rmLoaded file imp0
  eStr :: Either IOException String <- try $ readFile file
  case eStr of
    Right str ->
      case P.parseModule (Just file) str of
        P.ParseOk m -> do
          let typeCtx = inferCtx'binds $ moduleCtx'types $ imports'current imp
          case evalModule typeCtx m of
            Right modCtx -> return $ Right $ loadCtx file modCtx imp
            Left  tyErr  -> return $ Left $ ImportTypeError tyErr
        P.ParseFailed loc err -> return $ Left $ ImportParseError loc err
    Left _ -> return $ Left $ ImportFileMissing file

-- | Reloads all modules that were loaded in the REPL session
reload :: MonadIO io => Imports -> io (Either ImportError Imports)
reload x = go (getLoadedFiles x) x
  where
    go files imp = case files of
        []        -> return $ Right imp
        file:rest -> do
          eRes <- load file imp
          case eRes of
            Right res -> go rest res
            Left err  -> return $ Left err


-- | We do not need to include prelude definitions because
-- they are inlined during compilation.
loadBase :: Imports -> Imports
loadBase imp = imp { imports'base = ModuleCtx
  { moduleCtx'types = baseLibInferCtx
  , moduleCtx'exprs = mempty } }

updateCurrent :: Imports -> Imports
updateCurrent = updateCtx . updateNames
  where
    updateNames imp = imp { imports'names = getModuleCtxNames =<< allCtxs imp }

    updateCtx imp = imp { imports'current = mconcat $ allCtxs imp }

    allCtxs Imports{..} = imports'base : (M.elems imports'loaded)

-- | Return list of files for modules loaded in the REPL session
getLoadedFiles :: Imports -> [FilePath]
getLoadedFiles = M.keys . imports'loaded
