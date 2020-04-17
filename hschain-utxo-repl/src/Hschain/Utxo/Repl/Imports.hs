module Hschain.Utxo.Repl.Imports(
    Imports(..)
  , ImportError(..)
  , load
  , reload
  , getLoadedFiles
) where

import Control.Monad.IO.Class

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Lib.Base

import Data.Default
import Data.Map.Strict (Map)
import Data.Text (Text)

import qualified Data.Map.Strict as M

import qualified Hschain.Utxo.Lang.Parser.Hask as P

data ImportError
  = ImportParseError P.SrcLoc String
  | ImportTypeError Error

data Imports = Imports
  { imports'base    :: !ModuleCtx
  , imports'current :: !ModuleCtx
  , imports'names   :: ![Text]
  , imports'loaded  :: !(Map FilePath ModuleCtx)
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

load :: MonadIO io => FilePath -> Imports -> io (Either ImportError Imports)
load file imp0 = liftIO $ do
  let imp = updateCurrent $ rmLoaded file imp0
  str <- readFile file
  case P.parseModule (Just file) str of
    P.ParseOk m -> do
      let typeCtx = inferCtx'binds $ moduleCtx'types $ imports'current imp
      case evalModule typeCtx m of
        Right modCtx   -> return $ Right $ loadCtx file modCtx imp
        Left typeError -> return $ Left $ ImportTypeError typeError
    P.ParseFailed loc err -> return $ Left $ ImportParseError loc err

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


loadBase :: Imports -> Imports
loadBase imp = imp { imports'base = baseModuleCtx }

updateCurrent :: Imports -> Imports
updateCurrent = updateCtx . updateNames
  where
    updateNames imp = imp { imports'names = getModuleCtxNames =<< allCtxs imp }

    updateCtx imp = imp { imports'current = mconcat $ allCtxs imp }

    allCtxs Imports{..} = imports'base : (M.elems imports'loaded)

getLoadedFiles :: Imports -> [FilePath]
getLoadedFiles = M.keys . imports'loaded
