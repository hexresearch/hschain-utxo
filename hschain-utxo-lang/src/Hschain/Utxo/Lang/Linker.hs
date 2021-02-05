-- | Functions to load module dependencies and check them for recursive depenendencies.
module Hschain.Utxo.Lang.Linker(
    ImportDeps(..)
  , loadImports
  , evalImports
) where

import Control.Monad.Except

import Data.Bifunctor
import Data.Either.Extra
import Data.Map.Strict (Map)

import System.Directory
import System.FilePath

import Hschain.Utxo.Lang.Core.Types         (Name)
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Parser.Hask (parseModule)
import Hschain.Utxo.Lang.Module
import Hschain.Utxo.Lang.Exec.Module

import qualified Data.Graph as G
import qualified Data.List.Extra as L
import qualified Control.Monad.Extra as E
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Type.Check.HM as H

-- | All dependencies for main module.
data ImportDeps = ImportDeps
  { importDeps'modules :: Map Name Module   -- ^ loaded modules
  , importDeps'order   :: [Name]            -- ^ order by dependencies, from less deps to main
  }

-- | Context for dependencies
newtype DepCtx = DepCtx (Map Name ModuleCtx)

-- | Evaluate all dependnecies (imported modules)
evalImports :: ImportDeps -> Either Error DepCtx
evalImports ImportDeps{..} = fmap DepCtx $ foldM go M.empty importDeps'order
  where
    go res modName = do
      m    <- getModule modName
      ctx  <- loadTypeCtx res $ module'imports m
      mCtx <- evalModule ctx m
      return $ M.insert modName mCtx res

    getModule name =
      maybeToEither (ImportError $ ModuleNotFound $ VarName noLoc name) $ M.lookup name importDeps'modules

    loadTypeCtx depCtx imps = fmap mconcat $ mapM (importToTypeCtx $ DepCtx depCtx) imps

importToTypeCtx :: DepCtx -> Import -> Either Error TypeContext
importToTypeCtx (DepCtx depCtx) Import{..} = do
  ctx   <- getCtx
  let names = getImportNames ctx
  return $ getSignatures names ctx
  where
    getCtx = maybeToEither (ImportError $ ModuleNotFound import'name) $ M.lookup (varName'name import'name) depCtx

    getSignatures names ModuleCtx{..} =
      case inferCtx'binds moduleCtx'types of
        H.Context allBinds -> H.Context $ M.intersection allBinds (M.fromList $ fmap (, ()) names)

    getImportNames ModuleCtx{..} = case import'list of
      Nothing   -> exportBindNames
      Just imps ->
        let impBinds = importNames $ importList'items imps
        in  if importList'hides imps
              then exportBindNames L.\\ impBinds
              else L.intersect exportBindNames impBinds
      where
        allModuleBindNames = M.keys $ H.unContext $ inferCtx'binds moduleCtx'types

        exportBindNames = maybe allModuleBindNames (getExportBind =<<) moduleCtx'exports

        getExportBind = \case
          ExportVar v -> [varName'name v]
          _           -> []

        importNames impItems = getImportBind =<< impItems

        getImportBind = \case
          ImportVar v -> [varName'name v]
          _           -> []

-- | Reads imported modules and checks for import errors.
-- We check:
--
-- * there are no cyclic dependencies
-- * all module names from headers match names of the files
-- * reuqired modules are present as files
loadImports :: [FilePath] -> Module -> IO (Either Error ImportDeps)
loadImports path modules = do
  eMods <- readModules path modules
  return $ check =<< eMods
  where
    check mods = case checkModuleHeaders mods of
      Just err -> Left $ ImportError err
      Nothing  -> bimap ImportError (\names -> mods { importDeps'order = names }) $ checkCycleDeps mods

-- | Reads all import modules from files.
readModules :: [FilePath] -> Module -> IO (Either Error ImportDeps)
readModules path mainMod =
  runExceptT $ fmap (\m -> ImportDeps m []) $ go (M.singleton "Main" mainMod) [mainMod]
  where
    go :: Map Name Module -> [Module] -> ExceptT Error IO (Map Name Module)
    go res = \case
      []     -> pure res
      a:rest -> do
                  newModules <- mapM load $ filter (not . isImported) $ getModuleImportNames a
                  go (M.fromList newModules <> res) (fmap snd newModules <> rest)
      where
        isImported x = M.member (varName'name x) res

    load :: VarName -> ExceptT Error IO (Name, Module)
    load importName = do
      mFile <- liftIO $ findModuleFile importName
      case mFile of
        Nothing   -> throwError $ ImportError $ ModuleNotFound importName
        Just file -> do
          str <- liftIO $ readFile file
          m   <- liftEither $ fromParseError $ parseModule (Just file) str
          return (varName'name importName, m)

    findModuleFile name = E.findM doesPathExist $ fmap (flip toFileName name) path

    toFileName prefix imp =
      flip addExtension "hs" $ L.foldl1' (</>) $ (prefix : ) $ fmap T.unpack $ T.splitOn "." (varName'name imp)

-- | Check that there are no cyclic dependnencies.
-- Also returns order of dependencies
checkCycleDeps :: ImportDeps -> Either ImportError [Name]
checkCycleDeps (ImportDeps ms _) =
  case L.firstJust getCycle comps of
    Nothing -> Right $ fmap varName'name $ G.flattenSCCs comps
    Just vs -> Left $ CycleDependencies vs
  where
    comps :: [G.SCC VarName]
    comps = G.stronglyConnComp $ fmap toGraph $ (M.toList ms)

    getCycle = \case
      G.AcyclicSCC _ -> Nothing
      G.CyclicSCC vs -> Just vs

    toGraph (name, m) = (getModuleName m, name, fmap varName'name $ getModuleImportNames m)

getModuleImportNames :: Module -> [VarName]
getModuleImportNames Module{..} = fmap import'name module'imports

-- | Check that all names of modules declared in the code match the file names.
checkModuleHeaders :: ImportDeps -> Maybe ImportError
checkModuleHeaders (ImportDeps ms _) =
  L.firstJust (uncurry checkHeader) $ M.toList ms
  where
    checkHeader name m@Module{..}
      | name /= varName'name modName = Just $ ModuleNameNotMatchHeader name modName
      | otherwise                    = Nothing
      where
        modName = getModuleName m

