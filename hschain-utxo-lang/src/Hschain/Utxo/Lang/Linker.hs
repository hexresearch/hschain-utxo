-- | Functions to load module dependencies and check them for recursive depenendencies.
module Hschain.Utxo.Lang.Linker(
    ImportDeps(..)
  , loadImports
) where

import Control.Monad.Except

import Data.Bifunctor
import Data.Map.Strict (Map)
import Data.Set (Set)

import System.Directory
import System.FilePath

import Hschain.Utxo.Lang.Core.Types         (Name)
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Parser.Hask (parseModule)
import Hschain.Utxo.Lang.Module

import qualified Data.Graph as G
import qualified Data.List.Extra as L
import qualified Control.Monad.Extra as E
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

data ImportDeps = ImportDeps
  { importDeps'modules :: Map Name Module   -- ^ loaded modules
  , importDeps'order   :: [Name]            -- ^ order by dependencies, from less deps to main
  }

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
  runExceptT $ fmap (\m -> ImportDeps m []) $ go (M.singleton "Main" mainMod) (S.singleton "Main") [mainMod]
  where
    go :: Map Name Module -> Set Name -> [Module] -> ExceptT Error IO (Map Name Module)
    go res imported = \case
      []     -> pure res
      a:rest -> do
                  newModules <- mapM load $ filter (not . isImported) $ getModuleImportNames a
                  go (M.fromList newModules <> res) ((S.fromList $ fmap fst newModules) <> imported) (fmap snd newModules <> rest)
      where
        isImported x = S.member (varName'name x) imported

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

