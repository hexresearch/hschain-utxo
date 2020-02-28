module Hschain.Utxo.App(
  runApp
) where

import Hex.Common.Aeson
import Hex.Common.Yaml

import Control.Monad

import Data.Maybe

import Hschain.Utxo.Lang
import Hschain.Utxo.Back.App
import Hschain.Utxo.App.Options

runApp :: IO ()
runApp = app =<< readOptions

app :: Options -> IO ()
app = \case
  RunWebNode{..}    -> runBy runWebNode   runWebNode'config   runWebNode'genesis
  RunValidator{..}  -> runBy runValidator runValidator'config runValidator'genesis
  where
    runBy :: (FromJSON a, Show a) => (a -> [Tx] -> IO ()) -> FilePath -> FilePath -> IO ()
    runBy cmd configPath genesisPath =
      join $ cmd <$> readYaml configPath <*> readGenesis genesisPath

    readGenesis :: FilePath -> IO [Tx]
    readGenesis = fmap (fromMaybe err) . readJson
      where
        err = error "Error: failed to read genesis"


