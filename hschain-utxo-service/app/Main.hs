module Main where

import Hex.Common.Delay

import Options.Applicative

import Hschain.Utxo.Back.App
import Hschain.Utxo.Back.Config

data Options = Options
  { configPath  :: FilePath
  , genesisPath :: FilePath
  }
  deriving (Show)

options :: Parser Options
options = Options
  <$> strOption
      (  metavar "CONFIG_PATH"
      <> help "Config path"
      <> long "config"
      <> value "./config/config.yaml"
      <> showDefault )
  <*> strOption
      (  metavar "GENESIS_PATH"
      <> help "Genesis path"
      <> long "genesis"
      <> value "./config/genesis.json"
      <> showDefault )

main :: IO ()
main = do
  app =<< execParser opts
  waitForever
  where
    opts = info (options <**> helper)
       ( fullDesc
      <> progDesc "Bots for stress test of xenochain"
      <> header "hschain-utxo-service - service to run utxo blockchain model" )

app :: Options -> IO ()
app Options{..} = do
  cfg <- loadConfig configPath
  mGenesis <- loadGenesis  genesisPath
  putStrLn $ mconcat ["Starts hschain-utxo server on port ", show $ serverConfig'port $ config'server cfg]
  case mGenesis of
    Just genesis -> runWebNode cfg genesis
    Nothing -> error "Failed to read genesis."

