module Hschain.Utxo.Pow.App.Options(
    Options(..)
  , readOptions
) where

import Options.Applicative

readOptions :: IO Options
readOptions = execParser opts
  where
    opts = info (options <**> helper)
       ( fullDesc
      <> progDesc "Utility to run webnodes and validators"
      <> header "hschain-utxo - utility to run webnodes and validators" )

data Options
  = Options
      { options'config   :: FilePath
      , options'genesis  :: FilePath
      , options'nodeName :: String
      , options'mine     :: Bool
      , options'dbPath   :: FilePath
      }

options :: Parser Options
options = Options
          <$> strOption
              (  metavar "CONFIG_FILE_PATH"
              <> long "config"
              <> short 'c'
              <> help "path to config")
          <*> strOption
              (  metavar "GENESIS_FILE_PATH"
              <> long "genesis"
              <> short 'g'
              <> help "path to genesis")
          <*> strOption
              (  metavar "NODE_SECRET"
              <> long "secret"
              <> short 's'
              <> help "secret of the node")
          <*> switch
              (  long "mine"
              <> short 'm'
              <> help "enables mining alongside full node functionality"
              )
          <*> strOption
              (  metavar "DB_PATH"
              <> long "db"
              <> help "path to database files")

