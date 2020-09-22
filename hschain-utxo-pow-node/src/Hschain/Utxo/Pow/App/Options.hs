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
      { options'config     :: [FilePath]
      , options'genesis    :: FilePath
      , options'nodeSecret :: Maybe String
      , options'dbPath     :: FilePath
      }

options :: Parser Options
options = Options
          <$> some (strOption
              (  metavar "CONFIG_FILE_PATH"
              <> long "config"
              <> short 'c'
              <> help "path to config"))
          <*> strOption
              (  metavar "GENESIS_FILE_PATH"
              <> long "genesis"
              <> short 'g'
              <> help "path to genesis")
          <*> (Just <$> strOption
                        (  metavar "NODE_SECRET_ENV_VAR"
                        <> long "secret-env-var"
                        <> short 's'
                        <> help "name of environment variable with the node's secret - enables mining process")
              <|> pure Nothing)
          <*> strOption
              (  metavar "DB_PATH"
              <> long "db"
              <> help "path to database files")

