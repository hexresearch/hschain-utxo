module Hschain.Utxo.App.Options(
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
  = RunWebNode
      { runWebNode'config  :: FilePath
      , runWebNode'genesis :: FilePath
      }
  | RunValidator
      { runValidator'config  :: FilePath
      , runValidator'genesis :: FilePath
      }

options :: Parser Options
options = subparser
  (  command "webnode"   webnodeParser
  <> command "validator" validatorParser
  )
  where
    webnodeParser   = configParser RunWebNode   "Run webnode"
    validatorParser = configParser RunValidator "Run validator"

    configParser cons msg = info parser (progDesc msg)
      where
        parser = cons
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

