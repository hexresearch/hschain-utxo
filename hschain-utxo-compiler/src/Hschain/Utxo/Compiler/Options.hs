module Hschain.Utxo.Compiler.Options(
    Options(..)
  , optionParser
) where

import Options.Applicative

optionParser :: IO Options
optionParser = execParser opts
  where
    opts = info (options <**> helper)
       ( fullDesc
      <> progDesc "Utility to compile script to text"
      <> header "hschain-utxo-compiler - utility to compile scripts" )

data Options
  = Compile
      { compile'input  :: FilePath
      , compile'output :: Maybe FilePath
      }
  | GenPrivateKey
      { genPrivateKey'output :: Maybe FilePath
      }
  | GetPublicKey
      { getPublicKey'input  :: FilePath
      , getPublicKey'output :: Maybe FilePath
      }
  | SignSigma
      { signSigma'secret     :: FilePath
      , signSigma'input      :: FilePath
      , signSigma'output     :: Maybe FilePath
      }
  deriving (Show)

options :: Parser Options
options = subparser
  (  command "compile"    compileParser
  <> command "secret"     genPrivateKeyParser
  <> command "public-key" getPublicKeyParser
  <> command "sign"       signSigmaParser
  )
  where
    compileParser = info parser msg
      where
        parser = Compile
          <$> strOption
              (  metavar "INPUT_FILE_PATH"
              <> long "input"
              <> short 'i'
              <> help "input script")
          <*> optional (strOption
              (  metavar "OUTPUT_FILE_PATH"
              <> long "output"
              <> short 'o'
              <> help "output" ))
        msg = progDesc "Compile script"

    genPrivateKeyParser = info parser msg
      where
        parser = GenPrivateKey
          <$> optional (strOption
              (  metavar "OUTPUT_FILE_PATH"
              <> long "output"
              <> short 'o'
              <> help "output" ))
        msg = progDesc "Generate private key"

    getPublicKeyParser = info parser msg
      where
        parser =
          GetPublicKey
            <$> strOption
                (  metavar "INPUT_FILE_PATH"
                <> long "input"
                <> short 'i'
                <> help "input script")
            <*> optional (strOption
                (  metavar "OUTPUT_FILE_PATH"
                <> long "output"
                <> short 'o'
                <> help "output" ))

        msg = progDesc "Get public key from secret file"

    signSigmaParser = info parser msg
      where
        parser =
          SignSigma
            <$> strOption
                (  metavar "SECRET_FILE_PATH"
                <> long "secret"
                <> short 's'
                <> help "file with private key")
            <*> strOption
                (  metavar "INPUT_FILE_PATH"
                <> long "input"
                <> short 'i'
                <> help "input script")
            <*> optional (strOption
                (  metavar "OUTPUT_FILE_PATH"
                <> long "output"
                <> short 'o'
                <> help "output" ))

        msg = progDesc "Sign sigma expression"


