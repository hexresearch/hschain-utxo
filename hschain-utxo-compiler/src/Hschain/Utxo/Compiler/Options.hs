-- | Parser of comand line arguments for compiler
module Hschain.Utxo.Compiler.Options(
    Options(..)
  , optionParser
) where

import Options.Applicative

-- | Read command line arguments.
optionParser :: IO Options
optionParser = execParser opts
  where
    opts = info (options <**> helper)
       ( fullDesc
      <> progDesc "Utility to compile script to text"
      <> header "hschain-utxo-compiler - utility to compile scripts" )

-- | Command line arguments
--
-- All commands save result to output file. If file is missing result
-- is dumped to stdout.
data Options
  = Compile
      { compile'input  :: FilePath          -- ^ file with script
      , compile'output :: Maybe FilePath    -- ^ file for compiled script
      }
  -- ^ compile script
  | GenPrivateKey
      { genPrivateKey'output :: Maybe FilePath  -- ^ file to write private key
      }
  -- ^ generate private key
  | GetPublicKey
      { getPublicKey'input  :: FilePath         -- ^ file with private key
      , getPublicKey'output :: Maybe FilePath   -- ^ file for public key
      }
  -- ^ convert private key to public key
  | SignSigma
      { signSigma'secret     :: FilePath        -- ^ file with private key
      , signSigma'input      :: FilePath        -- ^ file with sigma-expression
      , signSigma'output     :: Maybe FilePath  -- ^ file for signed sigma-expression (proof)
      }
  -- ^ sign sigma-expression
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


