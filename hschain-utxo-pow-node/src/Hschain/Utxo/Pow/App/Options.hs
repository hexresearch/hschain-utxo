module Hschain.Utxo.Pow.App.Options(
    Command(..)
  , readCommandOptions
) where

import Data.Word
import Options.Applicative

readCommandOptions :: IO Command
readCommandOptions = execParser $ info (helper <*> opts) fullDesc
  where
    opts = subparser
            (  command "node"   runNode
            <> command "genkey" genKey
            )
    runNode = info (runNodeOptions <**> helper)
                   (  fullDesc
                   <> progDesc "Run (mining) consensus node with web interface"
                   <> header "hschain-utxo-pow-node-app node - runs webnodes and (mining) consensus nodes" )
    genKey  = info (generateKeyOptions <**> helper)
                   (  fullDesc
                   <> progDesc "Generate a key for specific secret and height (utility)"
                   <> header "hschain-utxo-pow-node-app genkey - utility to generate keypair for secret and height")

data Command
  = RunNode
      { runnode'config         :: [FilePath]
      , runnode'nodeSecret     :: Maybe String
      , runnode'dbPath         :: FilePath
      }
  | GenerateKey
      { generatekey'nodeSecret :: String
      , generatekey'height     :: Word64
      }

generateKeyOptions :: Parser Command
generateKeyOptions = GenerateKey
          <$> strOption
                        (  metavar "NODE_SECRET_ENV_VAR"
                        <> long "secret-env-var"
                        <> short 's'
                        <> help "name of environment variable with the node's secret - enables mining process")
          <*> option (eitherReader readWord64)
                        (  metavar "HEIGHT"
                        <> long "height"
                        <> short 'h'
                        <> help "Height for which to generate key pair"
                        )
  where
    readWord64 s = case reads s of
                     ((i,""):_)
                       | (i :: Integer) < 0 -> Left "negative value is not a valid height"
                       | i > fromIntegral (maxBound :: Word64) -> Left "curiosity killed a cat"
                       | otherwise -> Right (fromIntegral i) :: Either String Word64
                     _ -> Left $ show s ++" is not a valid integer"

runNodeOptions :: Parser Command
runNodeOptions = RunNode
          <$> some (strOption
              (  metavar "CONFIG_FILE_PATH"
              <> long "config"
              <> short 'c'
              <> help "path to config"))
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

