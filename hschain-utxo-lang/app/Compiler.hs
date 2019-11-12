module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Options.Applicative

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Parser.Parser

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C

data Options = Options
  { options'input  :: FilePath
  , options'output :: Maybe FilePath
  }
  deriving (Show)

options :: Parser Options
options = Options
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

main :: IO ()
main = do
  app =<< execParser opts
  where
    opts = info (options <**> helper)
       ( fullDesc
      <> progDesc "Utility to compile script to text"
      <> header "hschain-utxo-compiler - utility to compile scripts" )

app :: Options -> IO ()
app opt@Options{..} = do
  inputStr <- readFile options'input
  case compile inputStr of
    Right res -> procOutput res
    Left err -> putStrLn err
  where
    procOutput = maybe noOutput save options'output

    save= BS.writeFile
    noOutput = C.putStrLn

compile :: String -> Either String ByteString
compile res =
  fmap (encode . toScript . Expr) $ parseExpr res


