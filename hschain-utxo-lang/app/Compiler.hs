module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.String
import Data.Text (Text)
import Options.Applicative

import Type.Type
import Type.Pretty

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Lib.Base
import Hschain.Utxo.Lang.Parser.Hask

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
  case parseModule res of
    ParseOk lang ->
      case checkType lang of
        Nothing  -> fmap (encode . toScript . Expr) $ moduleToMainExpr lang
        Just err -> Left $ T.unpack $ renderText err
    ParseFailed _ err -> Left err

checkType :: Module -> Maybe TypeError
checkType lang =
  case runInferExpr baseTypeAssump =<< (either (Left . TypeError [noLoc] . pure . fromString) (Right . importBase) $ moduleToMainExpr lang) of
    Right _  -> Nothing
    Left err -> Just err


