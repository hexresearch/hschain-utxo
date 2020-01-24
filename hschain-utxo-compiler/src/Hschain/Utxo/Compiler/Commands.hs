module Hschain.Utxo.Compiler.Commands(
    compile
  , genPrivateKey
  , getPublicKey
  , signSigma
) where

import Data.Aeson
import Data.String

import Data.ByteString.Lazy (ByteString)
import Hschain.Utxo.Lang.Parser.Hask

import Type.Type
import Type.Pretty
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Lib.Base
import Hschain.Utxo.Lang.Parser.Hask

import qualified Hschain.Utxo.Lang.Sigma as Sigma

import qualified Codec.Serialise as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- compile

compile :: FilePath -> Maybe FilePath -> IO ()
compile input output = do
  inputStr <- readFile input
  case go inputStr of
    Right res -> procOutput output res
    Left err -> putStrLn err
  where
    procOutput = maybe C.putStrLn LB.writeFile

    go :: String -> Either String ByteString
    go res =
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

----------------------------------------
-- generate secret

genPrivateKey :: Maybe FilePath -> IO ()
genPrivateKey output = do
  secret <- Sigma.newSecret
  saveSecret output $ S.serialise secret
  where
    saveSecret = maybe C.putStrLn LB.writeFile

----------------------------------------
-- get public key

getPublicKey :: FilePath -> Maybe FilePath -> IO ()
getPublicKey input output = do
  eSecret <- fmap S.deserialiseOrFail $ LB.readFile input
  either (const failToRead) onSecret eSecret
  where
    onSecret secret = saveKey output $ Sigma.publicKeyToText $ Sigma.getPublicKey secret

    failToRead = error $ mconcat ["Failed to read secret from file: ", input]

    saveKey = maybe T.putStrLn T.writeFile

----------------------------------------
-- sign sigma expression

signSigma :: FilePath -> FilePath -> Maybe FilePath -> IO ()
signSigma secretFile input output = undefined



