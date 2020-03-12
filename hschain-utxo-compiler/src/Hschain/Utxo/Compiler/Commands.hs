module Hschain.Utxo.Compiler.Commands(
    compile
  , genPrivateKey
  , getPublicKey
  , signSigma
) where

import Data.Aeson
import Data.Maybe
import Data.String

import Data.ByteString.Lazy (ByteString)
import Hschain.Utxo.Lang.Parser.Hask

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Exec
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
checkType = checkMainModule langTypeContext

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
  either (const failToReadInput) onSecret eSecret
  where
    onSecret secret = saveKey output $ Sigma.publicKeyToText $ Sigma.getPublicKey secret

    failToReadInput = failToRead "secret" input

    saveKey = maybe T.putStrLn T.writeFile

----------------------------------------
-- sign sigma expression

signSigma :: FilePath -> FilePath -> Maybe FilePath -> IO ()
signSigma secretFile input output = do
  secret <- readSecret
  expr   <- readInput
  case expr of
    ConstBool _     -> errorExpressionIsConst
    SigmaBool sigma -> do
      let env = Sigma.proofEnvFromKeys [Sigma.getKeyPair secret]
      signedSigma <- Sigma.newProof env sigma
      saveSigma signedSigma
  where
    readSecret :: IO Sigma.Secret
    readSecret = do
      file <- LB.readFile secretFile
      return $ either (const failToReadSecret) id $ S.deserialiseOrFail file

    readInput :: IO BoolExprResult
    readInput = do
      file <- LB.readFile input
      return $ fromMaybe failToReadExpression $ decode' file

    saveSigma = maybe C.putStrLn LB.writeFile output . encode

    failToReadSecret     = failToRead "secret"     secretFile
    failToReadExpression = failToRead "expression" input

    errorExpressionIsConst = error "Expression os consant boolean, not a sigma expression. Nothing to sign."


failToRead :: String -> FilePath -> a
failToRead item file = error $ mconcat ["Failed to read ", item, " from file: ", file]

