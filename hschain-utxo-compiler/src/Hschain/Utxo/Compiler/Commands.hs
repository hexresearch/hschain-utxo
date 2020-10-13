-- | Sub-commands of the compiler
module Hschain.Utxo.Compiler.Commands(
    compile
  , genPrivateKey
  , getPublicKey
  , signSigma
) where

import Data.Aeson
import Data.Maybe

import Data.ByteString.Lazy (ByteString)
import Hschain.Utxo.Lang.Parser.Hask

import Hschain.Utxo.Lang.Core.Compile (coreProgToScript,typeCheck)
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Exec.Module
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Lib.Base

import qualified Hschain.Utxo.Lang.Sigma as Sigma

import qualified Codec.Serialise as S
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Hschain.Utxo.Lang.Compile as C

-- compile

-- | Compile smartcontracts.
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
      fromErr $ (fromParseError $ parseModule (Just input) res) >>= (\langModule ->
          case checkType langModule of
            Nothing  -> mkScript langModule
            Just err -> Left err
          )

    fromErr = either (Left . T.unpack . renderText) Right

    mkScript m = do
      coreProg <- runInferM $ C.compile m
      let script = (LB.fromStrict . unScript . coreProgToScript) coreProg
      case typeCheck coreProg of
        Right SigmaT -> Right script
        Right BoolT  -> Right script
        Right _      -> Left $ CoreScriptError ResultIsNotSigma
        Left  e      -> Left $ CoreScriptError $ TypeCoreError e

checkType :: Module -> Maybe Error
checkType = checkMainModule langTypeContext

----------------------------------------
-- generate secret

-- | Generate private key
genPrivateKey :: Maybe FilePath -> IO ()
genPrivateKey output = do
  secret <- Sigma.newSecret
  saveSecret output $ S.serialise secret
  where
    saveSecret = maybe C.putStrLn LB.writeFile

----------------------------------------
-- get public key

-- | Convert private key to public key
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

-- | Sign sigma-expression.
--
-- > signSigma secretFile exprFile txFile mOutputFile
--
-- It takes file with user private key and sigma-expression
-- and saves signed expression (proof) to the file, or if file not
-- specified dumps to stdout.
signSigma :: FilePath -> FilePath -> FilePath -> Maybe FilePath -> IO ()
signSigma secretFile exprFile txFile output = do
  secret <- readSecret
  expr   <- readExpr
  tx     <- readTx
  case expr of
    ConstBool _       -> errorExpressionIsConst
    SigmaResult sigma -> do
      let env = Sigma.proofEnvFromKeys [Sigma.getKeyPair secret]
      signedSigma <- Sigma.newProof env sigma (getSigMessageTx SigAll tx)
      saveSigma signedSigma
  where
    readSecret :: IO Sigma.Secret
    readSecret = do
      file <- LB.readFile secretFile
      return $ either (const failToReadSecret) id $ S.deserialiseOrFail file

    readExpr :: IO BoolExprResult
    readExpr = do
      file <- LB.readFile exprFile
      return $ fromMaybe failToReadExpression $ decode' file

    readTx :: IO Tx
    readTx = do
      file <- LB.readFile txFile
      return $ either (const failToReadTx) id $ S.deserialiseOrFail file

    saveSigma = maybe C.putStrLn LB.writeFile output . encode

    failToReadSecret     = failToRead "secret"     secretFile
    failToReadExpression = failToRead "expression" exprFile
    failToReadTx         = failToRead "tx"         txFile

    errorExpressionIsConst = error "Expression os consant boolean, not a sigma expression. Nothing to sign."


failToRead :: String -> FilePath -> a
failToRead item file = error $ mconcat ["Failed to read ", item, " from file: ", file]

