module Hschain.Utxo.Test.Client.Scripts.MultiSig(
  multiSigExchange
) where

import Hex.Common.Delay

import Control.Monad
import Control.Monad.IO.Class

import Data.Int
import Data.Text
import Data.Either.Extra
import Data.Maybe

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Test.Client.Wallet

import Hschain.Utxo.Test.Client.Monad
import Hschain.Utxo.Test.Client.Scripts.Utils
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

import qualified Hschain.Utxo.Lang.Sigma.Interpreter as Sigma
import qualified Hschain.Utxo.Lang.Sigma.MultiSig    as Sigma

import qualified Data.Vector as V

-- | Alice and Bob create joint box that is guarded by multisig and then spend it.
multiSigExchange :: App ()
multiSigExchange = do
  testTitle "Multisig exchange. Alice and Bob create shared box that is protected by both of their keys."
  Scene{..} <- initUsers
  let alice     = user'wallet scene'alice
      bob       = user'wallet scene'bob
      Just aliceBox1 = user'box scene'alice
      Just bobBox1   = user'box scene'bob
  (tx, commonBoxId) <- getSharedBoxTx alice bob (5, 5) (5, 5) aliceBox1 bobBox1
  void $ postTxDebug True "Alice and Bob post joint multisig TX" tx
  return ()

getSharedBoxTx :: Wallet -> Wallet -> (Int64, Int64) -> (Int64, Int64) -> BoxId -> BoxId -> App (Tx, BoxId)
getSharedBoxTx alice bob (aliceValue, aliceChange) (bobValue, bobChange) aliceBox bobBox = liftIO $ do
  aliceProof <- fmap eitherToMaybe $ newProof aliceEnv (singleOwnerSigmaExpr alice) txId
  bobProof   <- fmap eitherToMaybe $ newProof bobEnv   (singleOwnerSigmaExpr bob)   txId
  let preTx' = getPreTx aliceProof bobProof
  return $ appendCommonBoxId $ newTx preTx'
  where
    appendCommonBoxId tx = (tx, box'id $ tx'outputs tx V.! 0)

    preTx = getPreTx Nothing Nothing
    txId  = computePreTxId preTx

    getPreTx aliceProof bobProof = Tx
      { tx'inputs   = [inputBox aliceBox aliceProof, inputBox bobBox bobProof]
      , tx'outputs  = [commonBox, changeBox aliceChange alicePk, changeBox bobChange bobPk]
      }

    inputBox boxId proof = BoxInputRef
      { boxInputRef'id    = boxId
      , boxInputRef'args  = mempty
      , boxInputRef'proof = proof
      }

    commonBox = PreBox
      { preBox'value  = aliceValue + bobValue
      , preBox'script = mainScriptUnsafe $ pk' alicePk &&* pk' bobPk
      , preBox'args   = mempty
      }

    alicePk = getWalletPublicKey alice
    bobPk   = getWalletPublicKey bob

    aliceEnv = getProofEnv alice
    bobEnv   = getProofEnv bob


spendCommonBoxTx :: Wallet -> Wallet -> BoxId -> (Int64, Int64) -> IO (Tx, BoxId, BoxId)
spendCommonBoxTx alice bob commonBoxId (aliceValue, bobValue) = do
  _ <- Sigma.runProve $ do
    commitmentQueryExpr <- Sigma.initMultiSigProof knownKeys commonScript
    return ()
  return undefined
  where
    preTx = Tx
      { tx'inputs  = [commonInput]
      , tx'outputs = [aliceBox, bobBox]
      }

    txId  = computePreTxId preTx

    commonInput = BoxInputRef
      { boxInputRef'id    = commonBoxId
      , boxInputRef'args  = mempty
      , boxInputRef'proof = Nothing
      }

    commonScript = sigmaPk alicePk &&* sigmaPk bobPk

    aliceBox = changeBox aliceValue alicePk
    bobBox   = changeBox bobValue   bobPk

    alicePk  = getWalletPublicKey alice
    bobPk    = getWalletPublicKey bob

    knownKeys = [alicePk, bobPk]


postTxDebug :: Bool -> Text -> Tx -> App (Either Text TxHash)
postTxDebug isSuccess msg tx = do
  logTest msg
  logTest "Going to post TX:"
  logTest $ renderText tx
  resp <- postTx tx
  printTest $ postTxResponse'value resp
  st <- getState
  logTest $ renderText st
  wait
  testCase msg $ (isJust $ getTxHash resp) == isSuccess
  return $ maybe  (Left "Error postTxDebug") Right $ postTxResponse'value resp
  where
    wait = sleep 0.1


changeBox value pubKey = PreBox
  { preBox'value  = value
  , preBox'script = mainScriptUnsafe $ pk' pubKey
  , preBox'args   = mempty
  }
