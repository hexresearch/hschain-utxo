-- | Alice and Bob create shared Box sealed with multi-sig:
--
-- > pkAlice && pkBob
--
-- and then spend it.
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

import qualified Data.Vector as V

-- | Alice and Bob create joint box that is guarded by multisig and then spend it.
multiSigExchange :: App ()
multiSigExchange = do
  testTitle "Multisig exchange. Alice and Bob create shared box that is protected by both of their keys."
  Scene{..} <- initUsers
  let alice     = user'wallet scene'alice
      bob       = user'wallet scene'bob
      john      = user'wallet scene'john
      Just aliceBox1 = user'box scene'alice
      Just bobBox1   = user'box scene'bob
  (tx, commonBoxId) <- getSharedBoxTx alice bob (5, 5) (5, 5) aliceBox1 bobBox1
  void $ postTxDebug True "Alice and Bob post joint multisig TX" tx
  (multiSigTx, aliceBox2, bobBox2) <- spendCommonBoxTx alice bob commonBoxId (aliceShareValue, bobShareValue)
  void $ postTxDebug True "Alice and bob create shared multi-sig proof and spend common box with it" multiSigTx
  let johnPubKey = getWalletPublicKey john
  simpleSpendTo "Alice is able to spends everything to John from her part of shared box"
      alice aliceBox2 johnPubKey aliceShareValue
  simpleSpendTo "Bob is able to spends everything to John from her part of shared box"
      bob bobBox2 johnPubKey bobShareValue
  return ()
  where
    aliceShareValue = 4
    bobShareValue   = 6


getSharedBoxTx :: Wallet -> Wallet -> (Int64, Int64) -> (Int64, Int64) -> BoxId -> BoxId -> App (Tx, BoxId)
getSharedBoxTx alice bob (aliceValue, aliceChange) (bobValue, bobChange) aliceBox bobBox = liftIO $ do
  aliceProof <- fmap eitherToMaybe $ newProof aliceEnv (singleOwnerSigmaExpr alice) message
  bobProof   <- fmap eitherToMaybe $ newProof bobEnv   (singleOwnerSigmaExpr bob)   message
  let preTx' = getPreTx aliceProof bobProof
  return $ appendCommonBoxId $ newTx preTx'
  where
    appendCommonBoxId tx = (tx, box'id $ tx'outputs tx V.! 0)

    preTx = getPreTx Nothing Nothing
    message = getSigMessagePreTx SigAll preTx

    getPreTx aliceProof bobProof = Tx
      { tx'inputs   = [inputBox aliceBox aliceProof, inputBox bobBox bobProof]
      , tx'outputs  = [commonBox, changeBox aliceChange alicePk, changeBox bobChange bobPk]
      }

    inputBox boxId proof = BoxInputRef
      { boxInputRef'id      = boxId
      , boxInputRef'args    = mempty
      , boxInputRef'proof   = proof
      , boxInputRef'sigMask = SigAll
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


spendCommonBoxTx :: Wallet -> Wallet -> BoxId -> (Int64, Int64) -> App (Tx, BoxId, BoxId)
spendCommonBoxTx alice bob commonBoxId (aliceValue, bobValue) = liftIO $ do
  proof <- fmap eitherToMaybe $ runProve $ do
    comQueryExpr <- initMultiSigProof knownKeys commonScript
    (aliceCommitments, aliceSecret) <- queryCommitments aliceKeys comQueryExpr
    (bobCommitments,   bobSecret)   <- queryCommitments bobKeys   comQueryExpr
    commitments <- appendCommitments [(aliceKeys, aliceCommitments), (bobKeys, bobCommitments)]
    challenges <- getChallenges commitments message
    aliceResponses <- queryResponses aliceEnv aliceSecret challenges
    bobResponses   <- queryResponses bobEnv   bobSecret   challenges
    proof <- appendResponsesToProof [(aliceKeys, aliceResponses), (bobKeys, bobResponses)]
    return proof
  return $ appendOutputs $ newTx $ getPreTx proof
  where
    appendOutputs tx = (tx, boxId 0, boxId 1)
      where
        boxId n = box'id $ tx'outputs tx V.! n

    getPreTx proof = Tx
      { tx'inputs  = [commonInput proof]
      , tx'outputs = [aliceBox, bobBox]
      }

    preTx = getPreTx Nothing

    message = getSigMessagePreTx SigAll preTx

    commonInput proof = BoxInputRef
      { boxInputRef'id      = commonBoxId
      , boxInputRef'args    = mempty
      , boxInputRef'proof   = proof
      , boxInputRef'sigMask = SigAll
      }

    commonScript = sigmaPk alicePk &&* sigmaPk bobPk

    aliceBox = changeBox aliceValue alicePk
    bobBox   = changeBox bobValue   bobPk

    alicePk  = getWalletPublicKey alice
    bobPk    = getWalletPublicKey bob

    knownKeys = [alicePk, bobPk]
    aliceKeys = [alicePk]
    bobKeys   = [bobPk]

    aliceEnv  = getProofEnv alice
    bobEnv    = getProofEnv bob


simpleSpendTo :: Text -> Wallet -> BoxId -> PublicKey -> Int64 -> App ()
simpleSpendTo message wallet fromId toPubKey value = do
  eTx <- simpleSpendToTx wallet fromId toPubKey value
  case eTx of
    Right tx -> void $ postTxDebug True message tx
    Left err -> testCase ("Failed to construct tx: " <> err) False

simpleSpendToTx :: Wallet -> BoxId -> PublicKey -> Int64 -> App (Either Text Tx)
simpleSpendToTx wallet fromId toPubKey value =
  newProofTxOrFail (getProofEnv wallet) preTx
  where
    preTx = Tx
      { tx'inputs  = [inputRef]
      , tx'outputs = [changeBox value toPubKey]
      }

    inputRef = BoxInputRef
      { boxInputRef'id    = fromId
      , boxInputRef'args  = mempty
      , boxInputRef'proof = Just $ singleOwnerSigmaExpr wallet
      , boxInputRef'sigMask = SigAll
      }

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

changeBox :: Int64 -> PublicKey -> PreBox
changeBox value pubKey = PreBox
  { preBox'value  = value
  , preBox'script = mainScriptUnsafe $ pk' pubKey
  , preBox'args   = mempty
  }
