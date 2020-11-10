{-# LANGUAGE OverloadedLists            #-}
-- |
-- This is implementation of XorGame from Advanced ErgoScript Tutorial
-- (§3.1)
module TM.SmartCon.XorGame where

import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.Int
import Data.Fix
import Data.Boolean
import Data.String
import System.Random

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

import HSChain.Crypto (ByteRepr(..))
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma.Types (generateKeyPair, KeyPair(..))
import Hschain.Utxo.Lang

import qualified Hschain.Utxo.Lang.Sigma            as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Protocol   as Sigma
import qualified Hschain.Utxo.Lang.Utils.ByteString as B

import TM.BCH.Util

tests :: TestTree
tests = testGroup "XorGame"
  [ testCase "XorGame [Alice]" $ runMiner $ xorGame 0 0
  , testCase "XorGame [Bob]"   $ runMiner $ xorGame 0 1
  ]

xorGame :: Int64 -> Int64 -> Mine ()
xorGame aliceGuess bobGuess = do
  alice@KeyPair  {publicKey=pkAlice  } <- liftIO generateKeyPair
  bob@KeyPair    {publicKey=pkBob    } <- liftIO generateKeyPair
  let sigmaEnv = Sigma.Env [ alice, bob ]
  -- Alice and Bob mine some money
  bidAlice <- mineBlock (Just pkAlice) []
  bidBob   <- mineBlock (Just pkBob  ) []
  -- First Alice prepares both halves of scripts
  (commitmentHash, secret) <- makeAliceSecret aliceGuess
  let fullScript = fullGameScript (bytes commitmentHash) (bytes $ encodeToBS pkAlice)
      halfScript = halfGameScript $ bytes $ hashScript $ mainScriptUnsafe fullScript
  -- Alice posts half-game transaction
  txAlice <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidAlice pkAlice ]
    , tx'outputs = [
        Box { box'value  = 100
            , box'script = mainScriptUnsafe halfScript
            , box'args   = mempty
            }
        ]
    }
  let aliceGameBid = computeBoxId (computeTxId txAlice) 0
  _ <- mineBlock Nothing [ txAlice ]
  -- Bob posts full game script
  txBob <- newProofTx sigmaEnv $ Tx
    { tx'inputs  = [ simpleInputRef bidBob pkBob
                   , BoxInputRef { boxInputRef'id      = aliceGameBid
                                 , boxInputRef'args    = mempty
                                 , boxInputRef'proof   = Nothing
                                 , boxInputRef'sigs    = []
                                 , boxInputRef'sigMask = SigAll
                                 }
                   ]
    , tx'outputs = [ Box { box'value  = 200
                         , box'script = mainScriptUnsafe fullScript
                         , box'args   = intArgs [ bobGuess, 100 ]
                                     <> byteArgs [encodeToBS pkBob]
                         }
                   ]
    }
  let bobGameBid = computeBoxId (computeTxId txBob) 0
  _ <- mineBlock Nothing [ txBob ]
  -- Now check that correct player can spend TX
  let aliceWin = Tx
        { tx'inputs  = [
            BoxInputRef { boxInputRef'id      = bobGameBid
                        , boxInputRef'args    = byteArgs [secret] <> intArgs [aliceGuess]
                        , boxInputRef'proof   = Just $ Fix $ Sigma.SigmaPk pkAlice
                        , boxInputRef'sigs    = []
                        , boxInputRef'sigMask = SigAll
                        }
            ]
        , tx'outputs = [ burnBox 200 ]
        }
      bobWin = Tx
        { tx'inputs  = [
            BoxInputRef { boxInputRef'id      = bobGameBid
                        , boxInputRef'args    = byteArgs [secret] <> intArgs [aliceGuess]
                        , boxInputRef'proof   = Just $ Fix $ Sigma.SigmaPk pkBob
                        , boxInputRef'sigs    = []
                        , boxInputRef'sigMask = SigAll
                        }
            ]
        , tx'outputs = [ burnBox 200 ]
        }
  case aliceGuess == bobGuess of
    -- Alice wins
    True -> do
      badTx sigmaEnv bobWin
      tx <- newProofTx sigmaEnv aliceWin
      _  <- mineBlock Nothing [tx]
      pure ()
    -- Bob wins
    False -> do
      badTx sigmaEnv aliceWin
      tx <- newProofTx sigmaEnv bobWin
      _  <- mineBlock Nothing [tx]
      pure ()




makeAliceSecret :: MonadIO m => Int64 -> m (ByteString, ByteString)
makeAliceSecret guess = liftIO $ do
  s <- fmap fromString $ sequence $ replicate 64 randomIO
  let k = B.getSha256 $ s <> B.serialiseInt guess
  return (k, s)

----------------------------------------------------------------

halfGameScript :: Expr ByteString -> Expr SigmaBool
halfGameScript fullGameScriptHash =
  "out"            =: getOutput 0                      $ \out ->
  "b"              =: getBobGuess out                  $ \(b :: Expr Int) ->
  "bobDeadline"    =: getBobDeadline out               $ \bobDeadline ->
  "validBobInput"  =: (b ==* 0 ||* b ==* 1)            $ \validBobInput ->
      toSigma $
      validBobInput
  &&* ((sha256 $ getBoxScript out) ==* fullGameScriptHash)
  &&* (lengthVec getOutputs ==* 1 ||* lengthVec getOutputs ==* 2)
  &&* (bobDeadline >=* getHeight + 30)
  &&* (getBoxValue out >=* 2 * getBoxValue getSelf )

fullGameScript :: Expr ByteString -> Expr ByteString -> Expr SigmaBool
fullGameScript commitmentHash alice =
  "s"              =: listAt getBytesVars (int 0)        $ \(s :: Expr ByteString) ->
  "a"              =: listAt getIntVars   (int 0)        $ \(a :: Expr Int) ->
  "b"              =: getBobGuess getSelf                $ \(b :: Expr Int) ->
  "bob"            =: getBobPk getSelf                   $ \bob ->
  "bobDeadline"    =: getBobDeadline getSelf             $ \bobDeadline ->
      (pk bob &&* (toSigma $ getHeight >* bobDeadline))
  ||* (   toSigma (sha256 (s <> serialiseInt a) ==* commitmentHash)
      &&* (   (pk alice &&* toSigma (a ==* b))
          ||* (pk bob   &&* toSigma (a /=* b))
          )
      )


getBobGuess :: Expr Box -> Expr Int
getBobGuess box = listAt (getBoxIntArgList box) bobGuessFieldId

getBobDeadline :: Expr Box -> Expr Int
getBobDeadline box = listAt (getBoxIntArgList box) bobDeadlineFieldId

sFieldId, aFieldId :: Expr Int
sFieldId = int 0
aFieldId = int 0

getBobPk :: Expr Box -> Expr ByteString
getBobPk box = listAt (getBoxBytesArgList box) bobPkFieldId

bobGuessFieldId, bobDeadlineFieldId, bobPkFieldId :: Expr Int
bobGuessFieldId    = int 0
bobDeadlineFieldId = int 1
bobPkFieldId       = int 0
