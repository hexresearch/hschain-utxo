-- | Test script for XOR-game between two users.
module Hschain.Utxo.Test.Client.Scripts.XorGame where

import Prelude hiding ((<*))

import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Int
import Data.Maybe
import Data.String

import System.Random

import HSChain.Crypto (ByteRepr(..))
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

import Hschain.Utxo.Test.Client.Monad (App, testCase, testTitle, txIsValid)
import Hschain.Utxo.Test.Client.Wallet
import Hschain.Utxo.Test.Client.Scripts.Utils

import qualified Hschain.Utxo.Test.Client.Monad as M

import qualified Data.Text.IO as T
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Utils.Hash as H

halfGameScript :: ByteString -> Script
halfGameScript fullGameScriptHash = [utxo|

  out           = listAt getOutputs 0
  b             = fst (getBoxArgs out)
  bobDeadline   = snd (getBoxArgs out)
  validBobInput = b == 0 || b == 1

  main = toSigma (and
    [ validBobInput
    , sha256 (getBoxScript out) == $(fullGameScriptHash)
    , (length getOutputs == 1) || (length getOutputs == 2)
    , bobDeadline >= (getHeight + 30)
    , getBoxValue out >= (2 * getBoxValue getSelf)
    ])
|]

{-
halfGameScript' :: Expr ByteString -> Expr SigmaBool
halfGameScript' fullGameScriptHash =
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
-}

fullGameScript :: ByteString -> ByteString -> Script
fullGameScript k alice = [utxo|

  main =
    case (getArgs, getBoxArgs getSelf) of
      ((s, a), (b, bob, bobDeadline)) ->
        (pk bob &&* toSigma (getHeight > bobDeadline))
        ||* ((toSigma (sha256 (appendBytes s (serialise a)) == $(k)))
              &&* (   pk $(alice) &&* (toSigma (a == b))
                  ||* pk bob      &&* (toSigma (a /= b))))
|]

{-
fullGameScript' :: Expr ByteString -> Expr ByteString -> Expr SigmaBool
fullGameScript' k alice =
  "s"              =: getS                               $ \(s :: Expr ByteString) ->
  "a"              =: getA                               $ \(a :: Expr Int) ->
  "b"              =: getBobGuess getSelf                $ \(b :: Expr Int) ->
  "bob"            =: getBobPk getSelf                   $ \bob ->
  "bobDeadline"    =: getBobDeadline getSelf             $ \bobDeadline ->
      (pk bob &&* (toSigma $ getHeight >* bobDeadline))
  ||* ((toSigma (sha256 (s <> serialiseInt a) ==* k))
        &&* (     pk alice &&* (toSigma (a ==* b))
              ||* pk bob   &&* (toSigma (a /=* b ))))
-}

data Game = Game
  { game'guess   :: !Guess
  , game'amount  :: !Int64
  } deriving (Show, Eq)

data Guess = Guess
  { guess'alice   :: !Int64
  , guess'bob     :: !Int64
  } deriving (Show, Eq)

data GameResult = GameResult
  { gameResult'aliceWins :: !Bool
  , gameResult'bobWins   :: !Bool
  } deriving (Show, Eq)

isAliceWin :: Game -> Bool
isAliceWin Game{..} =  guess'alice game'guess == guess'bob game'guess

isBobWin :: Game -> Bool
isBobWin Game{..} =  guess'alice game'guess /= guess'bob game'guess

xorGame :: App ()
xorGame = do
  scene <- initUsers
  xorGameRound scene (Game (Guess 0 0) 1)

xorGameRound :: Scene -> Game -> App ()
xorGameRound Scene{..} game@Game{..} = do
  testTitle "XOR-game test."
  let alice     = user'wallet scene'alice
      bob       = user'wallet scene'bob
      Just aliceBox1 = user'box scene'alice
      Just bobBox1   = user'box scene'bob
  mAliceScript <- getAliceScript (guess'alice game'guess) alice aliceBox1
  res <- fmap join $ forM mAliceScript $ \(alicePublicHash, scriptBox, _aliceBox2, aliceSecret) -> do
    mBobScript <- getBobScript (fromIntegral $ guess'bob game'guess) bob alicePublicHash scriptBox (encodeToBS $ getWalletPublicKey alice) bobBox1
    forM mBobScript $ \(gameBox, _bobBox2) -> do
      aliceRes <- triesToWin (isAliceWin game) "Alice" alice gameBox aliceSecret (fromIntegral $ guess'alice game'guess)
      bobRes   <- triesToWin (isBobWin   game) "Bob"   bob   gameBox aliceSecret (fromIntegral $ guess'alice game'guess)
      return $ GameResult
        { gameResult'aliceWins = aliceRes
        , gameResult'bobWins   = bobRes
        }
  testCase "Game result is right" $ res == Just (GameResult (isAliceWin game) (isBobWin game))
  where
    getAliceScript guess wallet@Wallet{..} box = do
      (k, s) <- makeAliceSecret guess
      let fullScriptHash = hashScript $ fullGameScript k (encodeToBS $ getWalletPublicKey wallet)
          aliceScript = halfGameScript $ fullScriptHash
      (tx, backAddr, gameAddr) <- makeAliceTx game'amount aliceScript wallet box
      eTx <- postTxDebug True "Alice posts half game script" tx
      case eTx of
        Right _txHash -> return $ Just (k, gameAddr, backAddr, s)
        Left err      -> do
          liftIO $ T.putStrLn err
          return Nothing

    makeAliceTx amount script wallet inBox = do
      total <- fmap (fromMaybe 0) $ getBoxBalance inBox

      let alicePk = getWalletPublicKey wallet

      let gameBox = if (amount > total)
            then Nothing
            else Just Box
              { box'value  = amount
              , box'script = script
              , box'args   = mempty
              }

      let restBox
            | total <= amount = Nothing
            | otherwise       = Just Box
                { box'value  = total - amount
                , box'script = [utxo|pk $(alicePk)|]
                , box'args   = mempty
                }

          inputBox = BoxInputRef
            { boxInputRef'id      = inBox
            , boxInputRef'args    = mempty
            , boxInputRef'proof   = Just $ singleOwnerSigmaExpr wallet
            , boxInputRef'sigs    = mempty
            , boxInputRef'sigMask = SigAll
            }

          preTx = Tx
            { tx'inputs     = [inputBox]
            , tx'outputs    = V.fromList $ catMaybes [restBox, gameBox]
            , tx'dataInputs = []
            }

      fmap appendSenderReceiverIds $ newProofTx (getProofEnv wallet) preTx

    getBobScript guess wallet alicePublicHash scriptBox alicePubKey inBox = do
      (tx, backAddr, gameAddr) <- makeBobTx
      eTxHash <- postTxDebug True "Bob posts full game script" tx
      case eTxHash of
        Right _txHash -> return $ Just (gameAddr, backAddr)
        Left  err     -> do
            bobPostError err
            return Nothing
      where
        bobPostError msg = liftIO $ T.putStrLn $ mconcat ["Bob posts full game error: ", msg]

        makeBobTx = do
          total <- fmap (fromMaybe 0) $ getBoxBalance inBox
          height <- M.getHeight
          let preTx = Tx
                { tx'inputs  = V.fromList
                    [ BoxInputRef inBox mempty (Just $ singleOwnerSigmaExpr wallet) mempty SigAll
                    , BoxInputRef scriptBox mempty Nothing mempty SigAll
                    ]
                , tx'outputs = V.fromList $ catMaybes [gameBox total height, restBox total]
                , tx'dataInputs = []
                }
          tx <- newProofTx (getProofEnv wallet) preTx
          let (gameBoxId, mChangeBoxId) = extractOutputs tx
          return (tx, mChangeBoxId, gameBoxId)
          where
            gameBox total height
              | total < game'amount = Nothing
              | otherwise           = Just Box
                  { box'value   = 2 * game'amount
                  , box'script  = fullGameScript alicePublicHash alicePubKey
                  , box'args    = makeArgs height
                  }

            restBox total
              | total <= game'amount = Nothing
              | otherwise            = Just Box
                  { box'value  = total - game'amount
                  , box'script = mainScriptUnsafe $ pk $ bytes $ encodeToBS $ getWalletPublicKey wallet
                  , box'args   = mempty
                  }

            makeArgs height = toArgs @(Int64, ByteString, Int64) (guess, encodeToBS $ getWalletPublicKey wallet, height + 35)

            extractOutputs tx = case tx'outputs tx of
              [_receiver]          -> (toBoxId 0, Nothing)
              [_receiver, _sender] -> (toBoxId 0, Just $ toBoxId 1)
              _                    -> error "Not enough outputs for TX"
              where
                txId = computeTxId tx
                toBoxId = computeBoxId txId

    triesToWin isSuccess name wallet gameBox aliceSecret aliceGuess = do
      (tx, _) <- winTx gameBox wallet aliceSecret aliceGuess
      _eSigma <- M.getTxSigma tx
      eTxHash <- postTxDebug isSuccess (winMsg name) tx
      void $ txIsValid tx
      return $ either (const False) (const True) eTxHash
      where
        winMsg str = mconcat [str, " tries to win."]

    winTx gameBox wallet aliceSecret aliceGuess = do
      tx <- newProofTx (getProofEnv wallet) preTx
      return (tx, extractWinAddr tx)
      where
        preTx = Tx
            { tx'inputs     = V.fromList [BoxInputRef gameBox args (Just $ singleOwnerSigmaExpr wallet) mempty SigAll]
            , tx'outputs    = V.fromList [outBox]
            , tx'dataInputs = []
            }

        args = toArgs @(ByteString, Int64) (aliceSecret, aliceGuess)

        pubKey = getWalletPublicKey wallet

        outBox = Box
          { box'value   = 2 * game'amount
          , box'script  = [utxo|pk $(pubKey)|]
          , box'args    = mempty
          }

        extractWinAddr tx = computeBoxId (computeTxId tx) 0

makeAliceSecret :: MonadIO m => Int64 -> m (ByteString, ByteString)
makeAliceSecret guess = liftIO $ do
  s <- fmap fromString $ sequence $ replicate 64 randomIO
  let k = H.getSha256 $ s <> (serialiseTerm guess)
  return (k, s)

