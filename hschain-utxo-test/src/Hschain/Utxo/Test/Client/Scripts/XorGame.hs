-- | Test script for XOR-game between two users.
module Hschain.Utxo.Test.Client.Scripts.XorGame where

import Prelude hiding ((<*))

import Control.Monad
import Control.Monad.IO.Class
import Control.Timeout

import Data.ByteString (ByteString)
import Data.Int
import Data.Maybe
import Data.String
import Data.Text (Text)

import System.Random

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

import Hschain.Utxo.Test.Client.Monad (App, logTest, printTest, testCase, testTitle)
import Hschain.Utxo.Test.Client.Wallet
import Hschain.Utxo.Test.Client.Scripts.Utils

import qualified Hschain.Utxo.Test.Client.Monad as M

import qualified Data.Text.IO as T
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Utils.ByteString as B

bobGuessFieldId, bobDeadlineFieldId, bobPkFieldId :: Expr Int

bobGuessFieldId = int 0
bobDeadlineFieldId = int 1
bobPkFieldId = int 0

getBobGuess :: Expr Box -> Expr Int
getBobGuess box = listAt (getBoxIntArgList box) bobGuessFieldId

getBobDeadline :: Expr Box -> Expr Int
getBobDeadline box = listAt (getBoxIntArgList box) bobDeadlineFieldId

getBobPk :: Expr Box -> Expr Text
getBobPk box = listAt (getBoxTextArgList box) bobPkFieldId

getS :: Expr ByteString
getS = listAt getBytesVars sFieldId

getA :: Expr Int
getA = listAt getIntVars aFieldId

sFieldId, aFieldId :: Expr Int
sFieldId = int 0
aFieldId = int 0

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

fullGameScript :: Expr ByteString -> Expr Text -> Expr SigmaBool
fullGameScript k alice =
  "s"              =: getS                               $ \(s :: Expr ByteString) ->
  "a"              =: getA                               $ \(a :: Expr Int) ->
  "b"              =: getBobGuess getSelf                $ \(b :: Expr Int) ->
  "bob"            =: getBobPk getSelf                   $ \bob ->
  "bobDeadline"    =: getBobDeadline getSelf             $ \bobDeadline ->
      (pk bob &&* (toSigma $ getHeight >* bobDeadline))
  ||* (toSigma (sha256 (s <> serialiseInt a) ==* k))
        &&* (     pk alice &&* (toSigma (a ==* b))
              ||* pk bob   &&* (toSigma (a /=* b )))

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
    mBobScript <- getBobScript (fromIntegral $ guess'bob game'guess) bob alicePublicHash scriptBox (publicKeyToText $ getWalletPublicKey alice) bobBox1
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
      let fullScriptHash = hashScript $ mainScriptUnsafe $ fullGameScript (bytes k) (text $ publicKeyToText $ getWalletPublicKey wallet)
          aliceScript = halfGameScript $ bytes $ fullScriptHash
      (tx, backAddr, gameAddr) <- makeAliceTx game'amount aliceScript wallet box
      eTx <- postTxDebug True "Alice posts half game script" tx
      case eTx of
        Right _txHash -> return $ Just (k, gameAddr, backAddr, s)
        Left err      -> do
          liftIO $ T.putStrLn err
          return Nothing

    makeAliceTx amount script wallet inBox = do
      total <- fmap (fromMaybe 0) $ getBoxBalance inBox

      let gameBox = if (amount > total)
            then Nothing
            else Just $ BoxContent
              { boxContent'value  = amount
              , boxContent'script = mainScriptUnsafe script
              , boxContent'args   = mempty
              }

      let restBox = if (total <= amount)
            then Nothing
            else Just $ BoxContent
              { boxContent'value  = total - amount
              , boxContent'script = mainScriptUnsafe $ pk' $ getWalletPublicKey wallet
              , boxContent'args   = mempty
              }

          inputBox = BoxInputRef
            { boxInputRef'id   = inBox
            , boxInputRef'args = mempty
            , boxInputRef'proof = Nothing
            }

          preTx = TxContent
            { txContent'inputs  = [ExpectedBox (Just $ singleOwnerSigmaExpr wallet) inputBox]
            , txContent'outputs = V.fromList $ catMaybes [restBox, gameBox]
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
          let preTx = TxContent
                { txContent'inputs  = V.fromList
                                        [ ExpectedBox (Just $ singleOwnerSigmaExpr wallet) (BoxInputRef inBox mempty Nothing)
                                        , ExpectedBox Nothing (BoxInputRef scriptBox mempty Nothing)
                                        ]
                , txContent'outputs = V.fromList $ catMaybes [gameBox total height, restBox total]
                }
          tx <- newProofTx (getProofEnv wallet) preTx
          let (gameBoxId, mChangeBoxId) = extractOutputs tx
          return (tx, mChangeBoxId, gameBoxId)
          where
            gameBox total height
              | total < game'amount = Nothing
              | otherwise           = Just $ BoxContent
                  { boxContent'value   = 2 * game'amount
                  , boxContent'script  = mainScriptUnsafe $ fullGameScript (bytes alicePublicHash) (text alicePubKey)
                  , boxContent'args    = makeArgs height
                  }

            restBox total
              | total <= game'amount = Nothing
              | otherwise            = Just $ BoxContent
                  { boxContent'value  = total - game'amount
                  , boxContent'script = mainScriptUnsafe $ pk $ text $ publicKeyToText $ getWalletPublicKey wallet
                  , boxContent'args   = mempty
                  }

            makeArgs height = intArgs [guess, height + 35] <> textArgs [publicKeyToText $ getWalletPublicKey wallet]

            extractOutputs tx = case tx'outputs tx of
              [receiver]         -> (box'id receiver, Nothing)
              [receiver, sender] -> (box'id receiver, Just $ box'id sender)
              _              -> error "Not enough outputs for TX"


    triesToWin isSuccess name wallet gameBox aliceSecret aliceGuess = do
      (tx, _) <- winTx gameBox wallet aliceSecret aliceGuess
      _eSigma <- M.getTxSigma tx
      eTxHash <- postTxDebug isSuccess (winMsg name) tx
      return $ either (const False) (const True) eTxHash
      where
        winMsg str = mconcat [str, " tries to win."]

    winTx gameBox wallet aliceSecret aliceGuess = do
      tx <- newProofTx (getProofEnv wallet) preTx
      return (tx, extractWinAddr tx)
      where
        preTx = TxContent
            { txContent'inputs  = V.fromList [ExpectedBox (Just $ singleOwnerSigmaExpr wallet) $ BoxInputRef gameBox args Nothing]
            , txContent'outputs = V.fromList [outBox]
            }

        args = byteArgs [aliceSecret] <> intArgs [aliceGuess]

        outBox = BoxContent
          { boxContent'value   = 2 * game'amount
          , boxContent'script  = mainScriptUnsafe $ pk $ text $ publicKeyToText $ getWalletPublicKey wallet
          , boxContent'args    = mempty
          }

        extractWinAddr tx = box'id $ V.head $ tx'outputs tx

makeAliceSecret :: MonadIO m => Int64 -> m (ByteString, ByteString)
makeAliceSecret guess = liftIO $ do
  s <- fmap fromString $ sequence $ replicate 64 randomIO
  let k = B.getSha256 $ s <> (B.serialiseInt guess)
  return (k, s)

postTxDebug :: Bool -> Text -> Tx -> App (Either Text TxHash)
postTxDebug isSuccess msg tx = do
  logTest msg
  logTest "Going to post TX:"
  logTest $ renderText tx
  resp <- M.postTx tx
  printTest $ postTxResponse'value resp
  -- logTest $ postTxResponse'debug resp
  st <- M.getState
  logTest $ renderText st
  wait
  testCase msg $ (isJust $ getTxHash resp) == isSuccess
  return $ maybe  (Left "Error postTxDebug") Right $ postTxResponse'value resp
  where
    wait = sleep 0.25

