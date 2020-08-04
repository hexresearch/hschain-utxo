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

import Hschain.Utxo.Test.Client.Monad (App, logTest, printTest, testCase, testTitle, getTxSigma)
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
      aliceBox1 = user'box scene'alice
      bobBox1   = user'box scene'bob
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
      backAddr <- allocAddress wallet
      gameAddr <- allocAddress wallet
      preTx <- makeAliceTx game'amount aliceScript wallet box backAddr gameAddr Nothing
      eSigma <- getTxSigma preTx
      eProof <- liftIO $ fmap join $ mapM (\sigma -> newProof (getProofEnv wallet) sigma (getTxBytes preTx)) eSigma
      case eProof of
        Right proof -> do
          tx <- makeAliceTx (fromIntegral game'amount) aliceScript wallet box backAddr gameAddr (Just proof)
          eTx <- postTxDebug True "Alice posts half game script" tx
          case eTx of
            Right _txHash -> return $ Just (k, gameAddr, backAddr, s)
            Left err      -> do
              liftIO $ T.putStrLn err
              return Nothing
        Left err -> do
          liftIO $ T.putStrLn err
          return Nothing


    makeAliceTx amount script wallet inBox backAddr gameAddr mProof = do
      total <- fmap (fromMaybe 0) $ getBoxBalance inBox

      let gameBox = if (amount > total)
            then Nothing
            else Just $ Box
              { box'id     = gameAddr
              , box'value  = amount
              , box'script = mainScriptUnsafe script
              , box'args   = mempty
              }

      let restBox = if (total <= amount)
            then Nothing
            else Just $ Box
              { box'id     = backAddr
              , box'value  = total - amount
              , box'script = mainScriptUnsafe $ pk' $ getWalletPublicKey wallet
              , box'args   = mempty
              }
      return $ Tx
            { tx'inputs  = V.fromList [inBox]
            , tx'outputs = V.fromList $ catMaybes [gameBox, restBox]
            , tx'proof   = mProof
            , tx'args    = mempty
            }

    getBobScript guess wallet alicePublicHash scriptBox alicePubKey inBox = do
      gameAddr <- allocAddress wallet
      backAddr <- allocAddress wallet
      preTx <- makeBobTx gameAddr backAddr Nothing
      eSigma <- getTxSigma preTx
      eProof <- liftIO $ fmap join $ mapM (\sigma -> newProof (getProofEnv wallet) sigma (getTxBytes preTx)) eSigma
      case eProof of
        Right proof -> do
          tx <- makeBobTx gameAddr backAddr (Just proof)
          eTxHash <- postTxDebug True "Bob posts full game script" tx
          case eTxHash of
            Right _txHash -> return $ Just (gameAddr, backAddr)
            Left  err     -> do
                bobPostError err
                return Nothing
        Left err -> do
          bobPostError err
          return Nothing
      where
        bobPostError msg = liftIO $ T.putStrLn $ mconcat ["Bob posts full game error: ", msg]

        makeBobTx gameAddr backAddr mProof = do
          total <- fmap (fromMaybe 0) $ getBoxBalance inBox
          height <- M.getHeight
          return $ Tx
              { tx'inputs  = V.fromList [inBox, scriptBox]
              , tx'outputs = V.fromList $ catMaybes [gameBox total height, restBox total]
              , tx'proof   = mProof
              , tx'args    = mempty
              }
          where
            gameBox total height
              | total < game'amount = Nothing
              | otherwise           = Just $ Box
                  { box'id      = gameAddr
                  , box'value   = 2 * game'amount
                  , box'script  =  mainScriptUnsafe $ fullGameScript (bytes alicePublicHash) (text alicePubKey)
                  , box'args    = makeArgs height
                  }

            restBox total
              | total <= game'amount = Nothing
              | otherwise            = Just $ Box
                  { box'id     = backAddr
                  , box'value  = total - game'amount
                  , box'script = mainScriptUnsafe $ pk $ text $ publicKeyToText $ getWalletPublicKey wallet
                  , box'args   = mempty
                  }

            makeArgs height = intArgs [guess, height + 35] <> textArgs [publicKeyToText $ getWalletPublicKey wallet]

    triesToWin isSuccess name wallet gameBox aliceSecret aliceGuess = do
      winAddr <- allocAddress wallet
      tx <- winTx gameBox winAddr wallet aliceSecret aliceGuess
      _eSigma <- M.getTxSigma tx
      eTxHash <- postTxDebug isSuccess (winMsg name) tx
      return $ either (const False) (const True) eTxHash
      where
        winMsg str = mconcat [str, " tries to win."]

    winTx gameBox winAddr wallet aliceSecret aliceGuess =
      fmap (toTx . Just) $ getOwnerProofUnsafe wallet (toTx Nothing)
      where
        toTx mProof = Tx
            { tx'inputs  = V.fromList [gameBox]
            , tx'outputs = V.fromList [outBox]
            , tx'proof   = mProof
            , tx'args    = args
            }

        args = byteArgs [aliceSecret] <> intArgs [aliceGuess]

        outBox = Box
          { box'id      = winAddr
          , box'value   = 2 * game'amount
          , box'script  = mainScriptUnsafe $ pk $ text $ publicKeyToText $ getWalletPublicKey wallet
          , box'args    = mempty
          }

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


