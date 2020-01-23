module Hschain.Utxo.Test.Client.Scripts.XorGame where

import Prelude hiding ((<*))
import Hex.Common.Text

import Control.Monad
import Control.Monad.IO.Class
import Control.Timeout

import Data.Fix
import Data.Either
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

import qualified Crypto.Hash as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Text.Show.Pretty

bobGuessField, bobDeadlineField, bobPkField, sField, aField :: IsString a => a

bobGuessField = "other-guess"
bobDeadlineField = "other-deadline"
bobPkField = "other-pk"

sField = "s"
aField = "a"

halfGameScript :: Expr Text -> Expr Bool
halfGameScript fullGameScriptHash =
  "out"            =: getOutput 0                      $ \out ->
  "b"              =: getBoxArg out bobGuessField      $ \(b :: Expr Int) ->
  "bobDeadline"    =: getBoxArg out bobDeadlineField   $ \bobDeadline ->
  "validBobInput"  =: (b ==* 0 ||* b ==* 1)            $ \validBobInput ->
      validBobInput
  &&* (blake2b256 (showScript $ getBoxScript out) ==* fullGameScriptHash)
  &&* (lengthVec getOutputs ==* 1 ||* lengthVec getOutputs ==* 2)
  &&* (bobDeadline >=* getHeight + 30)
  &&* (getBoxValue out >=* 2 * getBoxValue getSelf )
  where
    unl :: [Expr Text] -> Expr Text
    unl xs = mconcat $ L.intersperse "," xs

fullGameScript :: Expr Text -> Expr Text -> Expr Bool
fullGameScript k alice =
  "s"              =: getVar sField                      $ \(s :: Expr Text) ->
  "a"              =: getVar aField                      $ \(a :: Expr Int) ->
  "b"              =: getBoxArg getSelf bobGuessField    $ \(b :: Expr Int) ->
  "bob"            =: getBoxArg getSelf bobPkField       $ \bob ->
  "bobDeadline"    =: getBoxArg getSelf bobDeadlineField $ \bobDeadline ->
      (pk bob &&* getHeight >* bobDeadline)
  ||* (     blake2b256 (s <> showInt a) ==* k
        &&* (     pk alice &&* a ==* b
              ||* pk bob   &&* a /=* b ))


data Game = Game
  { game'guess   :: !Guess
  , game'amount  :: !Money
  } deriving (Show, Eq)

data Guess = Guess
  { guess'alice   :: !Int
  , guess'bob     :: !Int
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
  res <- fmap join $ forM mAliceScript $ \(alicePublicHash, scriptBox, aliceBox2, aliceSecret) -> do
    mBobScript <- getBobScript (guess'bob game'guess) bob alicePublicHash scriptBox (publicKeyToText $ getWalletPublicKey alice) bobBox1
    forM mBobScript $ \(gameBox, bobBox2) -> do
      aliceRes <- triesToWin (isAliceWin game) "Alice" alice gameBox aliceSecret (guess'alice game'guess)
      bobRes   <- triesToWin (isBobWin   game) "Bob"   bob   gameBox aliceSecret (guess'alice game'guess)
      return $ GameResult
        { gameResult'aliceWins = aliceRes
        , gameResult'bobWins   = bobRes
        }
  testCase "Game result is right" $ res == Just (GameResult (isAliceWin game) (isBobWin game))
  where
    getAliceScript guess wallet@Wallet{..} box = do
      (k, s) <- makeAliceSecret guess
      let fullScriptHash = scriptBlake256 $ toScript $ fullGameScript (text k) (text $ publicKeyToText $ getWalletPublicKey wallet)
          aliceScript = halfGameScript $ text $ fullScriptHash
      backAddr <- allocAddress wallet
      gameAddr <- allocAddress wallet
      preTx <- makeAliceTx game'amount aliceScript wallet box backAddr gameAddr Nothing
      eSigma <- getTxSigma preTx
      eProof <- liftIO $ fmap join $ mapM (newProof (getProofEnv wallet)) eSigma
      case eProof of
        Right proof -> do
          tx <- makeAliceTx game'amount aliceScript wallet box backAddr gameAddr (Just proof)
          eTx <- postTxDebug True "Alice posts half game script" tx
          case eTx of
            Right txHash -> return $ Just (k, gameAddr, backAddr, s)
            Left err     -> do
              liftIO $ T.putStrLn err
              return Nothing
        Left err -> do
          liftIO $ T.putStrLn err
          return Nothing


    makeAliceSecret guess = liftIO $ do
      s <- fmap fromString $ sequence $ replicate 64 randomIO
      let k = blake256 $ s <> showt guess
      return (k, s)

    makeAliceTx amount script wallet inBox backAddr gameAddr mProof = do
      total <- fmap (fromMaybe 0) $ getBoxBalance inBox

      let gameBox = if (amount > total)
            then Nothing
            else Just $ Box
              { box'id     = gameAddr
              , box'value  = amount
              , box'script = toScript script
              , box'args   = mempty
              }

      let restBox = if (total <= amount)
            then Nothing
            else Just $ Box
              { box'id     = backAddr
              , box'value  = total - amount
              , box'script = toScript $ pk' $ getWalletPublicKey wallet
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
      eProof <- liftIO $ fmap join $ mapM (newProof (getProofEnv wallet)) eSigma
      case eProof of
        Right proof -> do
          tx <- makeBobTx gameAddr backAddr (Just proof)
          eTxHash <- postTxDebug True "Bob posts full game script" tx
          case eTxHash of
            Right txHash -> return $ Just (gameAddr, backAddr)
            Left  err    -> do
                bobPostError err
                return Nothing
        Left err -> do
          bobPostError err
          return Nothing
      where
        bobPostError msg = liftIO $ T.putStrLn $ mconcat ["Bob posts full game error: ", msg]

        makeBobTx gameAddr backAddr mProof = do
          total <- fmap (fromMaybe 0) $ getBoxBalance inBox
          height <- fmap fromInteger $ M.getHeight
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
                  , box'script  = toScript $ fullGameScript (text alicePublicHash) (text alicePubKey)
                  , box'args    = makeArgs height
                  }

            restBox total
              | total <= game'amount = Nothing
              | otherwise            = Just $ Box
                  { box'id     = backAddr
                  , box'value  = total - game'amount
                  , box'script = toScript $ pk $ text $ publicKeyToText $ getWalletPublicKey wallet
                  , box'args   = mempty
                  }

            makeArgs height = M.fromList
              [ (bobDeadlineField, PrimInt $ height + 35)
              , (bobGuessField,    PrimInt guess)
              , (bobPkField,       PrimString $ publicKeyToText $ getWalletPublicKey wallet)
              ]

    triesToWin isSuccess name wallet gameBox aliceSecret aliceGuess = do
      winAddr <- allocAddress wallet
      tx <- winTx gameBox winAddr wallet aliceSecret aliceGuess
      eTxHash <- postTxDebug isSuccess (winMsg name) tx
      return $ either (const False) (const True) eTxHash
      where
        winMsg str = mconcat [str, " tries to win."]

    winTx gameBox winAddr wallet aliceSecret aliceGuess = fmap (\proof -> Tx
            { tx'inputs  = V.fromList [gameBox]
            , tx'outputs = V.fromList [outBox]
            , tx'proof   = Just proof
            , tx'args    = args
            }) $ getOwnerProofUnsafe wallet
      where
        args = M.fromList
          [ (sField, PrimString aliceSecret)
          , (aField, PrimInt    aliceGuess) ]

        outBox = Box
          { box'id      = winAddr
          , box'value   = 2 * game'amount
          , box'script  = toScript $ pk $ text $ publicKeyToText $ getWalletPublicKey wallet
          , box'args    = mempty
          }

    blake256 :: Text -> Text
    blake256 txt = showt $ C.hashWith C.Blake2b_256 $ T.encodeUtf8 txt

    scriptBlake256 = hashScript C.Blake2b_256

postTxDebug :: Bool -> Text -> Tx -> App (Either Text TxHash)
postTxDebug isSuccess msg tx = do
  logTest msg
  logTest "Going to post TX:"
  logTest $ renderText tx
  resp <- M.postTx tx
  printTest $ postTxResponse'value resp
  logTest $ postTxResponse'debug resp
  st <- M.getState
  logTest $ renderText st
  wait
  testCase msg $ (isJust $ getTxHash resp) == isSuccess
  return $ postTxResponse'value resp
  where
    wait = sleep 0.25


