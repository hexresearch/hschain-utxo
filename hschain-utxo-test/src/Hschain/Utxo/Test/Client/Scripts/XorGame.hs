module Hschain.Utxo.Test.Client.Scripts.XorGame where

import Prelude hiding ((<*))
import Hex.Common.Text

import Control.Monad
import Control.Timeout

import Data.Fix
import Data.Maybe
import Data.String
import Data.Text (Text)

import System.Random

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

import Hschain.Utxo.Test.Client.Wallet
import Hschain.Utxo.Test.Client.Scripts.Utils

import qualified Hschain.Utxo.API.Client as C

import qualified Crypto.Hash as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

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
  &&* (trace (unl ["DEBUG", showScript $ getBoxScript out, blake2b256 $ showScript $ getBoxScript out, fullGameScriptHash]) $ blake2b256 (showScript $ getBoxScript out) ==* fullGameScriptHash)
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

xorGame :: C.ClientSpec -> IO ()
xorGame client = do
  scene <- initUsers client
  res <- xorGameRound scene (Game (Guess 0 1) 1) client
  print $ res == Just (GameResult False True)

xorGameRound :: Scene -> Game -> C.ClientSpec -> IO (Maybe GameResult)
xorGameRound Scene{..} Game{..} client = do
  let alice     = user'wallet scene'alice
      bob       = user'wallet scene'bob
      aliceBox1 = user'box scene'alice
      bobBox1   = user'box scene'bob
  mAliceScript <- getAliceScript (guess'alice game'guess) alice aliceBox1
  fmap join $ forM mAliceScript $ \(alicePublicHash, scriptBox, aliceBox2, aliceSecret) -> do
    mBobScript <- getBobScript (guess'bob game'guess) bob alicePublicHash scriptBox (wallet'publicKey alice) bobBox1
    forM mBobScript $ \(gameBox, bobBox2) -> do
      aliceRes <- triesToWin "Alice" alice gameBox aliceSecret (guess'alice game'guess)
      bobRes   <- triesToWin "Bob"   bob   gameBox aliceSecret (guess'alice game'guess)
      return $ GameResult
        { gameResult'aliceWins = aliceRes
        , gameResult'bobWins   = bobRes
        }
  where
    getAliceScript guess wallet@Wallet{..} box = do
      (k, s) <- makeAliceSecret guess
      let fullScriptHash = blake256 $ showt $ toScript $ fullGameScript (text k) (text wallet'publicKey)
          aliceScript = halfGameScript $ text $ fullScriptHash
      (tx, scriptAddr, backAddr) <- makeAliceTx game'amount aliceScript wallet box
      eTx <- postTxDebug "Alice posts half game script" wallet'client tx
      return $ case eTx of
        Right txHash -> Just (k, scriptAddr, backAddr, s)
        Left _       -> Nothing

    makeAliceSecret guess = do
      s <- fmap fromString $ sequence $ replicate 64 randomIO
      let k = blake256 $ s <> showt guess
      return (k, s)

    makeAliceTx amount script wallet inBox = do
      backAddr <- allocAddress wallet
      gameAddr <- allocAddress wallet

      total <- fmap (fromMaybe 0) $ getBoxBalance wallet inBox

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
              , box'script = toScript $ pk $ text $ wallet'publicKey wallet
              , box'args   = mempty
              }
      let tx = Tx
            { tx'inputs  = V.fromList [inBox]
            , tx'outputs = V.fromList $ catMaybes [gameBox, restBox]
            , tx'proof   = getOwnerProof wallet
            , tx'args    = mempty
            }
      return (tx, gameAddr, backAddr)

    getBobScript guess wallet alicePublicHash scriptBox alicePubKey inBox = do
      gameAddr <- allocAddress wallet
      backAddr <- allocAddress wallet
      tx <- makeBobTx gameAddr backAddr
      eTxHash <- postTxDebug "Bob posts ful game script" (wallet'client wallet) tx
      return $ case eTxHash of
        Right txHash -> Just (gameAddr, backAddr)
        Left  _      -> Nothing
      where
        makeBobTx gameAddr backAddr = do
          total <- fmap (fromMaybe 0) $ getBoxBalance wallet inBox
          height <- fmap (either (const 0) fromInteger) $ C.call (wallet'client wallet) $ C.getHeight
          return $ Tx
              { tx'inputs  = V.fromList [inBox, scriptBox]
              , tx'outputs = V.fromList $ catMaybes [gameBox total height, restBox total]
              , tx'proof   = getOwnerProof wallet
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
                  , box'script = toScript $ pk $ text $ wallet'publicKey wallet
                  , box'args   = mempty
                  }

            makeArgs height = M.fromList
              [ (bobDeadlineField, PrimInt $ height + 35)
              , (bobGuessField,    PrimInt guess)
              , (bobPkField,       PrimString $ wallet'publicKey wallet)
              ]

    triesToWin name wallet gameBox aliceSecret aliceGuess = do
      winAddr <- allocAddress wallet
      let tx = winTx gameBox winAddr wallet aliceSecret aliceGuess
      eTxHash <- postTxDebug (winMsg name) (wallet'client wallet) tx
      return $ either (const False) (const True) eTxHash
      where
        winMsg str = mconcat [str, " tries to win."]

    winTx gameBox winAddr wallet aliceSecret aliceGuess = Tx
            { tx'inputs  = V.fromList [gameBox]
            , tx'outputs = V.fromList [outBox]
            , tx'proof   = getOwnerProof wallet
            , tx'args    = args
            }
      where
        args = M.fromList
          [ (sField, PrimString aliceSecret)
          , (aField, PrimInt    aliceGuess) ]

        outBox = Box
          { box'id      = winAddr
          , box'value   = 2 * game'amount
          , box'script  = toScript $ pk $ text $ wallet'publicKey wallet
          , box'args    = mempty
          }

    blake256 :: Text -> Text
    blake256 txt = showt $ C.hashWith C.Blake2b_256 $ T.encodeUtf8 txt

postTxDebug :: Text -> C.ClientSpec -> Tx -> IO (Either Text TxHash)
postTxDebug msg client tx = do
  T.putStrLn msg
  T.putStrLn "Going to post TX:"
  T.putStrLn $ renderText tx
  resp <- C.call client $ C.postTx tx
  mapM_ (print . postTxResponse'value) resp
  mapM_ (T.putStrLn . postTxResponse'debug) resp
  st <- C.call client C.getState
  T.putStrLn $  either (const mempty) renderText st
  wait
  return $ postTxResponse'value =<< resp
  where
    wait = sleep 0.25



