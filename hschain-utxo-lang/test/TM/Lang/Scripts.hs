module TM.Lang.Scripts(
    tests
  , singleOwnerScript
  , mkCoffeeScript
  , mkAtomicSwap1
  , mkAtomicSwap2
  , mkFullGame
  , mkHalfGame
  , patBindScript
  , fullMixScript
  , halfMixScript
) where

import Data.ByteString (ByteString)
import Data.Int
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Error

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

tests :: TestTree
tests = testGroup "High level lang"
  [ testScript "Single owner script"    singleOwnerScript
  , testScript "Pay for cofee script"   mkCoffeeScript
  , testScript "Atomic swap 1"          mkAtomicSwap1
  , testScript "Atomic swap 2"          mkAtomicSwap2
  , testScript "XOR half-game script"   mkHalfGame
  , testScript "XOR full-game script"   mkFullGame
  , testScript "All sorts of pat-binds" (pure patBindScript)
  , testScript "ErgoMix full-script"    (pure fullMixScript)
  , testScript "ErgoMix half-script"    mkHalfMixScript
  ]

testScript :: String -> IO Module -> TestTree
testScript msg prog = testCase msg $ do
  mErr <- fmap checkModule prog
  case mErr of
    Just err -> assertFailure $ T.unpack $ renderText err
    Nothing  -> return ()

checkModule :: Module -> Maybe Error
checkModule = either Just (const Nothing) . toCoreScript

--------------------------------------------------------------
-- single ownership test

singleOwnerScript :: IO Module
singleOwnerScript = do
  pubKey <- fmap getPublicKey newSecret
  return $ [utxoModule| pk $(pubKey) |]

--------------------------------------------------------------
-- Pay for coffee scripts (payment with delay and refund)

mkCoffeeScript :: IO Module
mkCoffeeScript = do
  senderPk   <- fmap getPublicKey newSecret
  receiverPk <- fmap getPublicKey newSecret
  let spendHeight = 10
  return $ coffeeScript spendHeight senderPk receiverPk


coffeeScript :: Int64 -> PublicKey -> PublicKey -> Module
coffeeScript spendHeight senderPk receiverPk = [utxoModule|

    receiverScript = pk $(receiverPk) &&* ($(spendHeight) <*  getHeight)
    refundScript   = pk $(senderPk)   &&* ($(spendHeight) >=* getHeight)

    main = receiverScript ||* refundScript
|]

-----------------------------------------------------------------
-- atomic swap scripts

mkAtomicSwap1 :: IO Module
mkAtomicSwap1 = do
  (alicePk, bobPk) <- getAliceBobKeys
  return $ atomicSwap1 100 alicePk bobPk

mkAtomicSwap2 :: IO Module
mkAtomicSwap2 = do
  (alicePk, bobPk) <- getAliceBobKeys
  return $ atomicSwap2 100 "swap-hash" alicePk bobPk

getAliceBobKeys :: IO (PublicKey, PublicKey)
getAliceBobKeys = do
  alicePk <- fmap getPublicKey newSecret
  bobPk   <- fmap getPublicKey newSecret
  return (alicePk, bobPk)

atomicSwap1 :: Int64 -> PublicKey -> PublicKey -> Module
atomicSwap1 deadlineBob alicePubKey bobPubKey = [utxoModule|
  orSigma [ (getHeight >* $(deadlineBob)) &&* pk $(alicePubKey)
          , pk $(bobPubKey) &&* (sha256 getArgs ==* getBoxArgs getSelf)
          ]
|]

atomicSwap2 :: Int64 -> ByteString -> PublicKey -> PublicKey -> Module
atomicSwap2 deadlineAlice swapHash alicePubKey bobPubKey = [utxoModule|
  orSigma [ (getHeight >* $(deadlineAlice)) &&* pk $(bobPubKey)
          , andSigma [ pk $(alicePubKey)
                     , lengthBytes getArgs <* 33
                     , sha256 getArgs ==* $(swapHash)
                     ]
          ]
|]

-----------------------------------------------------------------
-- XOR-game scripts

mkFullGame :: IO Module
mkFullGame = do
  alicePk <- fmap getPublicKey newSecret
  return $ fullGameScript "1234"  alicePk

mkHalfGame :: IO Module
mkHalfGame = do
  let fullGameHash = "1234"
  return $ halfGameScript fullGameHash

halfGameScript :: ByteString -> Module
halfGameScript fullGameScriptHash = [utxoModule|

validBobInput b = (b == 0) || (b == 1)

main = andSigma
      [ toSigma (validBobInput bobGuess)
      , sha256 (getBoxScript out) ==* $(fullGameScriptHash)
      , (length getOutputs ==* 1) ||* (length getOutputs ==* 2)
      , bobDeadline >=* (getHeight + 30)
      , getBoxValue out >=* (2 * getBoxValue getSelf) ]
  where
    out = getOutput 0
    (bobGuess, bobDeadline) = getBoxArgs out
|]


fullGameScript :: ByteString -> PublicKey -> Module
fullGameScript commitmentHash alice = [utxoModule|

(s, a) = getArgs
(b, bobDeadline, bob) = getBoxArgs getSelf

main =  (pk bob &&* (getHeight >* bobDeadline))
    ||* (   (sha256 (appendBytes s (serialise (a :: Int))) ==* $(commitmentHash))
        &&* (   (pk $(alice) &&* (a ==* b))
            ||* (pk bob      &&* (a /=* b))
            )
        )
|]


-- | Inludes all sorts of pattern binds.
patBindScript :: Module
patBindScript = [utxoModule|

data User = User Text Int

(bs, pair) = getArgs
john = User "john" 10
User name _ = john

main =
  let (a, b) = pair
  in  appendBytes (appendBytes (sha256 bs) (serialise (a + b + c + age  + lengthText name))) d
  where
    (c, d) = getBoxArgs getSelf
    (User _ age) = john
|]


----------------------------------------------
-- ErgoMix scripts

fullMixScript :: Module
fullMixScript = [utxoModule|

main = pk d ||* dtuple c u d
  where
    (u, c, d) = getBoxArgs getSelf
|]

halfMixScript :: ByteString -> Module
halfMixScript fullScriptHash = [utxoModule|

main = fullMixTx &&* (bob ||* alice)
  where
    u = getArgs getSelf
    (u0, c0, d0) = getArgs (getOutput 0) :: (Bytes, Bytes, Bytes)
    (u1, c1, d1) = getArgs (getOutput 1) :: (Bytes, Bytes, Bytes)

    bob   =  toSigma ((u0 == u) && (u1 == u) && (c0 == d1) && (c1 == d0))
         &&* (dtuple u c0 d0 ||* dtuple u d0 c0)

    fullMixBox b = sha256 (getBoxScript b) == $(fullScriptHash)
    fullMixTx = toSigma (
                 (getBoxValue (getOutput 0) == getBoxValue getSelf)
              && (getBoxValue (getOutput 1) == getBoxValue getSelf)
              && fullMixBox (getOutput 0)
              && fullMixBox (getOutput 1))

    alice = pk u
|]

mkHalfMixScript :: IO Module
mkHalfMixScript =
  pure $ halfMixScript $ hashScript $ toCoreScriptUnsafe fullMixScript

