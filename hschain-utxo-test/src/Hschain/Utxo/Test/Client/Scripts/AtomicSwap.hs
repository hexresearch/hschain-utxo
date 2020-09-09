module Hschain.Utxo.Test.Client.Scripts.AtomicSwap where

import Prelude hiding ((<*))
import Hex.Common.Delay

import Control.Concurrent.Lifted (fork)
import Control.Monad.IO.Class

import Data.ByteString (ByteString)

import System.Random

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Lang.Utils.ByteString(getSha256)

import Hschain.Utxo.Test.Client.Monad hiding (getHeight)
import Hschain.Utxo.Test.Client.Wallet (getWalletPublicKey, getProofEnv)
import Hschain.Utxo.Test.Client.Scripts.Utils

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

atomicSwap :: App ()
atomicSwap = do
  testTitle "Atomic swap: Alice exhanges with Bob."
  scene@Scene{..} <- initUsers
  let dealSpec = getDealSpec scene
  _aliceProcId <- fork $ startAliceProc dealSpec scene'alice
  _bobProcId   <- fork $ startBobProc dealSpec scene'bob
  sleep 1
  testCase "result is" False
  return ()

--wait :: App ()
-- wait = sleep 0.5

getDealSpec :: Scene -> SwapSpec
getDealSpec scene = SwapSpec
  { swapSpec'alice = SwapUser
      { swapUser'pk       = alicePk
      , swapUser'deadline = 10
      , swapUser'value    = 2
      }
  , swapSpec'bob = SwapUser
      { swapUser'pk       = bobPk
      , swapUser'deadline = 10
      , swapUser'value    = 2
      }
  , swapSpec'exchangeRate = 1
  }
  where
    alicePk = getPk scene'alice
    bobPk   = getPk scene'bob

    getPk extract = getWalletPublicKey $ user'wallet $ extract scene


type SwapSecret = ByteString
type SwapHash   = ByteString

data SwapSpec = SwapSpec
  { swapSpec'alice        :: !SwapUser
  , swapSpec'bob          :: !SwapUser
  , swapSpec'exchangeRate :: !Int
  }

data SwapUser = SwapUser
  { swapUser'pk        :: PublicKey
  , swapUser'deadline  :: Int
  , swapUser'value     :: Int
  }

initSecret :: Int -> IO (SwapSecret, SwapHash)
initSecret size = fmap (appendHash . B.pack) $ mapM (const randomIO) [1 .. size]
  where
    appendHash secret = (secret, getSha256 secret)

aliceInitSwapScript :: Int -> PublicKey -> PublicKey -> Script
aliceInitSwapScript deadlineBob alicePubKey bobPubKey = mainScriptUnsafe $
  orSigma $ fromVec
    [ toSigma (getHeight >* int deadlineBob) &&* pk' alicePubKey
    , pk' bobPubKey &&* (toSigma $ (sha256 $ listAt getBytesVars 0) ==* (listAt (getBoxBytesArgList getSelf) 0))
    ]

bobInitSwapScript :: SwapHash -> Int -> PublicKey -> PublicKey -> Script
bobInitSwapScript swapHash deadlineAlice alicePubKey bobPubKey = mainScriptUnsafe $
  orSigma $ fromVec
    [ toSigma (getHeight >* int deadlineAlice) &&* pk' bobPubKey
    , andSigma $ fromVec
        [ pk' alicePubKey
        , toSigma $ lengthBytes (listAt getBytesVars 0) <* 33
        , toSigma $ sha256 (listAt getBytesVars 0) ==* bytes swapHash
        ]
    ]

aliceInitSwapTx :: ProofEnv -> BoxId -> SwapHash -> SwapSpec -> App Tx
aliceInitSwapTx aliceKeys inputId swapHash spec = do
  Just totalValue <- getBoxBalance inputId
  newProofTx aliceKeys $ preTx totalValue
  where
    preTx totalValue = PreTx
      { preTx'inputs  = singleOwnerInput inputId alicePubKey
      , preTx'outputs = [ swapBox, changeBox totalValue ]
      }

    alicePubKey = swapUser'pk $ swapSpec'alice spec
    bobPubKey   = swapUser'pk $ swapSpec'bob spec
    deadlineBob = swapUser'deadline $ swapSpec'bob spec
    bobValue    = swapUser'value $ swapSpec'bob spec

    changeBox totalValue = PreBox
      { preBox'value  = totalValue - fromIntegral bobValue
      , preBox'script = singleOwnerScript alicePubKey
      , preBox'args   = mempty
      }

    swapBox = PreBox
      { preBox'value  = fromIntegral bobValue
      , preBox'script = aliceInitSwapScript deadlineBob alicePubKey bobPubKey
      , preBox'args   = byteArgs [ swapHash ]
      }



bobInitSwapTx :: SwapSpec -> App Tx
bobInitSwapTx = undefined

aliceGrabTx :: SwapSecret -> App Tx
aliceGrabTx = undefined

bobGrabTx :: SwapSecret -> App Tx
bobGrabTx = undefined

bobWaitForSecret :: App (Maybe SwapSecret)
bobWaitForSecret = undefined

bobWaitForHash :: App (Maybe SwapHash)
bobWaitForHash = do
  bch <- newBlockChan 0.25 (Just 2)
  liftIO $ fmap (getTxSwapHash =<<) $ findTx bch (const False) 10
  where
    getTxSwapHash Tx{..} = do
      out <- tx'outputs V.!? 0
      undefined


startAliceProc :: SwapSpec -> User -> App ()
startAliceProc spec user = do
  (_swapSecret, swapHash) <- liftIO $ initSecret 24
  let Just aliceBox = user'box user
  tx <- aliceInitSwapTx (getProofEnv aliceWallet) aliceBox swapHash spec
  resp <- postTx tx
  logTest $ "Alice response:"
  printTest resp
  where
    aliceWallet = user'wallet user

startBobProc :: SwapSpec -> User -> App ()
startBobProc spec user = do
  mSwapHash <- bobWaitForHash
  logTest "Bob gets hash:"
  printTest $ mSwapHash
  return ()


singleOwnerScript :: PublicKey -> Script
singleOwnerScript pubKey = mainScriptUnsafe $ pk' pubKey

