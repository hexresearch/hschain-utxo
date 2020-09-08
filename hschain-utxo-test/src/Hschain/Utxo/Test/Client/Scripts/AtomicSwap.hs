module Hschain.Utxo.Test.Client.Scripts.AtomicSwap where

import Prelude hiding ((<*))

import Data.ByteString (ByteString)

import System.Random

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Lang.Utils.ByteString(getSha256)

import Hschain.Utxo.Test.Client.Monad (App, testTitle)
import Hschain.Utxo.Test.Client.Scripts.Utils

import qualified Data.ByteString.Char8 as B

-- How to read block by height:
--
-- HSChain.Store.Internal.BlockDB.retrieveBlock ::  Height -> Query rw a (Maybe (Block a))

atomicSwap :: App ()
atomicSwap = do
  testTitle "Atomic swap: Alice exhanges with Bob."
  Scene{..} <- initUsers
  return ()

type SwapSecret = ByteString
type SwapHash   = ByteString

data SwapSpec = SwapSpec
  { swapSpec'alice        :: !SwapUser
  , swapSpec'bob          :: !SwapUser
  , swapSpec'hash         :: !SwapHash
  , swapUser'exchangeRate :: !Int
  }

data SwapUser = SwapUser
  { swapUser'pk        :: PublicKey
  , swapUser'deadline  :: Int
  , swapUser'value     :: Int
  }

initSecret :: Int -> IO SwapSecret
initSecret size = fmap B.pack $ mapM (const randomIO) [1 .. size]

aliceInitSwapScript :: SwapHash -> Int -> PublicKey -> PublicKey -> Script
aliceInitSwapScript swapHash deadlineBob alicePubKey bobPubKey = mainScriptUnsafe $
  orSigma $ fromVec
    [ toSigma (getHeight >* int deadlineBob) &&* pk' alicePubKey
    , pk' bobPubKey &&* (toSigma $ (sha256 $ listAt getBytesVars 0) ==* bytes swapHash)
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

aliceInitSwapTx :: SwapSpec -> App Tx
aliceInitSwapTx = undefined

bobInitSwapTx :: SwapSpec -> App Tx
bobInitSwapTx = undefined

aliceGrabTx :: SwapSecret -> App Tx
aliceGrabTx = undefined

bobGrabTx :: SwapSecret -> App Tx
bobGrabTx = undefined

bobWaitForSecret :: App (Maybe SwapSecret)
bobWaitForSecret = undefined


