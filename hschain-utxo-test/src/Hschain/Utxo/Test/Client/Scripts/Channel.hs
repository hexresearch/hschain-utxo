-- | Alice and Bob create bidirectional channel and make series of safe exchanges off-chain.
module Hschain.Utxo.Test.Client.Scripts.Channel(
  channelExchange
) where

import Control.Concurrent.STM

import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Int

import Hschain.Utxo.Test.Client.Wallet

import Hschain.Utxo.Test.Client.Monad hiding (getHeight)
import Hschain.Utxo.Test.Client.Scripts.MultiSig (getSharedBoxTx, postTxDebug)
import Hschain.Utxo.Test.Client.Scripts.Utils
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Utils.ByteString
import Hschain.Utxo.Lang.Build

import System.Random

import qualified Data.ByteString.Char8 as B

-- | Alice and Bob create bidirectional channel and make series of safe exchanges off-chain.
--
-- * the common box that contains 10 is created (5 from Alice and 5 from Bob)
-- * Alice trasfers 1 to Bob and balance becomes 4 for Alice, 6 for Bob
--     * they create assymetric TX and exchange revokation keys
-- * Alice transfers 1 more to bob and balance becomes 3 for Alice, 7 for Bob
-- * They post final commitment tx and spend the total balance.
channelExchange :: App ()
channelExchange = do
  testTitle "Bidirectional channel based on revokation key."
  Scene{..} <- initUsers
  let alice     = user'wallet scene'alice
      bob       = user'wallet scene'bob
      john      = user'wallet scene'john
      Just aliceBox1 = user'box scene'alice
      Just bobBox1   = user'box scene'bob
  (tx, commonBoxId) <- getSharedBoxTx alice bob (5, 5) (5, 5) aliceBox1 bobBox1
  void $ postTxDebug True "Alice and Bob post joint multisig TX" tx
  return ()

postDelay :: Int
postDelay = 1000

newtype Player = Player (TVar PlayerEnv)


signDeal :: Game -> Balance -> App ()
signDeal = undefined

data Game = Game
  { game'alice :: Player
  , game'bob   :: Player
  }

newPlayer :: BoxId -> Balance -> Wallet -> PublicKey -> App Player
newPlayer commonBoxId balance wallet partnerPubKey = liftIO $ do
  revokeSecret <- generateRevokeSecret
  let offChain = offChainPreTx revokeSecret commonBoxId balance (getWalletPublicKey wallet) partnerPubKey
  fmap Player $ newTVarIO $ PlayerEnv
    { playerEnv'balance       = balance
    , playerEnv'pending       = offChain
    , playerEnv'deals         = []
    , playerEnv'wallet        = wallet
    , playerEnv'partnerPubKey = partnerPubKey
    , playerEnv'commonBoxId   = commonBoxId
    }

data PlayerEnv = PlayerEnv
  { playerEnv'balance       :: !Balance
  , playerEnv'pending       :: !OffChain
  , playerEnv'deals         :: ![Deal]
  , playerEnv'wallet        :: !Wallet
  , playerEnv'partnerPubKey :: !PublicKey
  , playerEnv'commonBoxId   :: !BoxId
  }

data Deal = Deal
  { deal'tx           :: !Tx
  , deal'revokeSecret :: !RevokeSecret
  }

type Balance = (Int64, Int64)

data OffChain = OffChain
  { offChain'tx            :: !PreTx
  , offChain'revokationKey :: !ByteString
  } deriving (Show, Eq)

data OffChainSigned = OffChainSigned
  { offChainSigned'tx            :: !Tx
  , offChainSigned'revokationKey :: !ByteString
  }

initBalance :: BoxId -> Balance -> Wallet -> Wallet -> App (OffChain, OffChain)
initBalance commonBoxId (aliceValue, bobValue) alice bob = do
  aliceTx <- offChainTx commonBoxId (aliceValue, bobValue) alicePk bobPk
  bobTx   <- offChainTx commonBoxId (bobValue, aliceValue) bobPk   alicePk
  return (aliceTx, bobTx)
  where
    alicePk = getWalletPublicKey alice
    bobPk   = getWalletPublicKey bob

updateBalance :: BoxId -> (RevokeSecret, RevokeSecret) -> Balance -> Wallet -> Wallet -> App (OffChainSigned, OffChainSigned)
updateBalance commonBoxId (alicePrevRevokeSecret, bobPrevRevokeSecret) balance alice bob = do
  (aliceOffTx, bobOffTx) <- initBalance commonBoxId balance alice bob
  bobOffTx' <- signOffChain alicePrevRevokeSecret bobOffTx
  aliceOffTx' <- signOffChain bobPrevRevokeSecret aliceOffTx
  return (aliceOffTx', bobOffTx')

generateRevokeSecret :: IO ByteString
generateRevokeSecret = initSecret 128

signOffChain :: RevokeSecret -> OffChain -> App OffChainSigned
signOffChain = undefined

offChainTx :: BoxId -> Balance -> PublicKey -> PublicKey -> App OffChain
offChainTx commonBoxId balance me partner = do
  revokeSecret <- liftIO $ initSecret 1000
  return $ offChainPreTx revokeSecret commonBoxId balance me partner

offChainPreTx ::  ByteString -> BoxId -> Balance -> PublicKey -> PublicKey -> OffChain
offChainPreTx revokeSecret commonBoxId (myValue, partnerValue) myPk partnerPk = do
  OffChain
    { offChain'tx = preTx
    , offChain'revokationKey = revokeSecret
    }
  where
    preTx = Tx
      { tx'inputs  = [inputRef Nothing]
      , tx'outputs = [myBox, partnerBox]
      }

    inputRef proof = BoxInputRef
      { boxInputRef'id = commonBoxId
      , boxInputRef'args  = mempty
      , boxInputRef'proof = proof
      , boxInputRef'sigMask = SigAll
      }

    -- | Pays to me delayd by postDelay and eveybody with revoke key can claim it
    myBox = PreBox
      { preBox'value  = myValue
      , preBox'script = mainScriptUnsafe revokeScript
      , preBox'args   = mempty
      }

    revokeScript =
          (pk' myPk &&* (toSigma $ getHeight >* int postDelay))
      ||* (pk' partnerPk &&* (toSigma $ sha256 readKey ==* bytes revokeHash))
      where
        revokeHash = getSha256 revokeSecret

    readKey = listAt (getBoxBytesArgList getSelf) 0

    -- | Pays to partner right away
    partnerBox = PreBox
      { preBox'value  = partnerValue
      , preBox'script = mainScriptUnsafe $ pk' partnerPk
      , preBox'args   = mempty
      }

type RevokeSecret = ByteString

initSecret :: Int -> IO RevokeSecret
initSecret size = fmap B.pack $ mapM (const randomIO) [1 .. size]

