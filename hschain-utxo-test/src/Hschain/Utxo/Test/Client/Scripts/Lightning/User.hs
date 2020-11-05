-- | FSM for lightning user
module Hschain.Utxo.Test.Client.Scripts.Lightning.User(
    Api(..)
  , UserEnv(..)
  , User(..)
  , newUser
  , cancelUserProc
  , insertChan
  , lookupChan
  , ChanSt(..)
) where

import Control.Arrow (first, second)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except

import Data.ByteString
import Data.Int
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Tuple (swap)

import HSChain.Crypto.Classes (ByteRepr(..))
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Utils.ByteString (getSha256)
import Hschain.Utxo.Test.Client.Monad
import Hschain.Utxo.Test.Client.Wallet (Wallet(..), getWalletPublicKey, getWalletPrivateKey)
import Hschain.Utxo.Test.Client.Scripts.Lightning.Protocol
import Hschain.Utxo.Test.Client.Scripts.Lightning.Tx
import Hschain.Utxo.Test.Client.Scripts.Utils (postTxSuccess, postTxFailure)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Crypto.Signature as Crypto

-- | Communication with network
data Api = Api
  { api'send  :: Msg -> IO ()
  , api'read  :: IO Msg
  }

-- | Lightning user env
data UserEnv = UserEnv
  { userEnv'id      :: UserId                     -- ^ user id in the lightning network
  , userEnv'wallet  :: Wallet                     -- ^ user's wallet
  , userEnv'funds   :: Funds                      -- ^ funds so far
  , userEnv'chans   :: Map ChanId ChanSt          -- ^ set of active channels
  , userEnv'secrets :: Map ByteString ByteString  -- ^ secrets for HTLC transfers
  }

-- | User funds
type Funds = [(BoxId, Money)]

-- | Channel states
data ChanSt
  = ChanInit
    { chanSt'spec       :: ChanSpec
    , chanSt'boxes      :: [BoxId]
    , chanSt'change     :: Money
    }
  | ChanOpen
    { chanSt'spec       :: ChanSpec
    , chanSt'partnerKey :: PublicKey
    , chanSt'revoke     :: ByteString
    }
  | ChanAccepted
    { chanSt'fundTx      :: Tx
    , chanSt'partnerKey  :: PublicKey
    , chanSt'partnerHash :: ByteString
    , chanSt'spec        :: ChanSpec
    , chanSt'revoke      :: ByteString
    }
  | ChanSigned
    { chanSt'spec        :: ChanSpec
    , chanSt'commonBoxId :: BoxId
    , chanSt'partnerHash :: ByteString
    , chanSt'partnerKey  :: PublicKey
    , chanSt'revoke      :: ByteString
    }
  | ChanActive
    { chanSt'spec          :: ChanSpec
    , chanSt'balance       :: (Money, Money)
    , chanSt'partnerId     :: UserId
    , chanSt'partnerKey    :: PublicKey
    , chanSt'commonBoxId   :: BoxId
    , chanSt'revokes       :: Map BoxId RevokeBox
    , chanSt'htlcCount     :: Int64
    , chanSt'htlcs         :: Map HtlcId HtlcLink
    , chanSt'lastRevoke    :: ByteString
    }
  | ChanShutdownPending ChanSt

data RevokeBox = RevokeBox
  { revokeBox'secret     :: ByteString
  , revokeBox'value      :: Money
  }

-- | Htlc with info on where to give back revoke secret
data HtlcLink = HtlcLink
  { htlcLink'content :: Htlc
  , htlcLink'prev    :: Maybe BackHop
  }

data BackHop = BackHop
  { backHop'chanId   :: ChanId
  , backHop'htlcId   :: HtlcId
  }

-- | User
data User = User
  { user'id         :: UserId
  , user'env        :: TVar UserEnv   -- ^ user internal state
  , user'api        :: Api            -- ^ communication with network
  , user'proc       :: Async ()       -- ^ background listen and response process
  , user'revokeProc :: Async ()       -- ^ revoke TXs process
  }

newUser :: Api -> UserId -> Wallet -> Funds -> App User
newUser api uid wallet funds = do
  env <- liftIO $ newUserEnv uid wallet funds
  proc <- startUserProc api env
  revokeProc <- startRevokeProc env
  return $ User
    { user'id   = uid
    , user'env  = env
    , user'api  = api
    , user'proc = proc
    , user'revokeProc = revokeProc
    }

newUserEnv :: UserId -> Wallet -> Funds -> IO (TVar UserEnv)
newUserEnv uid wallet funds = do
  tv <- newTVarIO $ UserEnv uid wallet funds M.empty M.empty
  return tv

startUserProc :: Api -> TVar UserEnv -> App (Async ())
startUserProc Api{..} tv = fmap void $ async $ loop
  where
    loop = do
      msg <- liftIO api'read
      env <- liftIO $ readTVarIO tv
      when (userEnv'id env == msg'to msg) $ do
        (env', mReply) <- react msg env
        liftIO $ do
          atomically $ writeTVar tv env'
          mapM_ api'send mReply
      loop

cancelUserProc :: User -> App ()
cancelUserProc User{..} = do
  cancel user'proc
  cancel user'revokeProc

--------------------------------------------------------------
-- lightning user FSM

react :: Msg -> UserEnv -> App (UserEnv, Maybe Msg)
react msg@Msg{..} env =
  case msg'act of
    OpenChan{..}             -> openChan act'spec act'publicKey
    AcceptChan{..}           -> acceptChan act'chanId act'publicKey act'revokeHash
    FundingCreated{..}       -> fundingCreated act'chanId act'fundingTxId act'revokeHash
    FundingSigned{..}        -> fundingSigned act'chanId act'sign
    FundingLocked{..}        -> fundingLocked act'chanId
    ShutdownChan{..}         -> shutdownChan act'chanId
    ClosingSigned{..}        -> closingSigned act'chanId act'sign act'fee
    ConfirmClosingSigned{..} -> confirmClosingSigned act'chanId act'sign act'fee
    UpdateAddHtlc{..}        -> updateAddHtlc act'chanId act'htlcId act'route
    UpdateFulfillHtlc{..}    -> updateFulfillHtlc act'chanId act'htlcId act'paymentSecret
    CommitmentSigned{..}     -> commitmentSigned act'chanId act'sign act'revokeHash
  where
    myPk = getWalletPublicKey $ userEnv'wallet env

    openChan spec pubKey = do
      revokeSecret <- liftIO $ randomBS 16
      let revokeHash = getSha256 revokeSecret
          chanId = chanSpec'id spec
          reply = toReplyMsg msg $ AcceptChan chanId myPk revokeHash
      return (insertChan chanId (ChanOpen spec pubKey revokeSecret) env, Just reply)

    acceptChan chanId pubKey bobRevokeHash = case lookupChan chanId env of
      Just ChanInit{..} -> liftIO $ do
        let value  = chanSpec'capacity chanSt'spec
        fundTx <- fundingTx (userEnv'wallet env) (value, chanSt'change) chanSt'boxes pubKey
        aliceRevokeSecret <- randomBS 16
        let comTx = commitmentTx pubKey (getSharedBoxId fundTx) (0, value) myPk (chanSpec'delay chanSt'spec) bobRevokeHash []
        signature <- Crypto.sign (getWalletPrivateKey $ userEnv'wallet env) (getSigMessage SigAll comTx)
        let reply = toReplyMsg msg FundingCreated
                      { act'chanId            = chanId
                      , act'fundingTxId       = computeTxId fundTx
                      , act'signCommitmentTx  = encodeToBS signature
                      , act'revokeHash        = getSha256 aliceRevokeSecret
                      }
            env' = insertChan chanId ChanAccepted
                      { chanSt'spec = chanSt'spec
                      , chanSt'partnerKey = pubKey
                      , chanSt'partnerHash = bobRevokeHash
                      , chanSt'revoke = aliceRevokeSecret
                      , chanSt'fundTx = fundTx
                      }
                    env
        return (env', Just reply)
      _                 -> errWrongChanState

    fundingCreated chanId fundingTxId partnerRevokeHash = case lookupChan chanId env of
      Just ChanOpen{..} -> do
        let commonBoxId = computeBoxId fundingTxId 0
            value = chanSpec'capacity chanSt'spec
            comTx = commitmentTx  chanSt'partnerKey commonBoxId (value, 0) myPk (chanSpec'delay chanSt'spec) partnerRevokeHash []
        signature <- liftIO $ Crypto.sign (getWalletPrivateKey $ userEnv'wallet env) (getSigMessage SigAll comTx)
        let reply = toReplyMsg msg $ FundingSigned chanId (encodeToBS signature)
            env' = insertChan chanId ChanSigned
                      { chanSt'spec = chanSt'spec
                      , chanSt'partnerKey = chanSt'partnerKey
                      , chanSt'commonBoxId = commonBoxId
                      , chanSt'partnerHash = partnerRevokeHash
                      , chanSt'revoke = chanSt'revoke
                      }
                    env
        return (env', Just reply)
      _ -> errWrongChanState

    fundingSigned chanId _signature = case lookupChan chanId env of
      Just ChanAccepted{..} -> do
        isSent <- checkFundingWasSent chanSt'fundTx
        when (not isSent) $ postTxSuccess (msgForUsers "Post Funding TX" msg) chanSt'fundTx
        -- we should listen to Blockchain to wait for minDepth confirmations
        -- but let's skip this for know and send it right away
        let reply = toReplyMsg msg $ FundingLocked chanId
        return (env, Just reply)
      _ -> errWrongChanState
      where
        checkFundingWasSent tx =
          fmap isNothing $ getBoxBalance boxId
          where
            boxId = boxInputRef'id $ tx'inputs tx V.! 0

    fundingLocked chanId = case lookupChan chanId env of
      Just ChanAccepted{..} -> do
        let st = ChanActive
                  { chanSt'spec = chanSt'spec
                  , chanSt'partnerKey = chanSt'partnerKey
                  , chanSt'partnerId = msg'from
                  , chanSt'balance = (chanSpec'capacity chanSt'spec, 0)
                  , chanSt'commonBoxId = computeBoxId (computeTxId chanSt'fundTx) 0
                  , chanSt'revokes = M.empty
                  , chanSt'htlcCount = 0
                  , chanSt'htlcs = M.empty
                  , chanSt'lastRevoke = chanSt'revoke
                  }
        return (insertChan chanId st env, Nothing)
      Just ChanSigned{..}   -> do
        let st = ChanActive
                  { chanSt'spec = chanSt'spec
                  , chanSt'partnerKey = chanSt'partnerKey
                  , chanSt'partnerId = msg'from
                  , chanSt'balance = (0, chanSpec'capacity chanSt'spec)
                  , chanSt'commonBoxId = chanSt'commonBoxId
                  , chanSt'revokes = M.empty
                  , chanSt'htlcCount = 0
                  , chanSt'htlcs = M.empty
                  , chanSt'lastRevoke = chanSt'revoke
                  }
        return (insertChan chanId st env, Nothing)
      _  -> errWrongChanState

    shutdownChan chanId = case lookupChan chanId env of
      Just st@ChanActive{..} -> do
        let closeTx = closeChanTx chanSt'commonBoxId chanSt'balance (myPk, chanSt'partnerKey)
        sign <- fmap encodeToBS $ liftIO $ Crypto.sign (getWalletPrivateKey $ userEnv'wallet env) (getSigMessage SigAll closeTx)
        let fee = 1
            env' = insertChan chanId (ChanShutdownPending st) env
            reply = toReplyMsg msg $ ClosingSigned chanId sign fee
        return (env', Just reply)
      _ -> errWrongChanState

    closingSigned chanId partnerSign fee = case lookupChan chanId env of
      Just (ChanShutdownPending ChanActive{..}) -> do
        let reply = toReplyMsg msg $ ConfirmClosingSigned chanId partnerSign fee
            partnerCloseTx = closeChanTx chanSt'commonBoxId (swap chanSt'balance) (chanSt'partnerKey, myPk)
        mySign <- fmap encodeToBS $ liftIO $ Crypto.sign (getWalletPrivateKey $ userEnv'wallet env) (getSigMessage SigAll partnerCloseTx)
        postCloseChanTx partnerCloseTx mySign partnerSign
        return (closeChan chanId env, Just reply)
      _ -> errWrongChanState

    confirmClosingSigned _chanId _sign _fee = return (env, Nothing)

    updateAddHtlc chanId htlcId route = case lookupChan chanId env of
      Just st@ChanActive{..} -> checkHtlcIsNew htlcId chanSt'htlcCount $ do
        case route of
          []              -> throwError "Empty route"
          curHop:restHops -> if shouldProceed curHop
                               then proceedHops curHop restHops st
                               else replyWithSecret curHop
      _ -> errWrongChanState
      where
        proceedHops curHop restHops st = do
          env1 <- addIncomeHtlc htlcId (hop'htlc curHop) Nothing st chanId env
          case restHops of
            []          -> throwError "Empty route"
            nextHop : _ -> do
              let nextChanId = hop'chanId nextHop
              (nextHtlcId, env2) <- newHtlcId nextChanId env1
              env3 <- addOutcomeHtlc nextHtlcId (hop'htlc nextHop) (Just $ BackHop chanId htlcId) nextChanId env2
              nextUserId <- getChanPartner nextChanId env3
              let reply = Msg
                    { msg'from = userEnv'id env
                    , msg'to   = nextUserId
                    , msg'act  = UpdateAddHtlc nextChanId nextHtlcId restHops
                    }
              return (env2, Just reply)

        replyWithSecret Hop{..} = do
          case M.lookup (htlc'payHash hop'htlc) (userEnv'secrets env) of
            Just secret -> do
              let reply = toReplyMsg msg $ UpdateFulfillHtlc chanId htlcId secret
              return (env, Just reply)
            _ -> throwError "No secret found"

    addIncomeHtlc htlcId htlc backRef st chanId e = case st of
      ChanActive{..} -> return $ insertChan chanId st' e
      _              -> errWrongChanState
      where
        st' = st
                { chanSt'htlcCount = chanSt'htlcCount st + 1
                , chanSt'htlcs     = M.insert htlcId (HtlcLink htlc backRef) (chanSt'htlcs st)
                , chanSt'balance   = second (\v -> v - htlc'value htlc) $ chanSt'balance st
                }

    addOutcomeHtlc htlcId htlc backRef chId e = case lookupChan chId e of
      Just st@ChanActive{..} -> do
          let st' = st { chanSt'htlcs = M.insert htlcId (HtlcLink (negateHtlc htlc) backRef) chanSt'htlcs
                        , chanSt'balance = first (\v -> v - htlc'value htlc) chanSt'balance
                        }
          return $ insertChan chId st' e
      _              -> errWrongChanState

    updateFulfillHtlc chanId htlcId paySecret = case lookupChan chanId env of
      Just ChanActive{..} -> do
        case M.lookup htlcId chanSt'htlcs of
          Just h  ->
            if checkHtlcSecret paySecret (htlcLink'content h)
              then do
                let env1 = fulfillHtlc chanId htlcId env
                case htlcLink'prev h of
                  Just prevHop -> sendPrevHop prevHop paySecret env1
                  Nothing      -> return (env1, Nothing)
              else errHtlcPaymentSecretIsNotValid
          Nothing -> errHtlcNotFound

      _ -> errWrongChanState

    errHtlcNotFound = throwError "HTLC is not found"
    errHtlcPaymentSecretIsNotValid = throwError "HTLC payment secret is not valid"

    checkHtlcSecret secret Htlc{..} = htlc'payHash == getSha256 secret

    sendPrevHop BackHop{..} paySecret e = do
      partnerId <- getChanPartner backHop'chanId e
      return (e1, Just $ Msg
             { msg'from = userEnv'id e
             , msg'to   = partnerId
             , msg'act  = UpdateFulfillHtlc backHop'chanId backHop'htlcId paySecret
             })
      where
        e1 = fulfillHtlc backHop'chanId backHop'htlcId e

    fulfillHtlc chanId htlcId e = case lookupChan chanId e of
      Just st@ChanActive{..} -> case M.lookup htlcId chanSt'htlcs of
                                  Just HtlcLink{..} ->
                                    insertChan chanId (st
                                      { chanSt'htlcs   = M.delete htlcId chanSt'htlcs
                                      , chanSt'balance =
                                          let val = htlc'value htlcLink'content
                                          in  if val > 0
                                                then first  (+ val) chanSt'balance
                                                else second (+ val) chanSt'balance
                                      }) e
                                  Nothing -> e
      _                     -> e

    shouldProceed Hop{..} = hop'index > 0

    negateHtlc h = h { htlc'value = negate $ htlc'value h }

    newHtlcId chanId e = case lookupChan chanId e of
      Just st@ChanActive{..} -> do
        let curId = chanSt'htlcCount
            env' = insertChan chanId (st { chanSt'htlcCount = curId + 1 }) e
        return (HtlcId curId, env')
      _ -> errWrongChanState

    checkHtlcIsNew htlcId htlcCount cont =
      if (htlcId >= HtlcId htlcCount)
        then cont
        else throwError "HTLC id is not new"

    errWrongChanState =  testCase msg False >> throwError msg
      where
        msg = "Wrong channel state"

    postCloseChanTx tx signA signB = do
      testCase "close chan TX is valid" =<< txIsValid signedTx
      postTxSuccess (msgForUsers "Post close TX" msg) signedTx
      where
        signedTx = tx { tx'inputs = fmap setSigns $ tx'inputs tx }
        setSigns x = x { boxInputRef'sigs = V.fromList $ catMaybes $ fmap decodeFromBS [signA, signB] }


    commitmentSigned chanId signature revokeHash = case lookupChan chanId env of
      Just st@ChanActive{..} -> if checkCommitmentSignature signature revokeHash
        then do
          nextRevokeSecret <- liftIO $ randomBS 16
          let reply = toReplyMsg msg $ RevokeAndAck chanId chanSt'lastRevoke
              env'  = insertChan chanId (st { chanSt'lastRevoke = nextRevokeSecret }) env
          return (env', Just reply)
        else throwError "Signature is incorrect"
      _ -> errWrongChanState

    checkCommitmentSignature = undefined

msgForUsers :: Text -> Msg -> Text
msgForUsers txt Msg{..} =
  mconcat [txt, " for ", userIdToText msg'from, " and ", userIdToText msg'to]

toReplyMsg :: Msg -> Act -> Msg
toReplyMsg msg act = swapAddr msg { msg'act = act }

swapAddr :: Msg -> Msg
swapAddr msg = msg { msg'from = msg'to msg, msg'to = msg'from msg }

insertChan :: ChanId -> ChanSt -> UserEnv -> UserEnv
insertChan cid st env@UserEnv{..} =
  env { userEnv'chans = M.insert cid st userEnv'chans }

lookupChan :: ChanId -> UserEnv -> Maybe ChanSt
lookupChan cid UserEnv{..} = M.lookup cid userEnv'chans

closeChan :: ChanId -> UserEnv -> UserEnv
closeChan cid env@UserEnv{..} =
  env { userEnv'chans = M.delete cid userEnv'chans }

getChanPartner :: ChanId -> UserEnv -> App UserId
getChanPartner chanId env = case lookupChan chanId env of
  Just ChanActive{..} -> return chanSt'partnerId
  _ -> throwError "Wrong channel state"


-- | Todo: check for revokation TXs and commit them if any fraud has happened
-- For now it does nothing but in real world it should listen TXs
-- and check for any fraud TXs from previous echange steps being posted.
startRevokeProc ::  TVar UserEnv -> App (Async ())
startRevokeProc _ = fmap void $ async (return ())


