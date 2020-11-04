-- | Minimal lightning protocol
module Hschain.Utxo.Test.Client.Scripts.Lightning.Protocol(
    Chan(..)
  , ChanId(..)
  , ChanSpec(..)
  , UserId(..)
  , Msg(..)
  , Act(..)
  , HtlcId(..)
  , Htlc(..)
  , Route
  , Hop(..)
  , userIdToText
) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text

import Hschain.Utxo.Lang

import Data.Text.Encoding

newtype ChanId = ChanId ByteString
  deriving (Show, Eq, Ord)

-- | Chan is a link between two users
-- that ar erepresented by their publickeys
data Chan = Chan
  { chan'id       :: ChanId      -- ^ chan id
  , chan'a        :: UserId      -- ^ user A
  , chan'b        :: UserId      -- ^ user B
  , chan'capacity :: Int64       -- ^ capacity of the chan
  }
  deriving (Show, Eq)

data UserId = UserId ByteString
  deriving (Show, Eq, Ord)

userIdToText :: UserId -> Text
userIdToText (UserId bs) = decodeUtf8 bs

data ChanSpec = ChanSpec
  { chanSpec'id        :: ChanId -- ^ channel id
  , chanSpec'capacity  :: Money  -- ^ channel capacity, maximum throughput value
  , chanSpec'delay     :: Int64  -- ^ delay used for operation of the channel
  , chanSpec'minDepth  :: Int64  -- ^ minimal depth for funding TX to be valid for usage in channel
  }
  deriving (Show, Eq)

data Msg = Msg
  { msg'from  :: UserId
  , msg'to    :: UserId
  , msg'act   :: Act
  }

newtype HtlcId = HtlcId Int64
  deriving (Show, Eq, Ord)

data Htlc = Htlc
  { htlc'value     :: Int64       -- ^ amount of money to transfer
  , htlc'fee       :: Int64       -- ^ fee for transaction
  , htlc'time      :: Int64       -- ^ HTLC expiry absolute time
  , htlc'payHash   :: ByteString  -- ^ HTLC hash of the preimage
  }
  deriving (Show, Eq)

data Act
  --------------------------------------------------
  -- Establish channel
  = OpenChan
      { act'spec             :: ChanSpec
      , act'publicKey        :: PublicKey
      }
  -- ^ Request to open channel
  | AcceptChan
      { act'chanId           :: ChanId
      , act'publicKey        :: PublicKey
      , act'revokeHash       :: ByteString
      }
  -- ^ Accept request to open channel
  | FundingCreated
      { act'chanId           :: ChanId        -- channel id
      , act'fundingTxId      :: TxId          -- txid for funding TX
      , act'signCommitmentTx :: ByteString    -- signature for first commitment TX
      , act'revokeHash       :: ByteString    -- hash of revoke secret for first commitment
      }
  | FundingSigned
      { act'chanId           :: ChanId
      , act'signCommitmentTx :: ByteString    -- signature for first commitment TX from other party
      }
  | FundingLocked
      { act'chanId           :: ChanId
      }
  -- ^ Both parties send this message when depth of funding TX is reached
  --------------------------------------------------
  -- Close channel
  | ShutdownChan
      { act'chanId            :: ChanId
      }
  -- ^ Request and confirmation to close the channel
  | ClosingSigned
      { act'chanId           :: ChanId
      , act'sign             :: ByteString
      , act'fee              :: Money
      }
  -- ^ sends signature and fee for closing TX
  | ConfirmClosingSigned
      { act'chanId           :: ChanId
      , act'sign             :: ByteString
      , act'fee              :: Money
      }
  -- ^ confirms signature and fee for closing TX
  --------------------------------------------------
  -- Normal operation
  | UpdateAddHtlc
      { act'chanId           :: ChanId
      , act'htlcId           :: HtlcId       -- ^ HTLC id
      , act'route            :: Route
      }
  | UpdateFulfillHtlc
      { act'chanId           :: ChanId
      , act'htlcId           :: HtlcId       -- ^ HTLC id
      , act'paymentSecret    :: ByteString
      }
  -- ^ sets up HTLC
  | CommitmentSigned
      { act'chanId           :: ChanId
      , act'sign             :: ByteString
      , act'htlcs            :: [ByteString]
      }
  -- ^ sign commitment
  | RevokeAndAck
      { act'chanId           :: ChanId
      , act'secret           :: ByteString
      }
  -- ^ reveals revocation secret
  deriving (Show, Eq)

-- | Route of channels
-- For now we do not encrypt hops and do not hide the links
-- but we should in real system.
type Route = [Hop]

-- | Single link in the route.
data Hop = Hop
  { hop'chanId    :: ChanId  -- ^ Channel id
  , hop'htlc      :: Htlc
  , hop'index     :: Int64   -- ^ how many hops till end
  }
  deriving (Show, Eq)



