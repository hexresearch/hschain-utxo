-- | minimal lightning implementation to see
-- what primitives we need and do we cover this case
module Hschain.Utxo.Test.Client.Scripts.Lightning(
) where

import Control.Concurrent.STM
import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.Int

import Hschain.Utxo.Lang
import Hschain.Utxo.Test.Client.Wallet (Wallet)

newtype ChannelId = ChannelId ByteString

data Net = Net
  { net'channels :: [Channel]
  , net'users    :: [PublicKey]
  }

-- | Channel is a link between two users
-- that ar erepresented by their publickeys
data Channel = Channel
  { channel'id       :: ChannelId   -- ^ channel id
  , channel'a        :: User        -- ^ user A
  , channel'b        :: User        -- ^ user B
  , channel'capacity :: Int64       -- ^ capacity of the channel
  , channel'box      :: BoxId       -- ^ multisig box shared by the users
  }

-- | Lightning network environment
newtype NetEnv = NetEnv
  { netEnv'net :: TVar Net
  }

-- | Network monad
type NetApp a = Reader NetEnv a

data User = User
  { user'publicKey :: PublicKey
  -- some call-back methods for network to interact with the user
  , user'api       :: Msg -> IO Reply
  }

data Msg
data Reply

registerUser :: Wallet -> NetApp ()
registerUser = undefined

data Send = Send
  { send'from   :: PublicKey
  , send'to     :: PublicKey
  , send'value  :: Int64
  , send'fee    :: Int64
  }

send :: Send -> NetApp ()
send = undefined

type Route = [Channel]

data NetMsg = NetMsg
  { netMsg'next :: Maybe ChannelId
  , netMsg'rest :: NetMsg
  }

findRoute :: Send -> NetApp (Maybe Route)
findRoute = undefined

-- | User connects to network and gets API to send requests.
connectNet :: User -> NetApp Node
connectNet = undefined

data Node = Node
  { node'send         :: Send -> IO ()
  , node'revealSecret :: PublicKey -> ByteString -> IO ()
  , node'close        :: IO ()
  }




