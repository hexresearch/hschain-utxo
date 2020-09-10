{-# OPTIONS_GHC -Wno-orphans #-}
-- | API for hschain-utxo node
module Hschain.Utxo.API.Rest where

import Hex.Common.Aeson
import Hex.Common.Serialise
import Hex.Common.Text

import Control.Monad

import Data.Text (Text)
import Data.Vector (Vector)

import Web.HttpApiData

import Servant.API
import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types

type UtxoAPI = "api" :>
  (    PostTxEndpoint
  :<|> GetBoxEndpoint
  :<|> GetBoxBalanceEndpoint
  :<|> GetTxSigmaEndpoint
  :<|> GetEnvEndpoint
  :<|> GetStateEndpoint
  :<|> GetUtxosEndpoint
  :<|> HasUtxoEndpoint
  :<|> ReadBlockEndpoint
  :<|> ReadBlockchainHeightEndpoint
  )

-- | Post transaction
type PostTxEndpoint = "tx" :> Summary "Post Tx" :> "post"
  :> ReqBody '[JSON] Tx
  :> Post '[JSON] PostTxResponse

-- | Query box by identifier
type GetBoxEndpoint = "box" :> Summary "Gets the box by identifier" :> "get"
  :> Capture "box-id" BoxId
  :> Get '[JSON] (Maybe Box)

-- | Query box balance by identifier
type GetBoxBalanceEndpoint = "box-balance" :> Summary "Gets the balance inside UTXO box" :> "get"
  :> Capture "box-id" BoxId
  :> Get '[JSON] (Maybe Money)

-- | It evaluates transaction in the current state of blockchain
-- and returns sigma-expression. We can use it to create prove and post signed transaction.
type GetTxSigmaEndpoint = "tx-sigma" :> Summary "Gets tx-script evaluated to sigma expression" :> "get"
  :> ReqBody '[JSON] Tx
  :> Get '[JSON] SigmaTxResponse

-- | Get some stats for blockchain state (like height)
type GetEnvEndpoint = "env"
  :> Get '[JSON] GetEnvResponse

-- | Debug api method
type GetStateEndpoint = "debug" :> "state" :> Summary "Gets the state of box-chain" :> "get"
  :> Get '[JSON] BoxChain

-- | Reads all BoxId's that are available as inputs (UTXOs)
type GetUtxosEndpoint = "utxos" :> Summary "Reads all UTXOs" :> "get"
  :> Get '[JSON] [BoxId]

-- | Checks weather we can spend given @BoxId@.
type HasUtxoEndpoint = "has-utxo" :> Summary "Checks waether UTXO is avaiable"
  :> Capture "box-id" BoxId
  :> Get '[JSON] Bool

type ReadBlockEndpoint = "read-block" :> Summary "Reads block of transactions at the given height"
  :> Capture "height" Int
  :> Get '[JSON] (Maybe [Tx])

type ReadBlockchainHeightEndpoint = "read-blockchain-height" :> Summary "Reads current height of blockchain"
  :> Get '[JSON] Int

-- | Result of posted transaction. Contains TX-hash if it was approved.
data PostTxResponse = PostTxResponse
  { postTxResponse'value :: !(Maybe TxHash )
  } deriving (Show, Eq)

-- | Result of execution of TX in the current state of blockchain.
data SigmaTxResponse = SigmaTxResponse
  { sigmaTxResponse'value :: !(Either Text (Vector BoolExprResult))  -- ^ result of execution
                                                                     -- (sigma-expression or boolean)
  , sigmaTxResponse'debug :: !Text }                                 -- ^ Debug info on the process of execution
  deriving (Show, Eq)

-- | Useful stats about state of the blockchain
newtype GetEnvResponse = GetEnvResponse { unGetEnvResponse :: Env }
  deriving (Show, Eq, FromJSON, ToJSON)

instance ToHttpApiData TxHash where
  toQueryParam (TxHash bs) = serialiseToText bs

instance FromHttpApiData TxHash where
  parseQueryParam = fmap (join . fmap (maybe err (Right . TxHash) . serialiseFromText)) parseQueryParam
    where
      err = Left "Failed to decode query param for TxHash"

instance ToHttpApiData BoxId where
  toQueryParam = toText

instance FromHttpApiData BoxId where
  parseQueryParam = (\txt -> maybe (err txt) Right $ fromText txt) <=< parseQueryParam
    where
      err txt = Left $ "Failed to parse boxId from: " <> txt

$(deriveJSON dropPrefixOptions ''PostTxResponse)
$(deriveJSON dropPrefixOptions ''SigmaTxResponse)
