module Hschain.Utxo.API.Rest where

import Hex.Common.Aeson

import Data.Aeson
import Data.Text (Text)

import Web.HttpApiData

import Servant.API
import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types

type UtxoAPI = "api" :>
  (    PostTxEndpoint
  :<|> GetBoxBalanceEndpoint
  :<|> GetEnvEndpoint
  :<|> GetStateEndpoint
  )

type PostTxEndpoint = "tx" :> Summary "Post Tx" :> "post"
  :> ReqBody '[JSON] Tx
  :> Post '[JSON] PostTxResponse

type GetBoxBalanceEndpoint = "box-balance" :> Summary "Gets the balance inside UTXO box" :> "get"
  :> Capture "bix-id" BoxId
  :> Get '[JSON] (Maybe Money)

type GetEnvEndpoint = "env"
  :> Post '[JSON] GetEnvResponse

-- debug api method
type GetStateEndpoint = "debug" :> "state" :> Summary "Gets the state of box-chain" :> "get"
  :> Get '[JSON] BoxChain

data PostTxResponse = PostTxResponse
  { postTxResponse'value :: !(Either Text TxHash )
  , postTxResponse'debug :: !Text }
  deriving (Show, Eq)

newtype GetEnvResponse = GetEnvResponse { unGetEnvResponse :: Env }
  deriving (Show, Eq, FromJSON, ToJSON)

instance ToHttpApiData BoxId where
  toQueryParam (BoxId txt) = txt

instance FromHttpApiData BoxId where
  parseQueryParam = fmap (fmap BoxId) parseQueryParam

$(deriveJSON dropPrefixOptions ''PostTxResponse)
