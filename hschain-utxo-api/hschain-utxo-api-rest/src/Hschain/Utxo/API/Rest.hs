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
  :<|> GetTxSigmaEndpoint
  :<|> GetEnvEndpoint
  :<|> GetStateEndpoint
  )

type PostTxEndpoint = "tx" :> Summary "Post Tx" :> "post"
  :> ReqBody '[JSON] Tx
  :> Post '[JSON] PostTxResponse

type GetBoxBalanceEndpoint = "box-balance" :> Summary "Gets the balance inside UTXO box" :> "get"
  :> Capture "box-id" BoxId
  :> Get '[JSON] (Maybe Money)

type GetTxSigmaEndpoint = "tx-sigma" :> Summary "Gets tx-script evaluated to sigma expression" :> "get"
  :> ReqBody '[JSON] Tx
  :> Get '[JSON] SigmaTxResponse

type GetEnvEndpoint = "env"
  :> Get '[JSON] GetEnvResponse

-- debug api method
type GetStateEndpoint = "debug" :> "state" :> Summary "Gets the state of box-chain" :> "get"
  :> Get '[JSON] BoxChain

data PostTxResponse = PostTxResponse
  { postTxResponse'value :: !(Either Text TxHash )
  , postTxResponse'debug :: !Text }
  deriving (Show, Eq)

data SigmaTxResponse = SigmaTxResponse
  { sigmaTxResponse'value :: !(Either Text (Sigma PublicKey) )
  , sigmaTxResponse'debug :: !Text }
  deriving (Show, Eq)

newtype GetEnvResponse = GetEnvResponse { unGetEnvResponse :: Env }
  deriving (Show, Eq, FromJSON, ToJSON)

instance ToHttpApiData TxHash where
  toQueryParam (TxHash txt) = txt

instance FromHttpApiData TxHash where
  parseQueryParam = fmap (fmap TxHash) parseQueryParam

instance ToHttpApiData BoxId where
  toQueryParam (BoxId txt) = txt

instance FromHttpApiData BoxId where
  parseQueryParam = fmap (fmap BoxId) parseQueryParam

$(deriveJSON dropPrefixOptions ''PostTxResponse)
$(deriveJSON dropPrefixOptions ''SigmaTxResponse)
