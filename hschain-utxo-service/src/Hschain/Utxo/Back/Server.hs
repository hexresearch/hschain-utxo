module Hschain.Utxo.Back.Server(
  utxoServer
) where

import Servant.API
import Servant.Server

import Hschain.Utxo.API.Rest
import Hschain.Utxo.Lang
import Hschain.Utxo.State.React
import Hschain.Utxo.State.Types

import Hschain.Utxo.Back.Monad

import qualified Data.Text as T

import qualified Hschain.Utxo.State.Query as S

-- | Server implementation for 'UtxoAPI'
utxoServer :: ServerT UtxoAPI ServerM
utxoServer =
       postTxEndpoint
  :<|> getBoxBalanceEndpoint
  :<|> getTxSigmaEndpoint
  :<|> getEnvEndpoint
  :<|> getStateEndpoint

postTxEndpoint :: Tx -> ServerM PostTxResponse
postTxEndpoint tx = undefined -- fmap (uncurry PostTxResponse) $ updateBoxChain (react tx)

getBoxBalanceEndpoint :: BoxId -> ServerM (Maybe Money)
getBoxBalanceEndpoint boxId =
  fmap (\bch -> S.getBoxBalance bch boxId) readBoxChain

getTxSigmaEndpoint :: Tx -> ServerM SigmaTxResponse
getTxSigmaEndpoint tx =
  fmap (\bch -> uncurry SigmaTxResponse $ execInBoxChain tx bch) readBoxChain

getEnvEndpoint :: ServerM GetEnvResponse
getEnvEndpoint = do
  bch <- readBoxChain
  return $ GetEnvResponse $ getEnv bch

getStateEndpoint :: ServerM BoxChain
getStateEndpoint = readBoxChain

