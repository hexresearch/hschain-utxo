-- | Defines server for hschain-utxo node
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
import Hschain.Utxo.Blockchain

import qualified Hschain.Utxo.State.Query as S

-- | Server implementation for 'UtxoAPI'
utxoServer :: ServerT UtxoAPI ServerM
utxoServer =
       postTxEndpoint                -- posts transaction
  :<|> getBoxEndpoint                -- gets box by id
  :<|> getBoxBalanceEndpoint         -- reads balance for a box
  :<|> getTxSigmaEndpoint            -- executes script to sigma-expression without commiting
  :<|> getEnvEndpoint                -- reads blockchain environment
  :<|> getStateEndpoint              -- reads whole state (for debug only)
  :<|> getUtxosEndpoint              -- reads list of all available UTXOs
  :<|> hasUtxoEndpoint               -- is UTXO exists (available to spend)
  :<|> readBlockEndpoint             -- reads block at the given height
  :<|> readBlockchainHeightEndpoint  -- reads current height of the blockchain

postTxEndpoint :: Tx -> ServerM PostTxResponse
postTxEndpoint tx = fmap PostTxResponse $ postTxWait tx

getBoxEndpoint :: BoxId -> ServerM (Maybe Box)
getBoxEndpoint boxId = fmap (\bch -> S.getBox bch boxId) readBoxChain

getBoxBalanceEndpoint :: BoxId -> ServerM (Maybe Money)
getBoxBalanceEndpoint boxId =
  fmap (\bch -> S.getBoxBalance bch boxId) readBoxChain

getTxSigmaEndpoint :: Tx -> ServerM SigmaTxResponse
getTxSigmaEndpoint tx =
  fmap (\bch -> SigmaTxResponse (execInBoxChain tx bch) "") readBoxChain

getEnvEndpoint :: ServerM GetEnvResponse
getEnvEndpoint = do
  bch <- readBoxChain
  return $ GetEnvResponse $ getEnv bch

getStateEndpoint :: ServerM BoxChain
getStateEndpoint = readBoxChain

getUtxosEndpoint :: ServerM [BoxId]
getUtxosEndpoint = fmap getBoxIds readBoxChain

hasUtxoEndpoint :: BoxId -> ServerM Bool
hasUtxoEndpoint boxId = fmap (\boxChain -> hasBoxId boxChain boxId) readBoxChain

readBlockEndpoint :: Int -> ServerM (Maybe [Tx])
readBlockEndpoint height = readBlock height

readBlockchainHeightEndpoint :: ServerM Int
readBlockchainHeightEndpoint = blockchainSize

