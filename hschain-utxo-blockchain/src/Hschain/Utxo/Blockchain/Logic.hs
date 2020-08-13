-- | Bridge to hschain logic
module Hschain.Utxo.Blockchain.Logic where

import Codec.Serialise      (Serialise)
import Control.DeepSeq      (NFData)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Either
import Data.Foldable
import Data.IORef
import Data.Text (Text)

import GHC.Generics (Generic)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Crypto hiding (PublicKey)
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Mempool
import HSChain.Internal.Types.Consensus
import HSChain.Types
import HSChain.Types.Merkle.Types

import Hschain.Utxo.Lang hiding (Height)
import Hschain.Utxo.State.Types
import Hschain.Utxo.State.React

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM



type UtxoAlg = Ed25519 :& SHA512

newtype BData = BData { unBData :: [Tx] }
   deriving stock    (Show,Eq,Generic)
   deriving newtype  (NFData,CryptoHashable,JSON.ToJSON,JSON.FromJSON)
   deriving anyclass (Serialise)

data UtxoError = UtxoError Text
   deriving stock    (Show,Generic)
   deriving anyclass (Exception,NFData,JSON.ToJSON)


instance BlockData BData where
   type TX       BData = Tx
   type BChError BData = UtxoError
   type Alg      BData = UtxoAlg
   proposerSelection = ProposerSelection randomProposerSHA512
   logBlockData (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs

inMemoryView
  :: (MonadIO m)
  => ValidatorSet (Alg BData)
  -> m (StateView m BData, IORef BoxChain, [m ()])
inMemoryView valSet0 = do
  stateRef <- liftIO $ newIORef initialState
  (mem@Mempool{..}, memThr) <- newMempool (const True)
  let make mh valSet txList st = r where
        r = StateView
          { stateHeight   = mh
          , newValidators = valSet
          , commitState   = do
              liftIO $ writeIORef stateRef st
              removeTxByHashes $ hashed <$> txList
              startMempoolFiltering $ \tx -> return $ isRight $ processTransaction tx st
              return r
            --
          , validatePropBlock = \b vals -> do
              let txs = unBData $ merkleValue $ blockData b
                  st' = foldM (flip processTransaction) st txs
              return $ make (Just $ blockHeight b) vals txs <$> st'
            --
          , generateCandidate = \NewBlock{..} -> do
              memSt <- getMempoolState
              let selectTx c []     = (c,[])
                  selectTx c (t:tx) = case processTransaction t c of
                    Left  _  -> selectTx c  tx
                    Right c' -> let (c'', b  ) = selectTx c' tx
                                in  (c'', t:b)
              let (st', dat) = selectTx st
                             $ map merkleValue
                             $ toList
                             $ mempFIFO memSt
              return
                ( BData dat
                , make (Just newBlockHeight) newBlockValSet dat st'
                )
          , stateMempool = mem
          }
  return ( make Nothing valSet0 [] initialState
         , stateRef
         , [memThr]
         )
  where
    initialState = BoxChain mempty 0


processTransaction :: Tx -> BoxChain -> Either UtxoError BoxChain
processTransaction tx st = either (Left . UtxoError) Right $ fst $ react tx st
