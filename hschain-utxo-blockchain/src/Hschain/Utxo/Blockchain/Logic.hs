-- | Bridge to hschain logic
module Hschain.Utxo.Blockchain.Logic where

import Codec.Serialise      (Serialise)
import Control.DeepSeq      (NFData)
import Control.Exception
import Control.Monad

import Data.Text (Text)

import GHC.Generics (Generic)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Crypto hiding (PublicKey)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
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
   deriving anyclass (Exception,NFData)


instance BlockData BData where
   type TX              BData = Tx
   type BlockchainState BData = BoxChain
   type BChError        BData = UtxoError
   type BChMonad        BData = Either UtxoError
   type Alg             BData = UtxoAlg
   bchLogic                         = utxoLogic
   proposerSelection                = ProposerSelection randomProposerSHA512
   logBlockData         (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs

utxoLogic :: BChLogic (Either UtxoError) BData
utxoLogic = BChLogic{..}
  where
    processTx BChEval{..} = void $ processTransaction bchValue (merkleValue blockchainState)

    processBlock BChEval{..} = do
      st <- foldM (flip processTransaction) (merkleValue blockchainState)
          $ unBData $ merkleValue $ blockData bchValue
      return BChEval { bchValue        = ()
                     , blockchainState = merkled st
                     , ..
                     }

    generateBlock NewBlock{..} txs = do
      let selectTx c []     = (c,[])
          selectTx c (t:tx) = case processTransaction t c of
                                Left  _  -> selectTx c  tx
                                Right c' -> let (c'', b  ) = selectTx c' tx
                                            in  (c'', t:b)
      let (st', dat) = selectTx (merkleValue newBlockState) txs
      return BChEval { bchValue        = BData dat
                     , validatorSet    = merkled newBlockValSet
                     , blockchainState = merkled st'
                     }

    processTransaction :: Tx -> BoxChain -> Either UtxoError BoxChain
    processTransaction tx st = either (Left . UtxoError) Right $ fst $ react tx st

------------------------------------------
-- instance boilerplate

instance CryptoHashable BoxChain where
  hashStep = genericHashStep hashDomain
