module Hschain.Utxo.State.Blockchain(

) where

import Data.Foldable

import Codec.Serialise      (Serialise, serialise)
import Control.Applicative
import Control.DeepSeq      (NFData)
import Control.Exception    (Exception)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Except
import Control.Parallel.Strategies
import Data.Fix
import Data.Fixed
import Data.Int
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Vector         as V
import qualified Crypto.ECC.Edwards25519  as Ed

import GHC.Generics (Generic)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto hiding (PublicKey)
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Monitoring
import HSChain.Run
import HSChain.Store
import HSChain.Store.STM
import HSChain.Types
import HSChain.Types.Merkle.Types

import Hschain.Utxo.Lang
import Hschain.Utxo.State.Types

import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Interpreter as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Types as Sigma

type UtxoAlg = Ed25519 :& SHA512

newtype BData = BData [Tx]
   deriving stock    (Show,Eq,Generic)
   deriving newtype  (NFData,CryptoHashable,JSON.ToJSON,JSON.FromJSON)
   deriving anyclass (Serialise)

data UtxoError = UtxoError
   deriving stock    (Show,Generic)
   deriving anyclass (Exception,NFData)

hashDomain :: String
hashDomain = "hschain.utxo.sigma"

deriving instance Generic E12
deriving instance Generic Sigma.Ed25519

instance BlockData BData where
   type TX              BData = Tx
   type BlockchainState BData = BoxChain
   type BChError        BData = UtxoError
   type BChMonad        BData = Maybe
   type Alg             BData = UtxoAlg
   bchLogic                         = utxoLogic
   proposerSelection                = ProposerSelection randomProposerSHA512
   blockTransactions    (BData txs) = txs
   logBlockData         (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs

utxoLogic :: BChLogic Maybe BData
utxoLogic = BChLogic{..}
  where
    processTx     = const empty

    processBlock  = undefined

    generateBlock = undefined


{-
dioLogic :: forall tag. Dio tag => BChLogic Maybe (BData tag)
 dioLogic = BChLogic
   { processTx     = const empty
   --
   , processBlock  = \BChEval{..} -> do
       let sigCheck = guard
                    $ and
                    $ parMap rseq
                      (\(Tx sig tx) -> verifySignatureHashed (txFrom tx) tx sig)
                      (blockTransactions $ merkleValue $ blockData bchValue)
       let update   = foldM (flip process) (merkleValue blockchainState)
                    $ blockTransactions $ merkleValue $ blockData bchValue
       st <- uncurry (>>)
           $ withStrategy (evalTuple2 rpar rpar)
           $ (sigCheck, update)
       return BChEval { bchValue        = ()
                      , blockchainState = merkled st
                      , ..
                      }
   -- We generate one transaction for every key. And since we move
   -- money from one account to another it's quite simple to update state
   , generateBlock = \NewBlock{..} _ -> do
       let nonce = let Height h = newBlockHeight in fromIntegral h - 1
           keys  = dioUserKeys
       return $! BChEval
         { bchValue = BData
                    $ parMap rseq
                      (\(sk,pk) -> let body = TxBody
                                         { txTo     = pk
                                         , txFrom   = pk
                                         , txNonce  = nonce
                                         , txAmount = 1
                                         }
                                   in Tx { txSig  = signHashed sk body
                                         , txBody = body
                                         }
                      )
                      (V.toList keys)
         , validatorSet    = merkled newBlockValSet
         , blockchainState = merkled
                           $ userMap . each . userNonce %~ succ
                           $ merkleValue newBlockState
         }
   }
   where
     DioDict{..} = dioDict @tag

-}

------------------------------------------
-- instance boilerplate

instance CryptoHashable Tx where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxChain where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxId where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Prim where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Script where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Box where
  hashStep = genericHashStep hashDomain

instance CryptoHashable (Sigma PublicKey) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Proof where
  hashStep = genericHashStep hashDomain

instance CryptoHashable (Sigma.ProvenTree CryptoAlg) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable (SigmaExpr PublicKey (Fix (SigmaExpr PublicKey))) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable (Sigma.OrChild CryptoAlg) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable (Sigma.Challenge CryptoAlg) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable (Sigma.ECScalar CryptoAlg) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable (Sigma.ECPoint CryptoAlg) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable PublicKey where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Ed.Point where
  hashStep x = hashStep (Ed.pointEncode x :: ByteString)

instance CryptoHashable Ed.Scalar where
  hashStep x = hashStep (Ed.scalarEncode x :: ByteString)

instance CryptoHashable a => CryptoHashable (Seq a) where
  hashStep = hashStep . toList

instance CryptoHashable Text where
  hashStep = hashStep . serialise

instance CryptoHashable Bool where
  hashStep = hashStep . serialise

instance CryptoHashable Pico where
  hashStep = hashStep . serialise


