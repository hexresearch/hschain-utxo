module Hschain.Utxo.Blockchain.Hschain(
    UtxoAlg
  , UtxoError(..)
  , BData(..)
  , initBoxChain
--  , interpretSpec
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

import Hschain.Utxo.Lang hiding (Height)
import Hschain.Utxo.State.Blockchain.Config
import Hschain.Utxo.State.React
import Hschain.Utxo.State.Types

import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Interpreter as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Types as Sigma

type UtxoAlg = Ed25519 :& SHA512

newtype BData = BData { unBData :: [Tx] }
   deriving stock    (Show,Eq,Generic)
   deriving newtype  (NFData,CryptoHashable,JSON.ToJSON,JSON.FromJSON)
   deriving anyclass (Serialise)

data UtxoError = UtxoError Text
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
   type BChMonad        BData = Either UtxoError
   type Alg             BData = UtxoAlg
   bchLogic                         = utxoLogic
   proposerSelection                = ProposerSelection randomProposerSHA512
   blockTransactions    (BData txs) = txs
   logBlockData         (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs

utxoLogic :: BChLogic (Either UtxoError) BData
utxoLogic = BChLogic{..}
  where
    processTx BChEval{..} = void $ processTransaction bchValue (merkleValue blockchainState)

    processBlock BChEval{..} = do
      st <- foldM (flip processTransaction) (merkleValue blockchainState) $ blockTransactions $ merkleValue $ blockData bchValue
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
{-
interpretSpec
   :: ( MonadDB m BData, MonadFork m, MonadMask m, MonadLogger m
      , MonadTrace m, MonadTMMonitoring m
      , Has x BlockchainNet
      , Has x NodeSpec
      , Has x (Configuration BoxChainConfig))
   => Genesis BData
   -> x
   -> AppCallbacks m BData
   -> m (RunningNode m BData, [m ()])
interpretSpec genesis p cb = do
  conn    <- askConnectionRO
  store   <- newSTMBchStorage $ blockchainState genesis
  mempool <- makeMempool store (ExceptT . return)
  acts <- runNode (getT p :: Configuration BoxChainConfig) NodeDescription
    { nodeValidationKey = p ^.. nspecPrivKey
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb <> nonemptyMempoolCallback mempool
    , nodeRunner        = ExceptT . return
    , nodeStore         = AppStore { appBchState = store
                                   , appMempool  = mempool
                                   }
    , nodeNetwork       = getT p
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = mempool
                  }
    , acts
    )
-}

-- | Genesis block has many field with predetermined content so this
--   is convenience function to create genesis block.
makeGenesis
  :: (Crypto (Alg a), CryptoHashable a)
  => a                          -- ^ Block data
  -> Hashed (Alg a) (BlockchainState a)
  -> ValidatorSet (Alg a)           -- ^ Set of validators for H=0
  -> ValidatorSet (Alg a)           -- ^ Set of validators for H=1
  -> Block a
makeGenesis dat stateHash valSet0 valSet1 = Block
  { blockHeight        = Height 0
  , blockPrevBlockID   = Nothing
  , blockValidators    = merkled valSet0
  , blockNewValidators = merkled valSet1
  , blockData          = merkled dat
  , blockPrevCommit    = Nothing
  , blockEvidence      = merkled []
  , blockStateHash     = stateHash
  }

-------------------------------------------

data BoxChainSpec = BoxChainSpec

initBoxChain :: (Foldable f)
  => f (Validator (Alg BData))
  -> [Tx]
  -> Genesis BData
initBoxChain nodes txs = BChEval
  { bchValue        = genesis
  , validatorSet    = merkled valSet
  , blockchainState = merkled state0
  }
  where
    Right valSet = makeValidatorSet nodes
    genesis = makeGenesis (BData txs) (hashed state0) valSet valSet
    state0 = emptyBoxChain

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


