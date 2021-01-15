-- Module defines tyope for Fiat-Shamir tree.
--
--   Prover Step 7: Convert the tree to a string s for input to the Fiat-Shamir hash function.
--   The conversion should be such that the tree can be unambiguously parsed and restored given the string.
--   For each non-leaf node, the string should contain its type (OR or AND).
--   For each leaf node, the string should contain the Sigma-protocol statement being proven and the commitment.
--   The string should not contain information on whether a node is marked "real" or "simulated",
--   and should not contain challenges, responses, or the real/simulated flag for any node.
module Hschain.Utxo.Lang.Sigma.FiatShamirTree(
    FiatShamir(..)
  , FiatShamirLeaf(..)
  , fiatShamirCommitment
  , toFiatShamir
) where

import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Sigma.DTuple
import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.Protocol
import Hschain.Utxo.Lang.Sigma.Types

import qualified Codec.Serialise          as CBOR
import qualified Data.ByteString.Lazy     as BL


-- | Tree that is used as input to Fiat-Shamir hash function
data FiatShamir a
  = FSLeaf (FiatShamirLeaf a)
  | FSAnd  [FiatShamir a]
  | FSOr   [FiatShamir a]
  deriving (Generic)

-- | Leaf of Fiat-Shamir tree.
data FiatShamirLeaf a
  = FiatShamirLeafDLog
      { fsLeafDLog'public     :: PublicKey a
      , fsLeafDLog'commitment :: Commitment a
      }
  | FiatShamirLeafDTuple
      { fsLeafDTuple'public     :: DTuple a
      , fsLeafDTuple'commitment :: (Commitment a, Commitment a)
      }
      deriving (Generic)

deriving instance ( Show (PublicKey   a)
                  , Show (Commitment  a)
                  ) => Show (FiatShamir a)

deriving instance ( Show (PublicKey   a)
                  , Show (Commitment  a)
                  ) => Show (FiatShamirLeaf a)

instance ( CBOR.Serialise (ECPoint a)
         ) => CBOR.Serialise (FiatShamir a)

instance ( CBOR.Serialise (ECPoint a)
         ) => CBOR.Serialise (FiatShamirLeaf a)


-- | Hash of Fiat-Shamir tree.
fiatShamirCommitment :: (EC a, CBOR.Serialise b) => b -> Challenge a
fiatShamirCommitment = randomOracle . BL.toStrict . CBOR.serialise

-- | Convert sigma-expression to Fiat-Shamir tree.
toFiatShamir
  :: SigmaE k (FiatShamirLeaf a)
  -> FiatShamir a
toFiatShamir = \case
  Leaf _ leaf -> FSLeaf leaf
  AND  _ es   -> FSAnd (toFiatShamir <$> es)
  OR   _ es   -> FSOr  (toFiatShamir <$> es)

