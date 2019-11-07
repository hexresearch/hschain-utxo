module Hschain.Utxo.Lang.Sigma where

import Data.Aeson

import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Set as S

type PubKey = Text
type PrivateKey = Text

newtype Proof = Proof { unProof :: Set Text }
  deriving (Show, Eq, FromJSON, ToJSON, Semigroup, Monoid)

checkPubKey :: PubKey -> Proof -> Bool
checkPubKey pkey (Proof keys) = S.member pkey keys

