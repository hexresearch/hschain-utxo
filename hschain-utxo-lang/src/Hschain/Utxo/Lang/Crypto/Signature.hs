-- | Signatures based on Ed25519 (Schnorr signatures are used)
module Hschain.Utxo.Lang.Crypto.Signature(
    Signature
  , sign
  , verify
) where

import Hex.Common.Aeson

import Control.Applicative
import Codec.Serialise
import Control.DeepSeq

import GHC.Generics


import HSChain.Crypto (ByteRepr(..))
import HSChain.Crypto.Classes.Hash
import Hschain.Utxo.Lang.Sigma (CryptoAlg, PublicKey, Secret, SigMessage)
import Hschain.Utxo.Lang.Sigma.EllipticCurve (EC(..), hashDomain)
import Hschain.Utxo.Lang.Sigma.Types (Response)

import qualified Data.ByteString as B

import qualified Hschain.Utxo.Lang.Sigma.Types as Sigma

-- | Signature.
data Signature = Signature
  { signature'commitment :: ECPoint CryptoAlg
  , signature'response   :: Response CryptoAlg
  }
  deriving (Show, Eq, Ord, Generic, NFData, Serialise)

instance ByteRepr Signature where
  encodeToBS (Signature commitment response) = encodeToBS commitment <> encodeToBS response

  decodeFromBS bs = liftA2 Signature (decodeFromBS commitmentBS) (decodeFromBS responseBS)
    where
      (commitmentBS, responseBS) = B.splitAt 32 bs

-- | Signs message.
sign :: Secret -> SigMessage -> IO Signature
sign (Sigma.Secret privKey) msg = do
  k <- generateScalar
  let commitment = fromGenerator k
      challenge = randomOracle $ encodeToBS commitment <> encodeToBS msg
      response = k .+. fromChallenge challenge .*. privKey
  return $ Signature
    { signature'commitment = commitment
    , signature'response   = response }

-- | Verifies signed message
verify :: PublicKey -> Signature -> SigMessage -> Bool
verify (Sigma.PublicKey pubKey) (Signature commitment response) msg =
  fromGenerator response == commitment ^+^ (ch .*^ pubKey )
  where
    ch = fromChallenge $ randomOracle $ encodeToBS commitment <> encodeToBS msg

-----------------------------------
-- instances

instance CryptoHashable Signature where
  hashStep = genericHashStep hashDomain

$(deriveJSON dropPrefixOptions ''Signature)

