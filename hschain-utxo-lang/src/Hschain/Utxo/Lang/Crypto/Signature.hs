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

import HSChain.Crypto (ByteRepr(..),CryptoAsymmetric(..))
import HSChain.Crypto.Classes.Hash
import Hschain.Utxo.Lang.Sigma (CryptoAlg, PublicKey, Secret, SigMessage)
import Hschain.Utxo.Lang.Sigma.EllipticCurve (EC(..), ECPoint, Response, hashDomain)

import qualified Data.ByteString as B

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
sign privKey msg = do
  k <- generatePrivKey
  let commitment = publicKey k
      challenge = randomOracle $ encodeToBS commitment <> encodeToBS msg
      response = k .+. fromChallenge challenge .*. privKey
  return $ Signature
    { signature'commitment = commitment
    , signature'response   = response }

-- | Verifies signed message
verify :: PublicKey -> Signature -> SigMessage -> Bool
verify pubKey (Signature commitment response) msg =
  publicKey response == commitment ^+^ (ch .*^ pubKey )
  where
    ch = fromChallenge $ randomOracle $ encodeToBS commitment <> encodeToBS msg

-----------------------------------
-- instances

instance CryptoHashable Signature where
  hashStep = genericHashStep hashDomain

$(deriveJSON dropPrefixOptions ''Signature)

