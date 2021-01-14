-- | Module defines main types for sigma-expressions.
module Hschain.Utxo.Lang.Sigma.Types where

import HSChain.Crypto
import Hschain.Utxo.Lang.Sigma.EllipticCurve

-- | Pair of keys.
data KeyPair a = KeyPair
  { getSecretKey :: PrivKey a
  , getPublicKey :: PublicKey a
  }

type Commitment a = ECPoint a
type Response a   = ECScalar a

-- | Generate key-pair.
generateKeyPair :: EC a => IO (KeyPair a)
generateKeyPair = do
  s <- generatePrivKey
  return $ KeyPair s (publicKey s)

getCommitment :: EC a => Response a -> Challenge a -> PublicKey a -> Commitment a
getCommitment z ch pk = publicKey z ^+^ negateP (fromChallenge ch .*^ pk)
