-- | Test values to check out low-level sigma proof algorithm.
-- use like this
--
-- :l script/Sigma.hs
-- > Test{..} <- setup
module Sgima where

import Data.Fix
import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Sigma

data Test = Test
  { secret1 :: Secret
  , secret2 :: Secret
  , secret3 :: Secret
  , pk1     :: Sigma PublicKey
  , pk2     :: Sigma PublicKey
  , pk3     :: Sigma PublicKey
  , env1    :: ProofEnv
  , env2    :: ProofEnv
  , env3    :: ProofEnv
  }

setup :: IO Test
setup = do
  secret1 <- newSecret
  secret2 <- newSecret
  secret3 <- newSecret
  let env1 = proofEnvFromKeys [getKeyPair secret1]
      env2 = proofEnvFromKeys [getKeyPair secret2]
      env3 = proofEnvFromKeys [getKeyPair secret3]
      pk1  = Fix $ SigmaPk $ getPublicKey secret1
      pk2  = Fix $ SigmaPk $ getPublicKey secret2
      pk3  = Fix $ SigmaPk $ getPublicKey secret3
  return Test{..}

and' as = Fix $ SigmaAnd as
or'  as = Fix $ SigmaOr as


checkProof = do
  Test{..} <- setup
  Right proof <- newProof env1 (or' [pk1, pk2])
  print $ verifyProof proof

