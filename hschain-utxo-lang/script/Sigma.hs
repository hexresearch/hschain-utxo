-- | Test values to check out low-level sigma proof algorithm.
-- use like this
--
-- :l script/Sigma.hs
-- > Test{..} <- setup
module Sgima where

import Data.Fix
import Hschain.Utxo.Lang.Sigma

data Test = Test
  { secret1 :: Secret
  , secret2 :: Secret
  , pk1     :: Sigma PublicKey
  , pk2     :: Sigma PublicKey
  , env1    :: ProofEnv
  , env2    :: ProofEnv
  }

setup :: IO Test
setup = do
  secret1 <- newSecret
  secret2 <- newSecret
  let env1 = proofEnvFromKeys [getKeyPair secret1]
      env2 = proofEnvFromKeys [getKeyPair secret2]
      pk1  = Fix $ SigmaPk $ getPublicKey secret1
      pk2  = Fix $ SigmaPk $ getPublicKey secret2
  return Test{..}

and' a b = Fix $ SigmaAnd a b
or'  a b = Fix $ SigmaOr a b


