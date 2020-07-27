-- | Generates initial genesis with all funds owned by single user.
module Main where

import Data.Aeson.Encode.Pretty
import Data.Fix

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

import Data.ByteString.Char8 as B
import Data.ByteString.Lazy as LB
import qualified Data.Vector as V

main :: IO ()
main = B.putStrLn . LB.toStrict . encodePretty =<< singleOwnerGenesis

singleOwnerGenesis :: IO [Tx]
singleOwnerGenesis = withSecret =<< newSecret
  where
    withSecret secret = do
      Right proof <- newProof env (Fix $ SigmaPk publicKey)
      return $ [tx proof]
      where
        publicKey = getPublicKey secret
        env = proofEnvFromKeys [getKeyPair secret]

        box = Box
          { box'id     = BoxId "master:box-0"
          , box'value  = initMoney
          , box'script = mainScriptUnsafe $ pk' publicKey
          , box'args   = mempty
          }

        tx proof = Tx
          { tx'inputs  = V.empty
          , tx'outputs = V.fromList [box]
          , tx'proof   = Just proof
          , tx'args    = mempty
          }

        initMoney = 1000000









