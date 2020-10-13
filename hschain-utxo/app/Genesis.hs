-- | Generates initial genesis with all funds owned by single user.
module Main where

import Data.Aeson.Encode.Pretty

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

import Data.ByteString.Char8 as B
import Data.ByteString.Lazy as LB
import qualified Data.Vector as V

main :: IO ()
main = B.putStrLn . LB.toStrict . encodePretty =<< singleOwnerGenesis

singleOwnerGenesis :: IO [Tx]
singleOwnerGenesis = fmap withSecret newSecret
  where
    withSecret secret = return $ Tx
      { tx'inputs  = V.empty
      , tx'outputs = V.fromList [box]
      }
      where
        publicKey = getPublicKey secret

        box = Box
          { box'value  = initMoney
          , box'script = mainScriptUnsafe $ pk' publicKey
          , box'args   = mempty
          }

        initMoney = 1000000









