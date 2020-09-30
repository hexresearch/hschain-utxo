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
    withSecret secret = return $ newTx $ Tx
      { tx'inputs  = V.empty
      , tx'outputs = V.fromList [box]
      }
      where
        publicKey = getPublicKey secret

        box = PreBox
          { preBox'value  = initMoney
          , preBox'script = mainScriptUnsafe $ pk' publicKey
          , preBox'args   = mempty
          }

        initMoney = 1000000









