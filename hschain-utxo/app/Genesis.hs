module Main where

import Data.Aeson.Encode.Pretty
import Data.Default

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

import Data.ByteString.Char8 as B
import Data.ByteString.Lazy as LB
import Data.ByteString.Char8 as LB
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

main :: IO ()
main = B.putStrLn $ LB.toStrict $ encodePretty $ singleOwnerGenesis


singleOwnerGenesis :: [Tx]
singleOwnerGenesis = pure $ Tx
  { tx'inputs  = V.empty
  , tx'outputs = V.fromList [box]
  , tx'proof   = mempty
  , tx'args    = mempty
  }
  where
    box = Box
      { box'id     = BoxId "master:box-0"
      , box'value  = initMoney
      , box'script = toScript $ pk (text publicKey)
      , box'args   = M.empty
      }

    userId = UserId "master"
    publicKey = "master"
    privateKey = "master-private-key"
    initMoney = 1000000









