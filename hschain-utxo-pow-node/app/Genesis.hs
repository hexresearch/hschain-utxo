-- | Generates initial genesis with all funds owned by single user.
module Main where

import Data.Aeson.Encode.Pretty
import Data.Aeson
import Data.Default
import Data.Fix

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build

import Data.ByteString.Char8 as B
import Data.ByteString.Lazy as LB
import Data.ByteString.Char8 as LB
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import HSChain.Types.Merkle.Types
import qualified HSChain.PoW.Types as POWTypes
import Hschain.Utxo.Pow.App.Types

main :: IO ()
main = B.putStrLn . LB.toStrict . encodePretty =<< singleOwnerGenesis

singleOwnerGenesis :: IO (POWTypes.Block UTXOBlock)
singleOwnerGenesis = withSecret =<< newSecret
  where
    withSecret secret = do
      time <- POWTypes.getCurrentTime
      let blk = POWTypes.GBlock
                                  { POWTypes.blockHeight = POWTypes.Height 0
                                  , POWTypes.blockTime   = POWTypes.Time 0
                                  , POWTypes.prevBlock   = Nothing
                                  , POWTypes.blockData   = UTXOBlock
                                      { ubNonce = ""
                                      , ubProper = UTXOBlockProper
                                          { ubpPrevious  = Nothing
                                          , ubpData      = merkled []
                                          , ubpTime      = time
                                          , ubpTarget    = POWTypes.Target $ 2^256 - 1
                                          }
                                      }
                                  }
      return blk








