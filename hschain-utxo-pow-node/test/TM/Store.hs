{-# LANGUAGE OverloadedStrings #-}
-- |
module TM.Store where

import Data.List       (unfoldr)
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Types.Merkle.Types
import HSChain.PoW.Types
import HSChain.PoW.Tests
import Hschain.Utxo.Pow.App.Types

tests :: TestTree
tests = testGroup "Block store"
  [ testCase "Store" $ withHSChainT $ do
      (db,_,_) <- utxoStateView $ head emptyCoinChain
      testIdempotence emptyCoinChain db
  ]

emptyCoinChain :: [Block (UTXOBlock ())]
emptyCoinChain = gen : unfoldr (Just . (\b -> (b,b)) . mineCoin [] . Just) gen
  where
    gen = mineCoin [] Nothing

mineCoin :: [Tx (UTXOBlock ())] -> Maybe (Block (UTXOBlock ())) -> Block (UTXOBlock ())
mineCoin txs mb = GBlock
  { blockHeight = maybe (Height 0) (succ . blockHeight) mb
  , blockTime   = Time 0
  , prevBlock   = blockID <$> mb
  , blockData   = UTXOBlock
    { ubData   = merkled txs
    , ubTarget = Target $ 2^(256::Int)-1
    , ubNonce  = ""
    }
  }
