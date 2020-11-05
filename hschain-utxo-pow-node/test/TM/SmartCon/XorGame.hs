{-# LANGUAGE OverloadedLists            #-}
-- |
module TM.SmartCon.XorGame where

import Control.Monad.Reader

import Data.Boolean

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding ((<*))

import Hschain.Utxo.Lang.Expr (intArgs)
import Hschain.Utxo.Lang.Build
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma.Types (generateKeyPair, KeyPair(..))
import qualified Hschain.Utxo.Lang.Sigma.Protocol as Sigma

import TM.BCH.Util

tests :: TestTree
tests = testGroup "XorGame"
  [
  ]
