{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- |
module TM.Infer (tests) where

import Control.Monad
import Data.Text (Text)
import Data.String (IsString(..))
import Data.Text.Prettyprint.Doc
import Test.Tasty
import Test.Tasty.HUnit

import Language.HM
import Language.HM.Pretty

tests :: TestTree
tests = testGroup "infer"
  [ testCase "SKI:I"
  $ Right (var "a" --> var "a") @=? fmap fst (inferTerm mempty termI)
  , testCase "SKI:K"
  $ Right (var "a" --> (var "b" --> var "a")) @=? fmap fst (inferTerm mempty termK)
  , testCase "let-chain-case"
  $ Right (var "a" --> var "a") @=? fmap fst (inferTerm mempty termLetChain)
  , testCase "let-rec-chain-case"
  $ Right (var "a" --> var "a") @=? fmap fst (inferTerm mempty termLetRecChain)
  ]
  where
    a --> b = arrowT () a b
    var     = varT ()

----------------------------------------------------------------
data NoPrim
  deriving (Show)

instance IsPrim NoPrim where
  type PrimLoc NoPrim = ()
  type PrimVar NoPrim = Text
  getPrimType _ = error "No primops"

-- I combinator
termI,termK :: Term NoPrim () Text
termI = lamE () "x" $ varE () "x"
termK = lamE () "x" $ lamE () "y" $ varE () "x"

termLetChain :: Term NoPrim () Text
termLetChain = lamE () "x" $ letE ()
  [ Bind () "a" $ varE () "x"]
    (letE ()
      [ Bind () "b" $ varE () "a" ]
      (varE () "b"))

termLetRecChain :: Term NoPrim () Text
termLetRecChain = lamE () "x" $ letRecE ()
  [ Bind () "a" $ varE () "x"
  , Bind () "b" $ varE () "a"
  ]
  (varE () "b")

----------------------------------------------------------------
-- Orphans

instance IsVar Text where
  intToVar n = fromString $ "$$" ++ show n
  prettyLetters = fmap fromString $ [1..] >>= flip replicateM ['a'..'z']

instance HasPrefix Text where
  getFixity = const Nothing

instance PrintCons Text where
  printCons name args = hsep $ pretty name : args


