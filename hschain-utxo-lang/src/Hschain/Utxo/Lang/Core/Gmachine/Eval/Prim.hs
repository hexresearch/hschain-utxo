{-# LANGUAGE RankNTypes #-}
-- | primitive operators
module Hschain.Utxo.Lang.Core.Gmachine.Eval.Prim(
    primOp1
  , primOp2
  , binNumOp
  , negOp
  , compareOp
  , binBoolOp
  , notOp
  , binSigmaOp
  , pkOp
  , boolToSigmaOp
  , binTextOp
  , textLength
  , hashBlake
  , hashSha
  , showInt
  , showBool
) where

import Hex.Common.Text

import Data.Int
import Data.Fix
import Data.Maybe
import Data.Text (Text)

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Core.Gmachine.Eval.Vstack
import Hschain.Utxo.Lang.Core.Gmachine.Monad
import Hschain.Utxo.Lang.Core.Data.Prim
import qualified Data.Text as T

-- generic functions

primOp1 :: (Exec a) -> (b -> Exec ()) -> (a -> b) -> Exec ()
primOp1 unbox box f =
  box =<< fmap f unbox

primOp2 :: (Exec a) -> (Exec b) -> (c -> Exec ()) -> (a -> b -> c) -> Exec ()
primOp2 unboxA unboxB box f = do
  a <- unboxA
  b <- unboxB
  box $ f a b

-- numbers

binNumOp :: (Int64 -> Int64 -> Int64) -> Exec ()
binNumOp = primOp2 popInt popInt putInt

negOp :: Exec ()
negOp = primOp1 popInt putInt negate

-- comparison

compareOp :: (forall a. Ord a => a -> a -> Bool) -> Exec ()
compareOp (#) = do
  pa <- popVstack
  pb <- popVstack
  case (pa,pb) of
    (PrimInt   a, PrimInt   b) -> putBool $ a # b
    (PrimBool  a, PrimBool  b) -> putBool $ a # b
    (PrimText  a, PrimText  b) -> putBool $ a # b
    (PrimBS    a, PrimBS    b) -> putBool $ a # b
    (PrimSigma a, PrimSigma b) -> putBool $ a # b
    _                          -> throwError BadType

-- boolean operators

binBoolOp :: (Bool -> Bool -> Bool) -> Exec ()
binBoolOp = primOp2 popBool popBool putBool

notOp :: Exec ()
notOp = primOp1 popBool putBool not

-- sigma expression operators

binSigmaOp :: (Sigma PublicKey -> Sigma PublicKey -> Sigma PublicKey) -> Exec ()
binSigmaOp = primOp2 popSigma popSigma putSigma

-- | TODO: we have to provide errors or bottoms for G-machine
pkOp :: Exec ()
pkOp = primOp1 (fmap (fromMaybe err . publicKeyFromText) popText) putSigma (Fix . SigmaPk)
  where
    err = error "Text is not valid public key"

boolToSigmaOp :: Exec ()
boolToSigmaOp = primOp1 popBool putSigma (Fix . SigmaBool)

-- text operators

binTextOp :: (Text -> Text -> Text) -> Exec ()
binTextOp = primOp2 popText popText putText

textLength :: Exec ()
textLength = primOp1 popText putInt (fromIntegral . T.length)

hashBlake :: Exec ()
hashBlake = primOp1 popText putText getBlakeHash
  where
    getBlakeHash = undefined

hashSha :: Exec ()
hashSha = primOp1 popText putText getSha256
  where
    getSha256 = undefined

showInt :: Exec ()
showInt = primOp1 popText putText showt

showBool :: Exec ()
showBool = primOp1 popText putText showt


