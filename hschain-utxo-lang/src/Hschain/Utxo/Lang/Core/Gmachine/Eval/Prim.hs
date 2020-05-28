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
) where

import Data.Text (Text)

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

binNumOp :: (Int -> Int -> Int) -> Exec ()
binNumOp = primOp2 popInt popInt putInt

negOp :: Exec ()
negOp = primOp1 popInt putInt negate

-- comparison

compareOp :: (Int -> Int -> Bool) -> Exec ()
compareOp = primOp2 popInt popInt putBool

-- boolean operators

binBoolOp :: (Bool -> Bool -> Bool) -> Exec ()
binBoolOp = primOp2 popBool popBool putBool

notOp :: Exec ()
notOp = primOp1 popBool putBool not

-- sigma expression operators

binSigmaOp :: (SigmaExpr -> SigmaExpr -> SigmaExpr) -> Exec ()
binSigmaOp = primOp2 popSigma popSigma putSigma

pkOp :: Exec ()
pkOp = primOp1 popText putSigma SigmaPk

boolToSigmaOp :: Exec ()
boolToSigmaOp = primOp1 popBool putSigma SigmaBool

-- text operators

binTextOp :: (Text -> Text -> Text) -> Exec ()
binTextOp = primOp2 popText popText putText

textLength :: Exec ()
textLength = primOp1 popText putInt T.length

hashBlake :: Exec ()
hashBlake = primOp1 popText putText getBlakeHash
  where
    getBlakeHash = undefined

hashSha :: Exec ()
hashSha = primOp1 popText putText getSha256
  where
    getSha256 = undefined

