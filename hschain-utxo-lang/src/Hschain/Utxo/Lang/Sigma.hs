module Hschain.Utxo.Lang.Sigma where

import Data.Aeson

import Data.Fix
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Set as S

type PubKey = Text
type PrivateKey = Text

newtype Proof = Proof { unProof :: Set Text }
  deriving (Show, Eq, FromJSON, ToJSON, Semigroup, Monoid)

checkPubKey :: PubKey -> Proof -> Bool
checkPubKey pkey (Proof keys) = S.member pkey keys

type Sigma k = Fix (SigmaExpr k)
type Sigma' = Sigma ()

data SigmaExpr k a =
    SigmaPk k Text
  | SigmaAnd k a a
  | SigmaOr  k a a
  deriving (Functor, Foldable, Traversable, Show, Read, Eq, Ord)

getSigmaTag :: Sigma k -> k
getSigmaTag (Fix x) = case x of
  SigmaPk  k _   -> k
  SigmaAnd k _ _ -> k
  SigmaOr  k _ _ -> k

-- | Not for sigma expressions.
--  Warning: assumption
-- We propose that absense of the key can not be proved.
-- not (pk owner) - evaluates to Fasle for any owner
notSigma :: forall k . Sigma k -> Either Bool (Sigma k)
notSigma = evalConstants . applyNot
  where
    evalConstants :: Sigma (k, Maybe Bool) -> Either Bool (Sigma k)
    evalConstants = cata $ \case
      SigmaAnd (k, mBool) a b -> evalAnd k mBool a b
      SigmaOr  (k, mBool) a b -> evalOr  k mBool a b
      SigmaPk  (k, mBool) a   -> evalPk  k mBool a

    evalAnd k mBool a b = case mBool of
      Just res -> Left res
      Nothing  -> case (a, b) of
        (Left x, y) -> if x then y else Left False
        (x, Left y) -> if y then x else Left False
        (Right x, Right y) -> Right $ Fix $ SigmaAnd k x y

    evalOr k mBool a b = case mBool of
      Just res -> Left res
      Nothing  -> case (a, b) of
        (Left x, y) -> if x then Left True else y
        (x, Left y) -> if y then Left True else x
        (Right x, Right y) -> Right $ Fix $ SigmaOr k x y

    evalPk k mBool key = case mBool of
      Just b  -> Left b
      Nothing -> Right $ Fix $ SigmaPk k key

    applyNot :: Sigma k -> Sigma (k, Maybe Bool)
    applyNot = cata $ \case
      SigmaPk  k key -> Fix $ SigmaPk  (k, Just False) key
      SigmaAnd k a b -> Fix $ SigmaOr  (k, orTag a b) a b
      SigmaOr  k a b -> Fix $ SigmaAnd (k, andTag a b) a b

    orTag a b = case (snd $ getSigmaTag a, snd $ getSigmaTag b) of
      (Just a, b) -> if a then Just True else b
      (a, Just b) -> if b then Just True else a
      _           -> Nothing

    andTag a b = case (snd $ getSigmaTag a, snd $ getSigmaTag b) of
      (Just a, b) -> if a then b else Just False
      (a, Just b) -> if b then a else Just False
      _           -> Nothing


