{-# OPTIONS_GHC -Wno-orphans #-}
-- | Common primitive type definitions
module Hschain.Utxo.Lang.Core.Data.Prim(
    Name
  , TypeCore
  , SignatureCore
  , Typed(..)
  , Addr
  , Prim(..)
  , getPrimInt
  , getPrimText
  , getPrimBool
  , getPrimBytes
  , getPrimSigma
) where

import Codec.Serialise
import Control.DeepSeq

import Data.Fix
import Data.Int
import Data.ByteString (ByteString)
import Data.Text.Prettyprint.Doc
import Data.Text (Text)
import qualified Language.HM as H
import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Sigma

import Language.HM (IsVar, stringIntToVar, stringPrettyLetters)
import Language.HM.Pretty (PrintCons(..), HasPrefix(..))

type TypeCore = H.Type () Name
type SignatureCore = H.Signature () Name

-- | Type tags for values
data Typed a = Typed
  { typed'value :: a
  , typed'type  :: TypeCore
  }
  deriving stock    (Show, Eq, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Serialise)

-- | Name identifiers for variables or global functions
type Name = Text

-- | Address on the heap
type Addr = Int

-- | Primitive types
data Prim
  = PrimInt   !Int64
  | PrimText  !Text
  | PrimBytes !ByteString
  | PrimBool  !Bool
  | PrimSigma !(Sigma PublicKey)
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise)

-- | Extract integer primitive
getPrimInt :: Prim -> Maybe Int64
getPrimInt = \case
  PrimInt n -> Just n
  _         -> Nothing

-- | Extract boolean primitive
getPrimBool :: Prim -> Maybe Bool
getPrimBool = \case
  PrimBool b -> Just b
  _          -> Nothing

-- | Extract textual primitive
getPrimText :: Prim -> Maybe Text
getPrimText = \case
  PrimText t -> Just t
  _          -> Nothing

-- | Extract textual primitive
getPrimBytes :: Prim -> Maybe ByteString
getPrimBytes = \case
  PrimBytes t -> Just t
  _           -> Nothing

-- | Extract textual primitive
getPrimSigma :: Prim -> Maybe (Sigma PublicKey)
getPrimSigma = \case
  PrimSigma t -> Just t
  _           -> Nothing

-----------------------------------------------------
-- instnaces

instance IsVar Name where
  intToVar = stringIntToVar
  prettyLetters = stringPrettyLetters

instance HasPrefix Name where
  getFixity = const Nothing

instance PrintCons Name where
  printCons name args = hsep $ pretty name : args

{-
instance PrintCons Text where
  printCons name args
    | isTupleName name = parens $ hsep $ punctuate comma args
    | otherwise        = hsep $ pretty name : args
    where
      isTupleName str = (pre == "Tuple") && isInt post
        where
          (pre, post) = T.splitAt 5 str
          isInt = T.all isDigit
-}

instance Serialise (Fix (H.TypeF () Text))
instance (Serialise loc, Serialise var, Serialise a) => Serialise (H.TypeF loc var a)
instance Serialise TypeCore
--  encode = encode . toTypeSer
--  decode = fmap fromTypeSer decode
