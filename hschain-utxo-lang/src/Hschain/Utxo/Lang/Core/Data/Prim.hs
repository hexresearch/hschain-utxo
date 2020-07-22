{-# OPTIONS_GHC -Wno-orphans #-}
-- | Common primitive type definitions
module Hschain.Utxo.Lang.Core.Data.Prim(
    Name
  , TypeCore
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
import Data.Text (Text)
import qualified Language.HM as H
import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Sigma

type TypeCore = H.Type () Name

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

instance Serialise (Fix (H.TypeF () Text))
instance (Serialise loc, Serialise var, Serialise a) => Serialise (H.TypeF loc var a)
instance Serialise TypeCore
--  encode = encode . toTypeSer
--  decode = fmap fromTypeSer decode
