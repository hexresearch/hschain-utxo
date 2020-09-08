{-# OPTIONS_GHC -Wno-orphans #-}
-- | Common primitive type definitions
module Hschain.Utxo.Lang.Core.Data.Prim(
    Name
  , TypeCore
  , SignatureCore
  , Typed(..)
  , Prim(..)
    -- * Lens
  , typed'typeL
  , typed'valueL
  ) where

import Codec.Serialise
import Control.DeepSeq
import Control.Lens

import Data.Fix
import Data.Int
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Language.HM as H
import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Sigma

import Language.HM (IsVar, stringIntToVar, stringPrettyLetters)
import Language.HM.Pretty (HasPrefix(..))

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

-- | Primitive types
data Prim
  = PrimInt   !Int64
  | PrimText  !Text
  | PrimBytes !ByteString
  | PrimBool  !Bool
  | PrimSigma !(Sigma PublicKey)
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise)

-----------------------------------------------------
-- instnaces

instance IsVar Name where
  intToVar = stringIntToVar
  prettyLetters = stringPrettyLetters

instance HasPrefix Name where
  getFixity = const Nothing

instance Serialise (Fix (H.TypeF () Text))
instance (Serialise loc, Serialise var, Serialise a) => Serialise (H.TypeF loc var a)
instance Serialise TypeCore

$(makeLensesWith
   (defaultFieldRules & lensField .~ (mappingNamer (\nm -> [nm++"L"])))
   ''Typed)
