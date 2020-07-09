-- | Common primitive type definitions
module Hschain.Utxo.Lang.Core.Data.Prim(
    Name
  , Type
  , Typed(..)
  , Addr
  , Prim(..)
  , SigmaExpr(..)
  , getPrimInt
  , getPrimText
  , getPrimBool
  , getPrimSigma
) where

import Hschain.Utxo.Lang.Sigma (PublicKey)

import Control.DeepSeq
import Data.Int
import Data.Text (Text)
import qualified Language.HM as H
import GHC.Generics (Generic)

type Type = H.Type () Name

-- | Type tags for values
data Typed a = Typed
  { typed'value :: a
  , typed'type  :: Type
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Name identifiers for variables or global functions
type Name = Text

-- | Address on the heap
type Addr = Int

-- | Primitive types
data Prim
  = PrimInt   !Int64
  | PrimText  !Text
  | PrimBool  !Bool
  | PrimSigma !SigmaExpr
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Boolean expressions that include Sigma-expressions
data SigmaExpr
  = SigmaBool Bool         -- ^ constant values
  | SigmaAnd  [SigmaExpr]  -- ^ boolean AND
  | SigmaOr   [SigmaExpr]  -- ^ boolean OR
  | SigmaPk   PublicKey    -- ^ public key ownership
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

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
getPrimSigma :: Prim -> Maybe SigmaExpr
getPrimSigma = \case
  PrimSigma t -> Just t
  _           -> Nothing

