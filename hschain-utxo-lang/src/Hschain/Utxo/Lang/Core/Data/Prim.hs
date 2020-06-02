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

import Data.Text (Text)
import qualified Language.HM as H

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
  = PrimInt   !Int
  | PrimText  !Text
  | PrimBool  !Bool
  | PrimSigma !SigmaExpr
  deriving (Show, Eq, Ord)

-- | Boolean expressions
-- that include Sigma-expressions
data SigmaExpr
  = SigmaBool Bool                 -- ^ constant values
  | SigmaAnd  SigmaExpr SigmaExpr  -- ^ boolean AND
  | SigmaOr   SigmaExpr SigmaExpr  -- ^ boolean OR
  | SigmaPk   Text                 -- ^ public key ownership
  deriving (Show, Eq, Ord)

-- | Extract integer primitive
getPrimInt :: Prim -> Maybe Int
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

