{-# OPTIONS_GHC -Wno-orphans #-}
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

import Codec.Serialise
import Control.DeepSeq

import Data.Fix
import Data.Int
import Data.Text (Text)
import qualified Language.HM as H
import GHC.Generics (Generic)

type Type = H.Type () Name

-- | Type tags for values
data Typed a = Typed
  { typed'value :: a
  , typed'type  :: Type
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

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

-----------------------------------------------------
-- instnaces

-- special proxy type for serialization needs.
data TypeSer
    = VarT Name               -- ^ Variables
    | ConT Name [TypeSer]     -- ^ type constant with list of arguments
    | ArrowT TypeSer TypeSer  -- ^ Special case of ConT that is rendered as ->
    | TupleT [TypeSer]        -- ^ Special case of ConT that is rendered as (,,,)
    | ListT TypeSer           -- ^ Special case of ConT that is rendered as [a]
    deriving (Eq, Ord, Show, Generic)

toTypeSer :: Type -> TypeSer
toTypeSer (H.Type ty) = flip cata ty $ \case
  H.VarT _ v     -> VarT v
  H.ConT _ v xs  -> ConT v xs
  H.ArrowT _ a b -> ArrowT a b
  H.TupleT _ xs  -> TupleT xs
  H.ListT _ t    -> ListT t

fromTypeSer :: TypeSer -> Type
fromTypeSer x = H.Type $ case x of
  VarT v     -> Fix $ H.VarT () v
  ConT v xs  -> Fix $ H.ConT () v $ fmap rec xs
  ArrowT a b -> Fix $ H.ArrowT () (rec a) (rec b)
  TupleT xs  -> Fix $ H.TupleT () $ fmap rec xs
  ListT t    -> Fix $ H.ListT () (rec t)
  where
    rec = H.unType . fromTypeSer

instance Serialise Type where
  encode = encode . toTypeSer
  decode = fmap fromTypeSer decode

instance Serialise TypeSer
instance Serialise Prim
instance Serialise SigmaExpr
instance Serialise a => Serialise (Typed a)
