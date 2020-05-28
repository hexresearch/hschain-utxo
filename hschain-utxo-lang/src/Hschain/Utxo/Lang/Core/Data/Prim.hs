-- | Common primitive type definitions
module Hschain.Utxo.Lang.Core.Data.Prim(
    Name
  , Addr
  , Prim(..)
  , BoolExpr(..)
  , getPrimInt
  , getPrimText
  , getPrimBool
) where

import Data.Text (Text)

-- | Name identifiers for variables or global functions
type Name = Text

-- | Address on the heap
type Addr = Int

-- | Primitive types
data Prim
  = PrimInt  !Int
  | PrimText !Text
  | PrimBool !BoolExpr
  deriving (Show, Eq, Ord)

-- | Boolean expressions
-- that include Sigma-expressions
data BoolExpr
  = BoolValue Bool              -- ^ constant values
  | BoolAnd BoolExpr BoolExpr   -- ^ boolean AND
  | BoolOr  BoolExpr BoolExpr   -- ^ boolean OR
  | BoolXor BoolExpr BoolExpr   -- ^ boolean XOR
  | BoolNot BoolExpr            -- ^ boolean NOT
  | Pk Text                     -- ^ public key ownership
  deriving (Show, Eq, Ord)

-- | Extract integer primitive
getPrimInt :: Prim -> Maybe Int
getPrimInt = \case
  PrimInt n -> Just n
  _         -> Nothing

-- | Extract boolean primitive
getPrimBool :: Prim -> Maybe BoolExpr
getPrimBool = \case
  PrimBool b -> Just b
  _          -> Nothing

-- | Extract textual primitive
getPrimText :: Prim -> Maybe Text
getPrimText = \case
  PrimText t -> Just t
  _          -> Nothing

