{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Helpers for generics
module Hex.Common.Generics(
    constrName
  ) where

import GHC.Generics

-- | Get constructor name for given type with `Generic` instance
constrName :: (HasConstructor (Rep a), Generic a)=> a -> String
constrName = genericConstrName . from

class HasConstructor (f :: * -> *) where
  genericConstrName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstrName (M1 x) = genericConstrName x
  {-# INLINE genericConstrName #-}

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstrName (L1 l) = genericConstrName l
  genericConstrName (R1 r) = genericConstrName r
  {-# INLINE genericConstrName #-}

instance Constructor c => HasConstructor (C1 c f) where
  genericConstrName x = conName x
  {-# INLINE genericConstrName #-}
