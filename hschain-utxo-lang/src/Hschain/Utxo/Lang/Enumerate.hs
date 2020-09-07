{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators     #-}
-- |
module Hschain.Utxo.Lang.Enumerate
  ( Enumerated(..)
  ) where

import Data.Coerce
import Data.Void
import GHC.Generics


-- | Type class which allows to enumerate all possible non bottom
--   inhabitans of a type. 
class Enumerated a where
  enumerated :: [a]
  default enumerated :: (Generic a, GEnumerated (Rep a)) => [a]
  enumerated = GHC.Generics.to <$> gEnumerated

instance Enumerated Void where
  enumerated = []

instance Enumerated Bool
instance Enumerated a => Enumerated (Maybe a)
instance (Enumerated a, Enumerated b) => Enumerated (a,b)
instance (Enumerated a, Enumerated b, Enumerated c) => Enumerated (a,b,c
                                                                  )
----------------------------------------------------------------
-- Generic implementation
----------------------------------------------------------------

class GEnumerated f where
  gEnumerated :: [f a]

instance (GEnumerated f) => GEnumerated (M1 i c f) where
  gEnumerated = (coerce `asTypeOf` fmap M1) gEnumerated

instance (GEnumerated f, GEnumerated g) => GEnumerated (f :+: g) where
  gEnumerated = (L1 <$> gEnumerated) ++ (R1 <$> gEnumerated)

instance (GEnumerated f, GEnumerated g) => GEnumerated (f :*: g) where
  gEnumerated = (:*:) <$> gEnumerated <*> gEnumerated

instance (Enumerated a) => GEnumerated (K1 i a) where
  gEnumerated = (coerce `asTypeOf` fmap K1) enumerated

instance GEnumerated U1 where
  gEnumerated = [U1]

instance GEnumerated V1 where
  gEnumerated = []
