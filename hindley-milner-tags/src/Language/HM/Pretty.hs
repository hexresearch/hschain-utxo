{-# OPTIONS_GHC -Wno-orphans #-}
module Language.HM.Pretty(
) where

import Data.Fix
import Data.Maybe
import Data.Text.Prettyprint.Doc

import Language.HM.Type

import qualified Data.List as L

instance Pretty (Type src) where
  pretty = cata go . unType
    where
      go = \case
        VarT _ var   -> pretty var
        ConT _ name  -> pretty name
        AppT _ a b   ->
          if isComposite b
            then hsep [a, parens b]
            else hsep [a, b]
        ArrowT _ a b -> hsep [a, "->", b]

      isComposite a = isJust $ L.find (== ' ') $ show a

