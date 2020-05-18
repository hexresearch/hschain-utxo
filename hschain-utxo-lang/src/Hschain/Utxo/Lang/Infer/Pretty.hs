-- | Pretty printer for types
module Hschain.Utxo.Lang.Infer.Pretty(
) where

import Data.Char
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Language.HM.Pretty

import qualified Data.Text as T

instance PrintCons Text where
  printCons name args
    | isTupleName name = parens $ hsep $ punctuate comma args
    | otherwise        = hsep $ pretty name : args
    where
      isTupleName name = (pre == "Tuple") && isInt post
        where
          (pre, post) = T.splitAt 5 name
          isInt = T.all isDigit


