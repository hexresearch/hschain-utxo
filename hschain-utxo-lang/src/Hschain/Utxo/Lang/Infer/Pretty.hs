-- | Pretty printer for types
module Hschain.Utxo.Lang.Infer.Pretty(
) where

import Data.Bool
import Data.Char
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import qualified Hschain.Utxo.Lang.Expr as E

import Language.HM.Type
import Language.HM.Term
import Language.HM.Pretty

import qualified Data.Map.Strict as Map
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


