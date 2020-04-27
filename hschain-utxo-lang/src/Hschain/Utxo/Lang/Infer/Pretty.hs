module Hschain.Utxo.Lang.Infer.Pretty(
    prettySignature
  , prettyType
) where

import Data.Bool
import Data.Char
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang.Expr hiding (Var, Let, Type, Signature)

import Language.HM.Type
import Language.HM.Term
import Language.HM.Pretty

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

instance Pretty VarName where
  pretty = pretty . varName'name

instance PrintCons VarName where
  printCons VarName{..} args
    | isTupleName varName'name = parens $ hsep $ punctuate comma args
    | otherwise                = hsep $ pretty varName'name : args
    where
      isTupleName name = (pre == "Tuple") && isInt post
        where
          (pre, post) = T.splitAt 5 name
          isInt = T.all isDigit

prettySignature :: Signature VarName -> Doc ann
prettySignature = pretty

prettyType :: Type VarName -> Doc ann
prettyType = pretty

