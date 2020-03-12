module Hschain.Utxo.Lang.Pretty(
    renderText
  , prettyRecord
) where

import Hex.Common.Control
import Hex.Common.Serialise

import Data.Fix
import Data.Functor.Compose
import Data.Text (Text)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma (Proof)
import Hschain.Utxo.Lang.Exec

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Parser.Hask as P
import qualified Hschain.Utxo.Lang.Sigma as S

import qualified Text.Show.Pretty as P

renderText :: Pretty a => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty


instance Pretty Lang where
  pretty = pretty . P.prettyExp

instance Pretty BoxId where
  pretty (BoxId txt) = pretty txt

instance Pretty Script where
  pretty = either err pretty . fromScript
    where
      err = pretty . mappend "Failed to parse script: "

instance Pretty Box where
  pretty Box{..} = prettyRecord "Box"
      [ ("id"    , pretty box'id)
      , ("value" , pretty box'value)
      , ("script", pretty box'script)
      , ("args"  , prettyArgs box'args) ]

prettyRecord :: Doc ann -> [(Text, Doc ann)] -> Doc ann
prettyRecord name fields = vcat [name <> colon, indent 2 (vsep $ fmap ppField fields)]
  where
    ppField (field, val) = hsep [hcat [pretty field, colon], val ]

prettyArgs :: Args -> Doc ann
prettyArgs m = hsep $ punctuate comma $ fmap phi $ M.toList m
    where
      phi (uid, pk) = hsep [pretty uid, "->", pretty pk]

instance Pretty TxHash where
  pretty (TxHash bs) = pretty $ serialiseToText bs

instance Pretty Tx where
  pretty Tx{..} = prettyRecord "Tx"
    [ ("inputs", brackets $ hsep $ punctuate comma $ V.toList $ fmap pretty tx'inputs )
    , ("outputs", vsep $ fmap pretty $ V.toList tx'outputs )
    , ("proof", pretty tx'proof)
    , ("args", prettyArgs tx'args)
    ]

instance Pretty TxArg where
  pretty TxArg{..} = prettyRecord "TxArg"
    [ ("inputs",  vsep $ fmap pretty $ V.toList txArg'inputs )
    , ("outputs", vsep $ fmap pretty $ V.toList txArg'outputs )
    , ("proof", pretty txArg'proof)
    , ("args", prettyArgs txArg'args)
    ]

instance Pretty Env where
  pretty Env{..} = prettyRecord "Env" [("height", pretty env'height)]

instance Pretty UserId where
  pretty (UserId txt) = pretty txt

-- TODO
instance Pretty Proof where
  pretty proof = pretty $ P.ppShow proof

-- TODO
instance Pretty (S.Sigma S.PublicKey) where
  pretty = undefined -- (Proof m) = hsep $ punctuate comma $ fmap pretty $ S.toList m

op1 :: Doc ann -> Doc ann -> Doc ann
op1 name a = hcat [name, parens a]

op2 :: Doc ann -> Doc ann -> Doc ann -> Doc ann
op2 name a b = parens $ hsep [a, name, b]

fun2 :: Doc ann -> Doc ann -> Doc ann -> Doc ann
fun2 name a b = parens $ hsep [name, a, b]

fun3 :: Doc ann -> Doc ann -> Doc ann -> Doc ann -> Doc ann
fun3 name a b c = parens $ hsep [name, a, b, c]

instance Pretty (Expr a) where
  pretty (Expr a) = pretty a

instance Pretty VarName where
  pretty (VarName _ txt) = pretty txt

prettyBinds :: BindGroup (Doc ann) -> Doc ann
prettyBinds bs = vcat $ fmap prettyBind bs
  where
    prettyBind :: Bind (Doc ann) -> Doc ann
    prettyBind Bind{..} = vcat
      [ maybe mempty (prettySignature bind'name) bind'type
      , prettyAlts bind'name bind'alts
      ]

    prettySignature :: VarName -> Signature -> Doc ann
    prettySignature name signature = hsep [ pretty name, "::",  pretty signature]

    prettyAlts :: VarName -> [Alt (Doc ann)] -> Doc ann
    prettyAlts name as = vcat $ fmap (prettyAlt name) as

    prettyAlt :: VarName -> Alt (Doc ann) -> Doc ann
    prettyAlt name Alt{..} =
      hsep [ pretty name, hsep $ fmap pretty alt'pats, "=", alt'expr]

instance Pretty Pat where
  pretty = \case
    PVar _ idx  -> pretty idx
    -- PWildcard _ -> "_"
    -- PLit _ p    -> pretty p

instance Pretty UnOp where
  pretty = \case
    Not -> "not"
    Neg -> "negate"
    TupleAt size n -> op1 (pretty $ mconcat ["tuple", show size, "-at"]) (pretty n)

instance Pretty BinOp where
  pretty = \case
    And       -> "&&"
    Or        -> "||"
    Plus      -> "+"
    Minus     -> "-"
    Times     -> "*"
    Div       -> "/"
    Equals    -> "=="
    NotEquals -> "/="
    LessThan  -> "<"
    GreaterThan -> ">"
    LessThanEquals -> "<="
    GreaterThanEquals -> ">="

instance Pretty Prim where
  pretty = \case
    PrimInt      n -> pretty n
    PrimDouble   d -> pretty d
    PrimBool     b -> pretty b
    PrimString   s -> hcat [dquote, pretty s, dquote]

instance Pretty Money where
  pretty m = pretty $ removeZeroes $ show m

removeZeroes = reverse . skipDot . skipZeroes . reverse
  where
    skipZeroes = dropWhile (== '0')
    skipDot    = dropWhile (== '.')

instance Pretty a => Pretty (EnvId a) where
  pretty = prettyId . fmap pretty

prettyId :: EnvId (Doc ann) -> Doc ann
prettyId = \case
    Height _ -> "HEIGHT"
    Input _  a       -> prettyVec "input"  a
    Output _ a       -> prettyVec "output" a
    Inputs _         -> "INPUTS"
    Outputs _        -> "OUTPUTS"
    Self _           -> hcat ["SELF"]
    GetVar _ a       -> op1 "getVar" a

prettyVec :: Doc ann -> Doc ann -> Doc ann
prettyVec name n = hcat [name, brackets n]

instance Pretty a => Pretty (BoxField a) where
  pretty = prettyBoxField . fmap pretty

prettyBoxField :: BoxField (Doc ann) -> Doc ann
prettyBoxField = \case
    BoxFieldId      -> "id"
    BoxFieldValue   -> "value"
    BoxFieldScript  -> "script"
    BoxFieldArg txt -> txt

instance Pretty Error where
  pretty = \case
    ParseError _ txt               -> err "Parse error" txt
    AppliedNonFunction lang        -> err "Applied non-function" lang
    PoorlyTypedApplication lang    -> err "Poorly typed application" lang
    UnboundVariables vars          -> hsep ["Unbound variables:", hsep $ punctuate comma $ fmap pretty vars]
    MismatchedBranches lang        -> err "Mismatched branches" lang
    NonBooleanCond lang            -> err "Non-boolean condition" lang
    ThisShouldNotHappen lang       -> err "This should not happen" lang
    BadUnaryOperator lang          -> err "Bad Unary operator" lang
    BadBinaryOperator lang         -> err "Bad binary operator" lang
    BadTypeAscription lang         -> err "Bad type ascription" lang
    IllegalRecursion lang          -> err "Illegal recursion" lang
    OutOfBound lang                -> err "Out of bound" lang
    NoField txt                    -> err "No field" txt
    where
      err msg val = hsep [mconcat [msg, ":"], pretty val]

prettyMap :: (Pretty a, Pretty b) => String -> M.Map a b -> Doc ann
prettyMap name m = hsep [pretty name, indent 2 $ vcat $ fmap (\(k, v) -> hsep [pretty k, ":", pretty v]) $ M.toList m]

