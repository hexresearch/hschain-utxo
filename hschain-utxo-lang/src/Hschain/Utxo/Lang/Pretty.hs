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
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma (Proof)
import Hschain.Utxo.Lang.Exec

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Parser.Hask as P
import qualified Hschain.Utxo.Lang.Sigma as S

import qualified Language.HM as H
import qualified Language.Haskell.Exts.SrcLoc as Hask

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
      , vcat $ fmap (prettyAlt bind'name) bind'alts
      ]

    prettySignature :: VarName -> Signature -> Doc ann
    prettySignature name signature = hsep [ pretty name, "::",  pretty signature]

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
    PrimBool     b -> pretty b
    PrimString   s -> hcat [dquote, pretty s, dquote]

removeZeroes = reverse . skipDot . skipZeroes . reverse
  where
    skipZeroes = dropWhile (== '0')
    skipDot    = dropWhile (== '.')

instance Pretty a => Pretty (EnvId a) where
  pretty = prettyId . fmap pretty

prettyId :: EnvId (Doc ann) -> Doc ann
prettyId = \case
    Height _         -> "HEIGHT"
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
    ParseError loc txt    -> hsep [hcat [pretty loc, ":"],  "parse error", pretty txt]
    ExecError err         -> pretty err
    TypeError err         -> pretty err
    PatternError err      -> pretty err

instance Pretty ExecError where
  pretty = \case
    AppliedNonFunction lang        -> err "Applied non-function" lang
    UnboundVariables vars          -> hsep ["Unbound variables:", hsep $ punctuate comma $ fmap pretty vars]
    ThisShouldNotHappen lang       -> err "This should not happen" lang
    IllegalRecursion lang          -> err "Illegal recursion" lang
    OutOfBound lang                -> err "Out of bound" lang
    NoField txt                    -> err "No field" txt
    Undefined loc                  -> hcat [pretty loc, ": undefined"]
    NonExaustiveCase loc lang      -> hsep [hcat [pretty loc, ":"], err "Non-exaustive case-pattern" lang]
    where
      err msg val = hsep [mconcat [msg, ":"], pretty val]

instance Pretty PatError where
  pretty = \case
    NoCasesLeft       -> "No cases letft in the pattern"
    NoVarFound        -> "Var not found in the pattern"
    NoSameArgsNumber  -> "Patterns do not have the same number of arguments for a function"
    EmptyArgument     -> "Pattern has no arguments"

prettyMap :: (Pretty a, Pretty b) => String -> M.Map a b -> Doc ann
prettyMap name m = hsep [pretty name, indent 2 $ vcat $ fmap (\(k, v) -> hsep [pretty k, ":", pretty v]) $ M.toList m]

instance Pretty TypeError where
  pretty = \case
    H.OccursErr src name ty  -> err src $ hsep ["Occurs error", pretty name, "with type", pretty ty]
    H.UnifyErr src tyA tyB   -> err src $ hsep ["Type mismatch got", inTicks $ pretty tyB, "expected", inTicks $ pretty tyA]
    H.NotInScopeErr src name -> err src $ hsep ["Not in scope", pretty name]
    where
      err src msg = hsep [hcat [pretty src, ":"], msg]
      inTicks x = hcat ["'", x, "'"]

instance Pretty Loc where
  pretty x = pretty $ Hask.srcInfoSpan x

instance Pretty Hask.SrcSpan where
  pretty Hask.SrcSpan{..} = hcat
    [ pretty srcSpanFilename, ":"
    , pretty srcSpanStartLine, ":"
    , pretty srcSpanStartColumn ]

instance Pretty Hask.SrcLoc where
  pretty Hask.SrcLoc{..} = hcat
    [ pretty srcFilename, ":"
    , pretty srcLine, ":"
    , pretty srcColumn ]

