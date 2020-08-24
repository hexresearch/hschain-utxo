{-# OPTIONS_GHC -Wno-orphans #-}
-- | Pretty-printer for values of the language.
module Hschain.Utxo.Lang.Pretty(
    renderDoc
  , renderText
  , prettyRecord
) where

import Hex.Common.Serialise

import Data.Text (Text)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Infer()
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma (Proof)

import qualified Data.Vector as V

import HSChain.Crypto.Classes (encodeBase58)
import qualified Hschain.Utxo.Lang.Parser.Hask as P
import qualified Hschain.Utxo.Lang.Sigma as S

import qualified Language.HM as H
import qualified Language.Haskell.Exts.SrcLoc as Hask

import qualified Text.Show.Pretty as P

-- | Convenience function to render pretty-printable value to text.
renderText :: Pretty a => a -> Text
renderText = renderDoc . pretty

-- | Convenience function to render pretty-printed value to text.
renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

instance Pretty VarName where
  pretty = pretty . varName'name

instance Pretty Lang where
  pretty = pretty . P.prettyExp

instance Pretty BoxId where
  pretty (BoxId txt) = pretty $ encodeBase58 txt

instance Pretty Script where
  pretty = pretty . scriptToText

instance Pretty Box where
  pretty Box{..} = prettyRecord "Box"
      [ ("id"    , pretty box'id)
      , ("value" , pretty box'value)
      , ("script", pretty box'script)
      , ("args"  , prettyArgs box'args) ]

-- | Helper to pretty-print records.
prettyRecord :: Doc ann -> [(Text, Doc ann)] -> Doc ann
prettyRecord name fields = vcat [name <> colon, indent 2 (vsep $ fmap ppField fields)]
  where
    ppField (field, val) = hsep [hcat [pretty field, colon], val ]

prettyArgs :: Args -> Doc ann
prettyArgs Args{..} = prettyRecord "Args"
  [ ("args'ints",  pretty $ V.toList args'ints)
  , ("args'texts", pretty $ V.toList args'texts)
  , ("args'bools", pretty $ V.toList args'bools)
  ]

instance Pretty TxHash where
  pretty (TxHash bs) = pretty $ serialiseToText bs

instance Pretty Tx where
  pretty Tx{..} = prettyRecord "Tx"
    [ ("inputs", brackets $ hsep $ punctuate comma $ V.toList $ fmap pretty tx'inputs )
    , ("outputs", vsep $ fmap pretty $ V.toList tx'outputs )
    ]

instance Pretty TxArg where
  pretty TxArg{..} = prettyRecord "TxArg"
    [ ("inputs",  vsep $ fmap pretty $ V.toList txArg'inputs )
    , ("outputs", vsep $ fmap pretty $ V.toList txArg'outputs )
    ]

instance Pretty BoxInput where
  pretty BoxInput{..} = prettyRecord "BoxInput"
    [ ("box",   pretty boxInput'box)
    , ("args",  prettyArgs boxInput'args)
    , ("proof", pretty boxInput'proof)
    ]

instance Pretty BoxInputRef where
  pretty BoxInputRef{..} = prettyRecord "BoxInputRef"
    [ ("id",    pretty boxInputRef'id)
    , ("args",  prettyArgs boxInputRef'args)
    , ("proof", pretty boxInputRef'proof)
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

instance Pretty (Expr a) where
  pretty (Expr a) = pretty a

instance Pretty ConsName where
  pretty (ConsName _ txt) = pretty txt

instance Pretty Pat where
  pretty = \case
    PVar _ idx  -> pretty idx
    PWildCard _ -> "_"
    PPrim _ p -> pretty p
    PCons _ name args -> parens $ hsep [pretty name, hsep $ fmap pretty args]
    PTuple _ args -> parens $ hsep $ punctuate comma $ fmap pretty args
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
    PrimSigma    s -> pretty $ show s
    PrimBytes    s -> pretty $ encodeBase58 s

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
    GetVar _ ty      -> pretty $ getEnvVarName ty

prettyVec :: Doc ann -> Doc ann -> Doc ann
prettyVec name n = hcat [name, brackets n]

instance Pretty a => Pretty (BoxField a) where
  pretty = prettyBoxField . fmap pretty

prettyBoxField :: BoxField (Doc ann) -> Doc ann
prettyBoxField = \case
    BoxFieldId      -> "id"
    BoxFieldValue   -> "value"
    BoxFieldScript  -> "script"
    BoxFieldArgList tag -> pretty $ getBoxArgVar tag

instance Pretty Error where
  pretty = \case
    ParseError loc txt    -> hsep [hcat [pretty loc, ":"],  "parse error", pretty txt]
    ExecError err         -> pretty err
    TypeError err         -> pretty err
    PatError err          -> pretty err
    InternalError err     -> pretty err
    MonoError err         -> pretty err
    CoreScriptError err   -> pretty err

instance Pretty ExecError where
  pretty = \case
    AppliedNonFunction lang        -> err "Applied non-function" lang
    UnboundVariables vars          -> vcat $ fmap unboundedVar vars
    UndefinedRecordCons loc cons   -> hcat [pretty loc, ": undefined record constructor ", pretty cons]
    UndefinedReocrdField loc cons field
                                   -> hcat [pretty loc, ": undefined record field ", pretty field, " for constructor ", pretty cons]
    ThisShouldNotHappen lang       -> err "This should not happen" lang
    IllegalRecursion lang          -> err "Illegal recursion" lang
    OutOfBound lang                -> err "Out of bound" lang
    NoField txt                    -> err "No field" txt
    Undefined loc                  -> hcat [pretty loc, ": undefined"]
    NonExaustiveCase loc lang      -> hsep [hcat [pretty loc, ":"], err "Non-exaustive case-pattern" lang]
    NoSigmaScript                  -> "Error: Script does not contain main function or does not terminate"
    FailedToDecodeScript           -> "Error: Failed to decode script"
    where
      err msg val = hsep [mconcat [msg, ":"], pretty val]
      unboundedVar VarName{..} = hsep [hcat [pretty varName'loc, ":"], "Unbound variable:", pretty varName'name]

instance Pretty PatError where
  pretty = \case
    NoCasesLeft       -> "No cases letft in the pattern"
    NoVarFound        -> "Var not found in the pattern"
    NoSameArgsNumber  -> "Patterns do not have the same number of arguments for a function"
    EmptyArgument     -> "Pattern has no arguments"
    WrongPatPrimMixture loc -> err loc "Wrong pattern mixture. Primitive is mixed with constructors"
    WrongPatConsMixture loc -> err loc "Wrong pattern mixture. Tuple is mixed with user-constructors"
    where
      err src msg = hsep [hcat [pretty src, ":"], msg]

instance Pretty TypeError where
  pretty = \case
    H.OccursErr src name     -> err src $ hsep ["Occurs error", pretty name]
    H.UnifyErr src tyA tyB   -> err src $ hsep ["Type mismatch got", inTicks $ pretty tyB, "expected", inTicks $ pretty tyA]
    H.NotInScopeErr src name -> err src $ hsep ["Not in scope", pretty name]
    H.SubtypeErr src tyA tyB -> err src $ hsep ["Subtype error", inTicks $ pretty tyB, "expected", inTicks $ pretty tyA]
    H.EmptyCaseExpr src      -> err src $ "Case-expression should have at least one alternative case"
    where
      err src msg = hsep [hcat [pretty src, ":"], msg]
      inTicks x = hcat ["'", x, "'"]

instance Pretty CoreScriptError where
  pretty = \case
    NoMainFunction                 -> "Error: No main function is defined"
    ResultIsNotSigma               -> "Error: Result of execution is not a sigma expression"
    TypeCoreError err              -> pretty err
    NotMonomorphicTypes            -> "Error: Polymorphic type is encountered"
    RecursiveScript                -> "Error: Recursive script is not allowed"

instance Pretty TypeCoreError where
  pretty = \case
    NotMonomorphicType v t   -> hsep ["Error: variable", pretty v, "is not monomorphic. Got type", pretty t]
    VarIsNotDefined v        -> hsep ["Error: variable", pretty v, "is not defined"]
    ArrowTypeExpected t      -> hsep ["Error: arrow type expected, but got", pretty t]
    TypeCoreMismatch ta tb   -> hsep ["Error: type mismatch. Got", pretty ta, "expected", pretty tb]
    SubtypeError ta tb       -> hsep ["Error: subtype error.", pretty ta, "is not a subtype of", pretty tb]
    EmptyCaseExpression      -> "Error: empty case alternatives"
    PolymorphicLet           -> "polymorphic type in the let binding"
    BadEquality ty           -> hsep ["Error: non comparable type:", pretty ty]

instance Pretty InternalError where
  pretty = \case
    FailedToEliminate txt -> hsep ["Failed to eliminate expression:", pretty txt]
    NonIntegerConstrTag txt -> hsep ["Non-integer constr tag after type inference", pretty txt]
    NonLamType -> "Not a lambda argument type"

instance Pretty MonoError where
  pretty = \case
    FailedToFindMonoType loc name -> err loc $ hsep ["Failed to find monomorphic type for", pretty name]
    CompareForNonPrim loc         -> err loc "Compare operator expects primitive type as input."
    where
      err src msg = hsep [hcat [pretty src, ":"], msg]

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

