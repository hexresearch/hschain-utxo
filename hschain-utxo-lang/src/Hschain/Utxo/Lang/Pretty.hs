module Hschain.Utxo.Lang.Pretty(
    renderText
  , prettyRecord
) where

import Hex.Common.Control

import Data.Fix
import Data.Functor.Compose
import Data.Text (Text)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma (Proof(..))
import Hschain.Utxo.Lang.Exec

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Sigma as S

import Type.Type
import Type.Pretty

renderText :: Pretty a => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty

instance Pretty Lang where
  pretty = cata prettyE

instance Pretty BoxId where
  pretty (BoxId txt) = pretty txt

instance Pretty Script where
  pretty = maybe err pretty . fromScript
    where
      err = "Failed to parse script"

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
  pretty (TxHash txt) = pretty txt

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

instance Pretty Proof where
  pretty (Proof m) = hsep $ punctuate comma $ fmap pretty $ S.toList m

op1 :: Doc ann -> Doc ann -> Doc ann
op1 name a = hcat [name, parens a]

op2 :: Doc ann -> Doc ann -> Doc ann -> Doc ann
op2 name a b = parens $ hsep [a, name, b]

fun2 :: Doc ann -> Doc ann -> Doc ann -> Doc ann
fun2 name a b = parens $ hsep [name, a, b]

fun3 :: Doc ann -> Doc ann -> Doc ann -> Doc ann -> Doc ann
fun3 name a b c = parens $ hsep [name, a, b, c]

instance Pretty (Expr a) where
  pretty (Expr a) = cata prettyE a

instance Pretty VarName where
  pretty (VarName _ txt) = pretty txt

prettyE :: E (Doc ann) -> Doc ann
prettyE = \case
  Var _ varName     -> pretty varName
  Apply _ a b       -> hsep [parens a, parens b]
  Lam _ name a      -> hsep [hcat ["\\", pretty name], "->", a]
  LamList _ names a -> hsep [hcat ["\\", hsep $ fmap (\var -> parens $ hsep [pretty var]) names], "->", a]
  Let _ bg a        -> vcat [ hsep ["let", indent 4 $ prettyBinds bg ]
                            , hsep ["in  ", a ]
                            ]
  LetRec _ name a b -> vcat [ hsep ["let rec", pretty name, "=", a]
                            , b ]
  Ascr _ a ty       -> parens $ hsep [a, ":", pretty ty]
  -- primitives
  PrimE _   p       -> pretty p
  -- logic
  If _ cond t e     -> vcat [ hsep ["if", cond]
                            , indent 2 $ vcat [ hsep ["then", t]
                                              , hsep ["else", e]
                                              ]
                            ]
  Pk _ a            -> op1 "pk" a
  -- tuples
  Tuple _ as        -> parens $ hsep $ punctuate comma $ V.toList as
  -- operations
  UnOpE _ uo a      -> op1 (pretty uo) a
  BinOpE _ bo a b   -> op2 (pretty bo) a b
  -- environment
  GetEnv _ idx      -> prettyId idx
  -- vectors
  VecE _ v          -> prettyVecExpr v
  -- text
  TextE _ txt       -> prettyTextExpr txt
  -- boxes
  BoxE _ box        -> prettyBoxExpr box
  -- undef
  Undef _           -> "undefined"
  -- debug
  Trace _ a b       -> parens $ hsep ["trace", a, b]

prettyBinds :: BindGroup (Doc ann) -> Doc ann
prettyBinds BindGroup{..} = vcat
  [ vcat $ fmap prettyExpl bindGroup'expl
  , vcat $ concat $ fmap2 prettyImpl $ bindGroup'impl]
  where
    prettyExpl :: Expl (Doc ann) -> Doc ann
    prettyExpl Expl{..} = vcat
      [ prettySignature (toVarName expl'name) expl'type
      , prettyAlts (toVarName expl'name) expl'alts
      ]

    prettyImpl :: Impl (Doc ann) -> Doc ann
    prettyImpl Impl{..} = prettyAlts (toVarName impl'name) impl'alts

    prettySignature :: VarName -> Scheme -> Doc ann
    prettySignature name scheme = hsep [ pretty name, "::",  pretty scheme]

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

prettyVecExpr :: VecExpr (Doc ann) -> Doc ann
prettyVecExpr = \case
  NewVec _ vec     -> brackets $ hsep $ punctuate comma $ V.toList vec
  VecAppend _ a b  -> op2 "++" a b
  VecLength _      -> "length"
  VecAt _ a n      -> op2 "!" a n
  VecMap _         -> "map"
  VecFold _        -> "fold"

prettyTextExpr :: TextExpr (Doc ann) -> Doc ann
prettyTextExpr = \case
  TextAppend _ a b  -> op2 "++" a b
  TextLength _      -> "length"
  ConvertToText _   -> "show"
  TextHash _ alg    -> case alg of
    Sha256     -> "sha256"
    Blake2b256 -> "blake2b256"

prettyBoxExpr :: BoxExpr (Doc ann) -> Doc ann
prettyBoxExpr = \case
  PrimBox _ box   -> pretty box
  BoxAt _ a field -> hcat [a, dot, prettyBoxField field]

instance Pretty UnOp where
  pretty = \case
    Not -> "not"
    Neg -> "negate"
    TupleAt n -> op1 "tuple-at" (pretty n)

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
    PrimInt _     n -> pretty n
    PrimDouble _  d -> pretty d
    PrimMoney _   m -> pretty m
    PrimBool _    b -> pretty b
    PrimString _  s -> hcat [dquote, pretty s, dquote]

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


