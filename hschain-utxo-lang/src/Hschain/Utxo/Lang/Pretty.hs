{-# OPTIONS_GHC -Wno-orphans #-}
-- | Pretty-printer for values of the language.
module Hschain.Utxo.Lang.Pretty(
    renderDoc
  , renderText
  , prettyRecord
  , pprint
) where

import Codec.Serialise (deserialiseOrFail)
import Hex.Common.Serialise

import Data.Bool
import Data.Void
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Module
import Hschain.Utxo.Lang.Infer()
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Sigma (Proof)
import Hschain.Utxo.Lang.Core.Types (TypeCoreError(..))
import Hschain.Utxo.Lang.Core.Compile.Expr (Core, TermVal(..), PrimCon(..))
import Hschain.Utxo.Lang.Core.ToHask
import Hschain.Utxo.Lang.Compile.Expr (TypedExprLam, TypedLamProg)
import Hschain.Utxo.Lang.Compile.Hask.TypedToHask (toHaskExpr, toHaskProg)
import Hschain.Utxo.Lang.Parser.Hask.ToHask (toHaskModule)
import qualified Hschain.Utxo.Lang.Core.Types as Core
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Vector as V

import HSChain.Crypto.Classes (ByteRepr, encodeToBS, encodeBase58)
import qualified Hschain.Utxo.Lang.Const as Const
import qualified Hschain.Utxo.Lang.Parser.Hask as P
import qualified Hschain.Utxo.Lang.Sigma as S
import qualified Hschain.Utxo.Lang.Sigma.Protocol as Sigma
import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve as EC
import qualified Hschain.Utxo.Lang.Crypto.Signature as Crypto

import qualified Type.Check.HM as H
import qualified Language.Haskell.Exts.SrcLoc as Hask

import qualified Text.Show.Pretty as P
import qualified Data.Text.IO as T

-- | Convenience function to render pretty-printable value to text.
renderText :: Pretty a => a -> Text
renderText = renderDoc . pretty

pprint :: Pretty a => a -> IO ()
pprint = T.putStrLn . renderText

-- | Convenience function to render pretty-printed value to text.
renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

instance Pretty VarName where
  pretty = pretty . varName'name

instance Pretty Lang where
  pretty = pretty . P.prettyExp

instance Pretty BoxId where
  pretty = pretty . encodeBase58

instance Pretty Script where
  pretty (Script bs) = case deserialiseOrFail $ fromStrict bs of
    Left  _ -> "Left: " <> pretty (encodeBase58 bs)
    Right e -> pretty (e :: Core Void)

instance IsVarName a => Pretty (Core a) where
  pretty = pretty . prettyPrint . toHaskExprCore

instance Pretty Box where
  pretty Box{..} = prettyRecord "Box"
      [ ("value" , pretty box'value)
      , ("script", pretty box'script)
      , ("args"  , prettyArgs box'args) ]

-- | Helper to pretty-print records.
prettyRecord :: Doc ann -> [(Text, Doc ann)] -> Doc ann
prettyRecord name fields = vcat [name <> colon, indent 2 (vsep $ fmap ppField fields)]
  where
    ppField (field, val) = hsep [hcat [pretty field, colon], val ]

prettyArgs :: Args -> Doc ann
prettyArgs (Args bs) = pretty $ encodeBase58 bs

instance Pretty TxHash where
  pretty (TxHash bs) = pretty $ serialiseToText bs

instance Pretty Tx where
  pretty Tx{..} = prettyRecord "Tx"
    [ ("inputs", brackets $ hsep $ punctuate comma $ V.toList $ fmap pretty tx'inputs )
    , ("outputs", vsep $ fmap pretty $ V.toList tx'outputs )
    , ("dataInputs", vsep $ fmap pretty $ V.toList tx'dataInputs)
    ]

instance Pretty TxArg where
  pretty TxArg{..} = prettyRecord "TxArg"
    [ ("inputs",  vsep $ fmap pretty $ V.toList txArg'inputs )
    , ("outputs", vsep $ fmap pretty $ V.toList txArg'outputs )
    , ("dataInputs", vsep $ fmap pretty $ V.toList txArg'dataInputs )
    ]

instance Pretty BoxInput where
  pretty BoxInput{..} = prettyRecord "BoxInput"
    [ ("box",   pretty boxInput'box)
    , ("id",    pretty boxInput'id)
    , ("args",  prettyArgs boxInput'args)
    , ("proof", pretty boxInput'proof)
    , ("sigMask", pretty boxInput'sigMask)
    , ("sigMsg", pretty boxInput'sigMsg)
    ]

instance Pretty BoxOutput where
  pretty BoxOutput{..} = prettyRecord "BoxOutput"
    [ ("box",   pretty boxOutput'box)
    , ("id",    pretty boxOutput'id)
    ]

instance Pretty PostBox where
  pretty PostBox{..} = prettyRecord "PostBox"
    [ ("content", pretty postBox'content)
    , ("height",  pretty postBox'height)
    ]

instance Pretty a => Pretty (BoxInputRef a) where
  pretty BoxInputRef{..} = prettyRecord "BoxInputRef"
    [ ("id",      pretty boxInputRef'id)
    , ("args",    prettyArgs boxInputRef'args)
    , ("proof",   pretty boxInputRef'proof)
    , ("sigMask", pretty boxInputRef'sigMask)
    , ("sigs",    pretty $ V.toList boxInputRef'sigs)
    ]

instance Pretty Crypto.Signature where
  pretty sig = pretty $ encodeBase58 $ encodeToBS sig

instance Pretty SigMask where
  pretty SigAll = "SigAll"
  pretty SigMask{..} = prettyRecord "SigMask"
    [ ("inputs",  prettyBits sigMask'inputs)
    , ("outputs", prettyBits sigMask'outputs)
    , ("dataInputs", prettyBits sigMask'dataInputs)
    ]
    where
      prettyBits bits = hcat $ fmap (bool "0" "1") $ V.toList bits

instance Pretty SigMessage where
  pretty = pretty . encodeBase58

instance Pretty Env where
  pretty Env{..} = prettyRecord "Env" [("height", pretty env'height)]

instance Pretty Proof where
  pretty proof = pretty $ P.ppShow proof

instance Pretty a => Pretty (S.SigmaE () (Either Bool a)) where
  pretty = \case
    S.Leaf () (Right k) -> parens $ hsep ["pk", pretty k]
    S.Leaf () (Left  b) -> "Sigma" <> pretty b
    S.AND  () as        -> parens $ hsep $ Const.sigmaAnd : fmap pretty as
    S.OR   () as        -> parens $ hsep $ Const.sigmaOr  : fmap pretty as

instance Pretty S.ProofInput where
  pretty = \case
    Sigma.InputDLog   pk -> pretty pk
    Sigma.InputDTuple dt -> parens $ hsep $ punctuate comma $ fmap pretty
      [Sigma.dtuple'g dt, Sigma.dtuple'g_x dt, Sigma.dtuple'g_y dt, Sigma.dtuple'g_xy dt]

instance ByteRepr (EC.ECPoint a) => Pretty (EC.ECPoint a) where
  pretty = pretty . encodeBase58

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

instance Pretty Error where
  pretty = \case
    ParseError loc txt    -> hsep [hcat [pretty loc, ":"],  "parse error", pretty txt]
    ExecError err         -> pretty err
    TypeError err         -> pretty err
    TypeDeclError err     -> pretty err
    PatError err          -> pretty err
    InternalError err     -> pretty err
    MonoError err         -> pretty err
    CoreScriptError err   -> pretty err
    FreeVariable txt      -> "Free variable: " <> pretty txt
    ErrorList es          -> vcat $ fmap pretty es

instance Pretty ExecError where
  pretty = \case
    UnboundVariables vars          -> vcat $ fmap unboundedVar vars
    UndefinedRecordCons loc cons   -> hcat [pretty loc, ": undefined record constructor ", pretty cons]
    UndefinedReocrdField loc cons field
                                   -> hcat [pretty loc, ": undefined record field ", pretty field, " for constructor ", pretty cons]
    FailedToDecodeScript           -> "Error: Failed to decode script"
    where
      unboundedVar VarName{..} = hsep [hcat [pretty varName'loc, ":"], "Unbound variable:", pretty varName'name]

instance Pretty PatError where
  pretty = \case
    MissingMain       -> "No main function"
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
    H.FreshNameFound         -> err noLoc $ "Impossible happened: failed to eliminate fresh name on type-checker stage"
    where
      err src msg = hsep [hcat [pretty src, ":"], msg]
      inTicks x = hcat ["'", x, "'"]

instance Pretty CoreScriptError where
  pretty = \case
    ResultIsNotSigma               -> "Error: Result of execution is not a sigma expression"
    TypeCoreError err              -> pretty err

instance Pretty TypeCoreError where
  pretty = \case
    ExpressionIsBottom       -> hsep ["Error: expression always evaluates to bottom"]
    VarIsNotDefined v        -> hsep ["Error: variable", pretty v, "is not defined"]
    ArrowTypeExpected t      -> hsep ["Error: function type expected, but got", pretty t]
    TypeCoreMismatch ta tb   -> hsep ["Error: type mismatch. Got", pretty ta, "expected", pretty tb]
    EmptyCaseExpression      -> "Error: empty case alternatives"
    PolymorphicLet           -> "polymorphic type in the let binding"
    BadEquality ty           -> hsep ["Error: non comparable type:", pretty ty]
    BadShow     ty           -> hsep ["Error: non showable type:", pretty ty]
    BadCase                  -> "Error: Type error in case expression"
    BadConstructor           -> "Error: Bad constuctor"

instance Pretty InternalError where
  pretty = \case
    FailedToEliminate txt -> hsep ["Failed to eliminate expression:", pretty txt]
    NonIntegerConstrTag txt -> hsep ["Non-integer constr tag after type inference", pretty txt]
    NonLamType -> "Not a lambda argument type"
    Unexpected msg -> hsep ["Unexpected has happened:", pretty msg]

instance Pretty MonoError where
  pretty = \case
    FailedToFindMonoType loc name -> err loc $ hsep ["Failed to find monomorphic type for", pretty name]
    CompareForNonPrim loc         -> err loc "Compare operator expects primitive type as input."
    InlineError loc name          -> err loc $ hsep ["Failed to inline", pretty name]
    where
      err src msg = hsep [hcat [pretty src, ":"], msg]

instance Pretty TypeDeclError where
  pretty = \case
    TypeIsDefined newName oldName       ->
      err (varName'loc newName) $ inTy newName $ hsep ["Type", pretty newName, "is already defined at", pretty (varName'loc oldName)]
    ConsDeclError userType userCons consErr ->
      err (consName'loc userCons) $ inTyCons userType userCons $ consDeclMessage userCons consErr
    where
      err src msg = hsep [hcat [pretty src, ":"], msg]

      inTy ty msg = vcat [msg, hcat ["In the data declaration of `", pretty ty, "`"]]
      inCons cons msg = vcat [msg, hcat ["In the definition of type constructor `", pretty cons, "`"]]

      inTyCons ty cons msg = inTy ty (inCons cons msg)

consDeclMessage :: ConsName -> ConsDeclError -> Doc ann
consDeclMessage userCons = \case
  ConsIsDefined oldTypeName   ->
    hcat  [ "Constructor `", pretty userCons
          , "` is already defined in type `", pretty oldTypeName, "` at `", pretty (varName'loc oldTypeName), "`"]
  RecFieldIsDefinedInCons fieldName oldConsName ->
    hcat [ "Record field name `", pretty fieldName
          , "` is already defined in the constructor `", pretty oldConsName  ,"` at `", pretty (consName'loc oldConsName), "`" ]
  RecFieldIsDefinedAsValue fieldName ->
    hcat [ "Record field name `", pretty fieldName
          , "` is already defined as value"  ]
  RecFieldIsReservedName fieldName ->
    hcat [ "Record field `", pretty fieldName
          , "` is reserved name"  ]
  TypeArgIsNotDefined tyName ->
    hcat [ "Type name `", pretty tyName, "` is not defined" ]
  TypeAppError ty kindErr ->
    hcat ["Wrong number of arguments for type `", pretty ty,  "` in application: ", pretty kindErr]

instance Pretty KindError where
  pretty KindError{..} = hcat ["expected ", arguments kindError'expected, ", but got ", arguments kindError'got]
    where
      arguments n = hsep [pretty n, hcat ["argument", suffix]]
        where
          suffix
            | n == 1    = ""
            | otherwise = "s"

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

instance Pretty TermVal where
  pretty = go False
    where
      go needParens term = case term of
        PrimVal p        -> pretty p
        ConVal con args  -> case con of
          ConUnit      -> "()"
          ConNil _     -> "[]"
          ConCons _    -> maybe (pretty $ show term) pretty $ getList args
          ConTuple _   -> parens $ hsep $ punctuate comma $ V.toList $ fmap pretty args
          ConNothing _ -> "Nothing"
          ConJust _    -> pCon "Just" args
          ConSum n ts   -> pCon (hcat ["Sum", pretty $ V.length ts, "_", pretty n]) args
        where
          withParens = if needParens then parens else id
          pCon name args = withParens $ hsep $ name : (fmap (go True) $ V.toList args)

      getList :: Vector TermVal -> Maybe [TermVal]
      getList args = case V.toList args of
        [a, as] -> case as of
                     ConVal (ConNil _) _     -> Just [a]
                     ConVal (ConCons _) rest -> fmap (a :) $ getList rest
                     _                       -> Nothing
        _       -> Nothing

instance Pretty Core.Prim where
  pretty = \case
    Core.PrimInt n      -> pretty n
    Core.PrimText txt   -> dquotes $ pretty txt
    Core.PrimBytes bs   -> pretty $ encodeBase58 bs
    Core.PrimBool b     -> pretty b
    Core.PrimSigma sig  -> pretty sig

instance Pretty EvalErr where
  pretty = \case
    TypeMismatch    -> "Error: Type mismatch"
    EvalErr msg     -> pretty msg

instance Pretty TypedExprLam where
  pretty = pretty . prettyPrint . toHaskExpr

instance Pretty Module where
  pretty = pretty . prettyPrint . toHaskModule

instance Pretty TypedLamProg where
  pretty = pretty . prettyPrint . toHaskProg


