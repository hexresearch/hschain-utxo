-- | Errors for our language
module Hschain.Utxo.Lang.Error where

import Control.DeepSeq (NFData)
import Control.Monad.Except

import Data.Data
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Core.Types (TypeCore)

import qualified Language.Haskell.Exts.SrcLoc as H
import qualified Language.Haskell.Exts.Parser as H


-- | errors for our language
data Error
  = ParseError Loc Text             -- ^ parse errors
  | ExecError ExecError             -- ^ errors of execution
  | TypeError TypeError             -- ^ type-errors
  | PatError PatError               -- ^ pattern definition errors
  | InternalError InternalError     -- ^ errors of this type should not happen in production
  | MonoError MonoError             -- ^ errors during monomorphizing
  | CoreScriptError CoreScriptError -- ^ errors of core scripts
  deriving stock    (Show,Eq,Generic,Data)

-- | Execution errors
-- TODO source locations
data ExecError
  = UnboundVariables [VarName]
  | UndefinedRecordCons Loc ConsName
  | UndefinedReocrdField Loc ConsName Text
  | FailedToDecodeScript
  deriving stock    (Show,Eq,Generic,Data)

-- | Errors that can arise during transformation of patterns in the bindings
-- to case-expressions.
--
-- TODO include locations
data PatError
  = NoCasesLeft
  | NoVarFound
  | NoSameArgsNumber  -- ^ All bindings should have the same number of arguments
  | EmptyArgument
  | WrongPatPrimMixture Loc
  | WrongPatConsMixture Loc
  | MissingMain
  deriving stock    (Show,Eq,Generic,Data)

data InternalError
  = FailedToEliminate Text
  | NonIntegerConstrTag Text
  | NonLamType
  deriving stock    (Show,Eq,Generic,Data)
  deriving anyclass (NFData)

data MonoError
  = FailedToFindMonoType Loc Text
  | CompareForNonPrim Loc
  | InlineError Loc Text
  deriving stock    (Show,Eq,Generic,Data)

data CoreScriptError
  = ResultIsNotSigma
  | TypeCoreError TypeCoreError
  deriving stock    (Show,Eq,Generic,Data)
  deriving anyclass (NFData)

-- | Errors for core language type-checker.
data TypeCoreError
  = ExpressionIsBottom                  -- ^ Expression as whole always evaluates to bottom
  | VarIsNotDefined Text                -- ^ Variable is used but not defined
  | ArrowTypeExpected TypeCore          -- ^ Function type expected, but got
  | TypeCoreMismatch  TypeCore TypeCore -- ^ Got type a while expected b
  | EmptyCaseExpression                 -- ^ Case has no alternatives
  | PolymorphicLet                      -- ^ Let is used to bind variable that always evaluate to bottom
  | BadEquality TypeCore                -- ^ Equality used on types that don't support it
  | BadShow     TypeCore                -- ^ Show is used on types that don't support it
  | BadCase
  | BadConstructor
  deriving stock    (Show,Eq,Generic,Data)
  deriving anyclass (NFData)

typeCoreMismatch :: MonadError TypeCoreError m => TypeCore -> TypeCore -> m a
typeCoreMismatch ta tb = throwError $ TypeCoreMismatch ta tb



-- | Lift type-errors
eitherTypeError :: Either TypeError a -> Either Error a
eitherTypeError = either (Left . TypeError) Right

-- | Convert parser errors to our type.
fromParseError :: H.ParseResult a -> Either Error a
fromParseError = \case
  H.ParseOk a           -> Right a
  H.ParseFailed loc err -> Left $ ParseError (H.noInfoSpan $ H.mkSrcSpan loc loc) (fromString err)

-- errors:

wrongPatPrimMixture :: MonadError Error m => Loc -> m a
wrongPatPrimMixture loc = throwError $ PatError $ WrongPatPrimMixture loc

wrongPatConsMixture :: MonadError Error m => Loc -> m a
wrongPatConsMixture loc = throwError $ PatError $ WrongPatConsMixture loc

noCasesLeft :: MonadError Error m => m a
noCasesLeft = throwError $ PatError $ NoCasesLeft

failedToEliminate :: MonadError Error m => Text -> m a
failedToEliminate msg = throwError $ InternalError $ FailedToEliminate msg

unboundVariable :: MonadError Error m => VarName -> m a
unboundVariable = unboundVariables . return

unboundVariables :: MonadError Error m => [VarName] -> m a
unboundVariables vars = throwError $ ExecError $ UnboundVariables vars

noSameArgsNumber :: MonadError Error m => m a
noSameArgsNumber = throwError $ PatError NoSameArgsNumber

emptyArgument :: MonadError Error m => m a
emptyArgument = throwError $ PatError EmptyArgument

compareForNonPrim :: MonadError Error m => Loc -> m a
compareForNonPrim = throwError . MonoError . CompareForNonPrim

failedToFindMonoType :: MonadError Error m => Loc -> Text -> m a
failedToFindMonoType loc name = throwError $ MonoError $ FailedToFindMonoType loc name

inlineError :: MonadError Error m => Loc -> Text -> m a
inlineError loc name = throwError $ MonoError $ InlineError loc name

