-- | Errors for our language
module Hschain.Utxo.Lang.Error where

import Data.String
import Data.Text (Text)

import Hschain.Utxo.Lang.Expr

import qualified Language.Haskell.Exts.SrcLoc as H
import qualified Language.Haskell.Exts.Parser as H

-- | errors for our language
data Error
  = ParseError Loc Text         -- ^ parse errors
  | ExecError ExecError         -- ^ errors of execution
  | TypeError TypeError         -- ^ type-errors
  | PatternError PatError       -- ^ pattern definition errors
  | InternalError InternalError -- ^ errors of this type should not happen in production
  deriving (Show)

-- | Execution errors
-- TODO source locations
data ExecError
  = AppliedNonFunction Lang
  | UnboundVariables [VarName]
  | UndefinedRecordCons Loc ConsName
  | UndefinedReocrdField Loc ConsName Text
  | Undefined Loc
  | ThisShouldNotHappen Lang
  | IllegalRecursion Lang
  | OutOfBound Lang
  | NoField VarName
  | NoMainFunction
  | NonExaustiveCase Loc Lang
  deriving (Show)

-- | Errors that can arise during transformation of patterns in the bindings
-- to case-expressions.
--
-- TODO include locations
data PatError
  = NoCasesLeft
  | NoVarFound
  | NoSameArgsNumber  -- ^ All bindings should have the same number of arguments
  | EmptyArgument
  deriving (Show)

data InternalError
  = FailedToEliminate Text
  deriving (Show)

-- pretty message
-- "There is no main expression defined in the module"

-- | Lift type-errors
eitherTypeError :: Either TypeError a -> Either Error a
eitherTypeError = either (Left . TypeError) Right

-- | Lift pattern-errors
eitherPatternError :: Either TypeError a -> Either Error a
eitherPatternError = either (Left . TypeError) Right

-- | Lift execution-errors
eitherExecError :: Either ExecError a -> Either Error a
eitherExecError = either (Left . ExecError) Right

-- | Convert parser errors to our type.
fromParseError :: H.ParseResult a -> Either Error a
fromParseError = \case
  H.ParseOk a           -> Right a
  H.ParseFailed loc err -> Left $ ParseError (H.noInfoSpan $ H.mkSrcSpan loc loc) (fromString err)

