module Hschain.Utxo.Lang.Error where

import Data.Text (Text)

import Hschain.Utxo.Lang.Expr

data Error
  = ParseError Loc Text
  | ExecError ExecError
  | TypeError TypeError
  | PatternError PatError
  deriving (Show)

-- TODO source locations
data ExecError
  = AppliedNonFunction Lang
  | UnboundVariables [VarName]
  | Undefined Loc
  | ThisShouldNotHappen Lang
  | IllegalRecursion Lang
  | OutOfBound Lang
  | NoField VarName
  | NoMainFunction
  | NonExaustiveCase Loc Lang
  deriving (Show)

-- TODO include locations
data PatError
  = NoCasesLeft
  | NoVarFound
  | NoSameArgsNumber
  | EmptyArgument
  deriving (Show)


-- pretty message
-- "There is no main expression defined in the module"

eitherTypeError :: Either TypeError a -> Either Error a
eitherTypeError = either (Left . TypeError) Right

eitherPatternError :: Either TypeError a -> Either Error a
eitherPatternError = either (Left . TypeError) Right

eitherExecError :: Either ExecError a -> Either Error a
eitherExecError = either (Left . ExecError) Right

-----------------------------------------
-- pretty printing



