-- | Types for core language and its compiled form.
module Hschain.Utxo.Lang.Core.Compile.Expr(
    CoreProg(..)
  , Scomb(..)
  , Typed(..)
  , Type
  , ExprCore(..)
  , CaseAlt(..)
  , CompiledScomb(..)
  , coreProgToText
  , coreProgFromText
) where

import Hex.Common.Serialise

import Codec.Serialise

import Data.String
import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Generics

import Hschain.Utxo.Lang.Core.Data.Code (Code)
import Hschain.Utxo.Lang.Core.Data.Prim

-- | core program is a sequence of supercombinator definitions
-- that includes supercombinator called main. The main is an entry point
-- for the execution of the program.
newtype CoreProg = CoreProg [Scomb]
  deriving newtype (Generic, Semigroup, Monoid)

coreProgToText :: CoreProg -> Text
coreProgToText = serialiseToText

coreProgFromText :: Text -> Maybe CoreProg
coreProgFromText = serialiseFromText

-- | Supercobinators do not contain free variables except for references to other supercombinators.
--
-- > S a1 a2 a3 = expr
data Scomb = Scomb
  { scomb'name :: Name                 -- ^ name of supercombinator
  , scomb'args :: Vector (Typed Name)  -- ^ list of arguments
  , scomb'body :: Typed ExprCore       -- ^ body
  } deriving (Show, Eq, Generic)

-- | Expressions of the Core-language
data ExprCore
  = EVar !Name
  -- ^ variables
  | EPrim !Prim
  -- ^ constant primitive
  | EAp  ExprCore ExprCore
  -- ^ application
  | ELet [(Typed Name, ExprCore)] ExprCore
  -- ^ lent bindings
  | EIf ExprCore ExprCore ExprCore
  -- ^ if expressions
  | ECase !(Typed ExprCore) [CaseAlt]
  -- ^ case alternatives
  | EConstr Type !Int !Int
  -- ^ constructor with tag and arity, also we should provide the type
  -- of constructor as afunction for a type-checker
  | EBottom
  -- ^ failed termination for the program
  deriving (Show, Eq, Generic)

-- | Case alternatives
data CaseAlt = CaseAlt
  { caseAlt'tag   :: !Int
  -- ^ integer tag of the constructor
  -- (integer substitution for the name of constructor)
  , caseAlt'args  :: [Typed Name]
  -- ^ arguments of the pattern matching
  , caseAlt'rhs   :: ExprCore
  -- ^ right-hand side of the case-alternative
  } deriving (Show, Eq, Generic)

-- | Compiled supercombinator
data CompiledScomb = CompiledScomb
  { compiledScomb'name  :: Name   -- ^ name
  , compiledScomb'arity :: Int    -- ^ size of argument list
  , compiledScomb'code  :: Code   -- ^ code to instantiate combinator
  } deriving (Show, Eq)

---------------------------------------------
-- instances

instance Serialise CaseAlt
instance Serialise ExprCore
instance Serialise Scomb
instance Serialise CoreProg

instance IsString ExprCore where
  fromString = EVar . fromString

