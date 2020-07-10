-- | Types for core language and its compiled form.
module Hschain.Utxo.Lang.Core.Compile.Expr(
    CoreProg(..)
  , Scomb(..)
  , Typed(..)
  , Type
  , Expr(..)
  , CaseAlt(..)
  , CompiledScomb(..)
  , coreProgToText
  , coreProgFromText
) where

import Hex.Common.Serialise

import Codec.Serialise

import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Generics

import Hschain.Utxo.Lang.Core.Data.Code (Code)
import Hschain.Utxo.Lang.Core.Data.Prim

-- | core program is a sequence of supercombinator definitions
-- that includes supercombinator called main. The main is an entry point
-- for the execution of the program.
newtype CoreProg = CoreProg [Scomb]
  deriving newtype (Generic)

instance Semigroup CoreProg where
  (CoreProg a) <> (CoreProg b) = CoreProg (a <> b)

instance Monoid CoreProg where
  mempty = CoreProg []

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
  , scomb'body :: Typed Expr           -- ^ body
  } deriving (Show, Eq, Generic)

-- | Expressions of the Core-language
data Expr
  = EVar !Name
  -- ^ variables
  | EPrim !Prim
  -- ^ constant primitive
  | EAp  Expr Expr
  -- ^ application
  | ELet [(Typed Name, Expr)] Expr
  -- ^ lent bindings
  | EIf Expr Expr Expr
  -- ^ if expressions
  | ECase !(Typed Expr) [CaseAlt]
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
  , caseAlt'rhs   :: Expr
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
instance Serialise Expr
instance Serialise Scomb
instance Serialise CoreProg


