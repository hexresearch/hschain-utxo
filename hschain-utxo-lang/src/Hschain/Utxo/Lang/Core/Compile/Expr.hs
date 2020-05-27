-- | Types for core language and its compiled form.
module Hschain.Utxo.Lang.Core.Compile.Expr(
    CoreProg
  , Scomb(..)
  , Expr(..)
  , CaseAlt(..)
  , CompiledScomb(..)
) where

import Data.Vector (Vector)

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..), CaseMap, GlobalName(..))
import Hschain.Utxo.Lang.Core.Data.Utils

-- | core program is a sequence of supercombinator definitions
-- that includes supercombinator called main. The main is an entry point
-- for the execution of the program.
type CoreProg = [Scomb]

-- | Supercobinators do not contain free variables.
--
-- > S a1 a2 a3 = expr
data Scomb = Scomb
  { scomb'name :: Name           -- ^ name of supercombinator
  , scomb'args :: Vector Name    -- ^ list of arguments
  , scomb'body :: Expr           -- ^ body
  }

-- | Expressions of the Core-language
data Expr
  = EVar !Name
  -- ^ variables
  | ENum !Int
  -- ^ constant integer
  | EAp  Expr Expr
  -- ^ application
  | ELet [(Name, Expr)] Expr
  -- ^ lent bindings
  | ECase !Expr [CaseAlt]
  -- ^ case alternatives
  | EConstr !Int !Int
  -- ^ constructor with tag and arity
  deriving (Show, Eq)

-- | Case alternatives
data CaseAlt = CaseAlt
  { caseAlt'tag   :: !Int
  -- ^ integer tag of the constructor
  -- (integer substitution for the name of constructor)
  , caseAlt'args  :: [Name]
  -- ^ arguments of the pattern matching
  , caseAlt'rhs   :: Expr
  -- ^ right-hand side of the case-alternative
  } deriving (Show, Eq)

-- | Compiled supercombinator
data CompiledScomb = CompiledScomb
  { compiledScomb'name  :: Name   -- ^ name
  , compiledScomb'arity :: Int    -- ^ size of argument list
  , compiledScomb'code  :: Code   -- ^ code to instantiate combinator
  } deriving (Show, Eq)
