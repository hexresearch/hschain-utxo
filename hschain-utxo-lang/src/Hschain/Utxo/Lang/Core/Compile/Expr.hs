{-# LANGUAGE TemplateHaskell #-}
-- | Types for core language and its compiled form.
module Hschain.Utxo.Lang.Core.Compile.Expr(
    CoreProg(..)
  , Scomb(..)
  , PrimOp(..)
  , Typed(..)
  , TypeCore
  , ExprCore(..)
  , CaseAlt(..)
  , coreProgToScript
  , coreProgFromScript
    -- * Recursion schemes stuff
  , ExprCoreF(..)
    -- * Lens
  , scomb'nameL
  , scomb'forallL
  , scomb'argsL
  , scomb'bodyL
) where

import Codec.Serialise
import Control.Lens

import Data.String
import Data.Vector (Vector)
import Data.Functor.Foldable.TH
import GHC.Generics

import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Expr (Script(..))

import qualified Data.ByteString.Lazy as LB

-- | core program is a sequence of supercombinator definitions
-- that includes supercombinator called main. The main is an entry point
-- for the execution of the program.
newtype CoreProg = CoreProg [Scomb]
  deriving newtype  (Generic, Semigroup, Monoid, Show)
  deriving anyclass (Serialise)

coreProgToScript :: CoreProg -> Script
coreProgToScript = Script . LB.toStrict . serialise

coreProgFromScript :: Script -> Maybe CoreProg
coreProgFromScript = either (const Nothing) Just . deserialiseOrFail . LB.fromStrict . unScript

-- | Supercobinators do not contain free variables except for references to other supercombinators.
--
-- > S a1 a2 a3 = expr
data Scomb = Scomb
  { scomb'name   :: Name                 -- ^ name of supercombinator
  , scomb'forall :: Vector Name          -- ^ names of type variables. It is empty if type is monomorphic.
  , scomb'args   :: Vector (Typed Name)  -- ^ list of arguments
  , scomb'body   :: Typed ExprCore       -- ^ body
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)

instance IsString ExprCore where
  fromString = EVar . fromString

data PrimOp
  = OpAdd
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)

-- | Expressions of the Core-language
data ExprCore
  = EVar !Name
  -- ^ variables
  | EPolyVar Name [TypeCore]
  -- ^ polymorphic variables which require explicit instantioation of type variables
  | EPrim !Prim
  -- ^ constant primitive
  | EPrimOp !PrimOp
  -- ^ Primitive operation
  | EAp  ExprCore ExprCore
  -- ^ application
  | ELet Name ExprCore ExprCore
  -- ^ lent bindings
  | EIf ExprCore ExprCore ExprCore
  -- ^ if expressions
  | ECase !ExprCore [CaseAlt]
  -- ^ case alternatives
  | EConstr TypeCore !Int !Int
  -- ^ constructor with tag and arity, also we should provide the type
  -- of constructor as afunction for a type-checker
  | EBottom
  -- ^ failed termination for the program
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)

-- | Case alternatives
data CaseAlt = CaseAlt
  { caseAlt'tag   :: !Int
  -- ^ integer tag of the constructor
  -- (integer substitution for the name of constructor)
  , caseAlt'args  :: [Typed Name]
  -- ^ arguments of the pattern matching
  , caseAlt'rhs   :: ExprCore
  -- ^ right-hand side of the case-alternative
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)

makeBaseFunctor ''ExprCore

$(makeLensesWith
   (defaultFieldRules & lensField .~ (mappingNamer (\nm -> [nm++"L"])))
   ''Scomb)
