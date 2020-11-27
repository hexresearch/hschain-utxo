{-# LANGUAGE TemplateHaskell #-}
-- | Types for core language and its compiled form.
module Hschain.Utxo.Lang.Core.Compile.Expr(
    PrimOp(..)
  , Typed(..)
  , TypeCore
  , ExprCore(..)
  , CaseAlt(..)
  , coreProgToScript
  , coreProgFromScript
    -- * Recursion schemes stuff
  , ExprCoreF(..)
  ) where

import Codec.Serialise
import Data.String
import Data.Functor.Foldable.TH
import GHC.Generics

import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Types (Script(..),ArgType)

import qualified Data.ByteString.Lazy as LB


coreProgToScript :: ExprCore -> Script
coreProgToScript = Script . LB.toStrict . serialise

coreProgFromScript :: Script -> Maybe ExprCore
coreProgFromScript = either (const Nothing) Just . deserialiseOrFail . LB.fromStrict . unScript

data PrimOp a
  = OpAdd                 -- ^ Addition
  | OpSub                 -- ^ Subtraction
  | OpMul                 -- ^ Multiplication
  | OpDiv                 -- ^ Division
  | OpNeg                 -- ^ Negation

  | OpBoolAnd             -- ^ Boolean AND
  | OpBoolOr              -- ^ Boolean OR
  | OpBoolNot             -- ^ Boolean negation

  | OpSigAnd              -- ^ AND for sigma expressions
  | OpSigOr               -- ^ OR for sigma expressions
  | OpSigPK               -- ^ Proof of key possession
  | OpSigBool             -- ^ Lift boolean to the sigma expression
  | OpSigListAnd          -- ^ AND for list of sigma expression
  | OpSigListOr           -- ^ OR for list of sigma expression
  | OpSigListAll !a       -- ^ AND for list of sigma expression
  | OpSigListAny !a       -- ^ OR for list of sigma expression

  | OpCheckSig            -- ^ Verify single signature
  | OpCheckMultiSig       -- ^ Verify N-of-M signatures

  | OpEQ !a               -- ^ Equal
  | OpNE !a               -- ^ Not equal
  | OpGT !a               -- ^ Greater then
  | OpGE !a               -- ^ Greater or equal
  | OpLT !a               -- ^ Less then
  | OpLE !a               -- ^ Less or equal

  | OpSHA256              -- ^ SHA256 hash

  | OpTextLength          -- ^ Text length
  | OpBytesLength         -- ^ Bytes length
  | OpTextAppend          -- ^ Text concatenation
  | OpBytesAppend         -- ^ Bytes concatenation
  | OpToBytes   !ArgType
  | OpFromBytes !ArgType

  | OpShow !a             -- ^ Polymorphic show

  | OpEnvGetHeight        -- ^ Current height
  | OpEnvGetSelf          -- ^ Reference to box being evaluated
  | OpEnvGetInputs        -- ^ Inputs of a current box
  | OpEnvGetOutputs       -- ^ Output of a current box
  | OpEnvGetDataInputs    -- ^ Data-inputs of a current box

  | OpArgs !ArgType
  | OpGetBoxId
  | OpGetBoxScript
  | OpGetBoxValue
  | OpGetBoxArgs !ArgType -- ^ Get arguments from box
  | OpGetBoxPostHeight    -- ^ Get time at which box was accepted to blockchain

  | OpListMap    !a !a    -- ^ Map over list
  | OpListAt     !a       -- ^ Index list
  | OpListAppend !a       -- ^ Append lists
  | OpListLength !a       -- ^ Length of list
  | OpListFoldr  !a !a    -- ^ Foldr
  | OpListFoldl  !a !a    -- ^ Foldl
  | OpListFilter !a
  | OpListSum             -- ^ Sum
  | OpListProduct         -- ^ Product
  | OpListAnd             -- ^ AND for all elements
  | OpListOr              -- ^ OR for all elements
  | OpListAll    !a       -- ^ Every element of list satisfy predicate
  | OpListAny    !a       -- ^ Any element of list satisfy predicate
  deriving stock    (Show, Eq, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Serialise)

-- | Expressions of the Core-language
data ExprCore
  = EVar !Name
  -- ^ variables
  | EPrim !Prim
  -- ^ constant primitive
  | EPrimOp !(PrimOp TypeCore)
  -- ^ Primitive operation
  | ELam !Name !TypeCore ExprCore
  -- ^ Lambda abstraction
  | EAp  ExprCore ExprCore
  -- ^ application
  | ELet Name ExprCore ExprCore
  -- ^ Let bindings
  | EIf ExprCore ExprCore ExprCore
  -- ^ if expressions
  | ECase !ExprCore [CaseAlt]
  -- ^ case alternatives
  | EConstr TypeCore !Int
  -- ^ Constructor of ADT. First field is a type of value being
  --   constructed. For example both constructors of @ListT IntT@ will
  --   have that type as parameter. Second is constructor's tag.
  | EBottom
  -- ^ failed termination for the program
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)

instance IsString ExprCore where
  fromString = EVar . fromString

-- | Case alternatives
data CaseAlt = CaseAlt
  { caseAlt'tag   :: !Int
  -- ^ integer tag of the constructor
  -- (integer substitution for the name of constructor)
  , caseAlt'args  :: [Name]
  -- ^ arguments of the pattern matching
  , caseAlt'rhs   :: ExprCore
  -- ^ right-hand side of the case-alternative
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)

makeBaseFunctor ''ExprCore
