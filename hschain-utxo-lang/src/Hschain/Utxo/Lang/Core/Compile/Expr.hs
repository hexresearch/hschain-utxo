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
    -- * Primop names for higher level lnaguage
  , monoPrimopName
  , monomorphicPrimops
  , monoPrimopNameMap
) where

import Codec.Serialise
import Control.Lens  hiding (op)

import Data.String
import Data.Vector (Vector)
import Data.Functor.Foldable.TH
import qualified Data.Map.Strict as Map
import GHC.Generics

import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Expr (Script(..), ArgType, argTypes, argTypeName)

import qualified Data.ByteString.Lazy as LB

-- | core program is a sequence of supercombinator definitions
-- that includes supercombinator called main. The main is an entry point
-- for the execution of the program.
newtype CoreProg = CoreProg [Scomb]
  deriving stock    (Generic)
  deriving newtype  (Semigroup, Monoid, Show, Serialise)
instance Wrapped CoreProg

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
  = OpAdd                       -- ^ Addition
  | OpSub                       -- ^ Subtraction
  | OpMul                       -- ^ Multiplication
  | OpDiv                       -- ^ Division
  | OpNeg                       -- ^ Negation

  | OpBoolAnd                   -- ^ Boolean AND
  | OpBoolOr                    -- ^ Boolean OR
  | OpBoolXor                   -- ^ Boolean XOR
  | OpBoolNot                   -- ^ Boolean negation

  | OpSigAnd                    -- ^ AND for sigma expressions
  | OpSigOr                     -- ^ OR for sigma expressions
  | OpSigPK                     -- ^ Proof of key possession
  | OpSigBool                   -- ^ Lift boolean to the sigma expression

  | OpEQ !TypeCore              -- ^ Equal
  | OpNE !TypeCore              -- ^ Not equal
  | OpGT !TypeCore              -- ^ Greater then
  | OpGE !TypeCore              -- ^ Greater or equal
  | OpLT !TypeCore              -- ^ Less then
  | OpLE !TypeCore              -- ^ Less or equal

  | OpSHA256                    -- ^ SHA256 hash

  | OpTextLength                -- ^ Text length
  | OpBytesLength               -- ^ Bytes length
  | OpTextAppend                -- ^ Text concatenation
  | OpBytesAppend               -- ^ Bytes concatenation
  | OpToBytes   !ArgType
  | OpFromBytes !ArgType

  | OpShow !TypeCore            -- ^ Polymorphic show

  | OpEnvGetHeight              -- ^ Current height
  
  | OpListMap    !TypeCore !TypeCore -- ^ Map over list
  | OpListAt     !TypeCore           -- ^ Index list
  | OpListAppend !TypeCore           -- ^ Append lists
  | OpListLength !TypeCore           -- ^ Length of list
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


----------------------------------------------------------------
-- Names
----------------------------------------------------------------

-- | Name of monomorphic primop which is used in high level language
monoPrimopName :: PrimOp -> Maybe Name
monoPrimopName = \case
  OpAdd         -> Just "+"
  OpSub         -> Just "-"
  OpMul         -> Just "*"
  OpDiv         -> Just "/"
  OpNeg         -> Just "negate"
  --
  OpBoolAnd     -> Just "&&"
  OpBoolOr      -> Just "||"
  OpBoolXor     -> Just "^^"
  OpBoolNot     -> Just "not"
  --
  OpSigAnd      -> Just "&&&"
  OpSigOr       -> Just "|||"
  OpSigPK       -> Just "pk"
  OpSigBool     -> Just "toSigma"
  --
  OpSHA256      -> Just "sha256"
  OpTextLength  -> Just "textLength"
  OpBytesLength -> Just "bytesLength"
  OpTextAppend  -> Just "<>"
  OpBytesAppend -> Just "appendBytes"
  OpToBytes   t -> Just $ "serialise"   <> argTypeName t
  OpFromBytes t -> Just $ "deserialise" <> argTypeName t
  --
  OpEnvGetHeight -> Just "getHeight"
  -- Polymorphic functions
  OpShow _ -> Nothing
  OpEQ _   -> Nothing
  OpNE _   -> Nothing
  OpGT _   -> Nothing
  OpGE _   -> Nothing
  OpLT _   -> Nothing
  OpLE _   -> Nothing

-- | List of all monomorphic primops
monomorphicPrimops :: [PrimOp]
monomorphicPrimops =
  [ OpAdd, OpSub, OpMul, OpDiv, OpNeg
  , OpBoolAnd, OpBoolOr, OpBoolXor, OpBoolNot
  , OpSigAnd, OpSigOr, OpSigPK, OpSigBool
  , OpSHA256, OpTextLength, OpBytesLength, OpTextAppend, OpBytesAppend
  , OpEnvGetHeight
  ]
  ++ (OpToBytes <$> argTypes)
  ++ (OpFromBytes <$> argTypes)

-- | Name map for substitution of monomorphic primops
monoPrimopNameMap :: Map.Map Name PrimOp
monoPrimopNameMap = Map.fromList
  [ (nm,op) | op      <- monomorphicPrimops
            , Just nm <- [ monoPrimopName op ]
            ]
