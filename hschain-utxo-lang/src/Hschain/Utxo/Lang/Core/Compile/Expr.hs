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

import Hex.Common.Lens (makeLensesWithL)
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Expr (Script(..), ArgType, argTypes, argTypeName)

import qualified Data.ByteString.Lazy as LB

import qualified Hschain.Utxo.Lang.Const as Const


-- | core program is a sequence of supercombinator definitions
-- that includes supercombinator called main. The main is an entry point
-- for the execution of the program.
newtype CoreProg = CoreProg [Scomb]
  deriving stock    (Generic)
  deriving newtype  (Show, Serialise)
instance Wrapped CoreProg

coreProgToScript :: CoreProg -> Script
coreProgToScript = Script . LB.toStrict . serialise

coreProgFromScript :: Script -> Maybe CoreProg
coreProgFromScript = either (const Nothing) Just . deserialiseOrFail . LB.fromStrict . unScript

-- | Supercobinators do not contain free variables except for references to other supercombinators.
--
-- > S a1 a2 a3 = expr
data Scomb = Scomb
  { scomb'name   :: Name
    -- ^ name of supercombinator
  , scomb'args   :: Vector (Typed TypeCore Name)
    -- ^ list of arguments
  , scomb'body   :: Typed TypeCore ExprCore
    -- ^ body
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)

data PrimOp a
  = OpAdd                 -- ^ Addition
  | OpSub                 -- ^ Subtraction
  | OpMul                 -- ^ Multiplication
  | OpDiv                 -- ^ Division
  | OpNeg                 -- ^ Negation

  | OpBoolAnd             -- ^ Boolean AND
  | OpBoolOr              -- ^ Boolean OR
  | OpBoolXor             -- ^ Boolean XOR
  | OpBoolNot             -- ^ Boolean negation

  | OpSigAnd              -- ^ AND for sigma expressions
  | OpSigOr               -- ^ OR for sigma expressions
  | OpSigPK               -- ^ Proof of key possession
  | OpSigBool             -- ^ Lift boolean to the sigma expression
  | OpSigListAnd          -- ^ AND for list of sigma expression
  | OpSigListOr           -- ^ OR for list of sigma expression
  | OpSigListAll !a       -- ^ AND for list of sigma expression
  | OpSigListAny !a       -- ^ OR for list of sigma expression

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

  | OpArgs !ArgType
  | OpGetBoxId
  | OpGetBoxScript
  | OpGetBoxValue
  | OpGetBoxArgs !ArgType -- ^ Get arguments from box
  | OpMakeBox

  | OpListMap    !a !a    -- ^ Map over list
  | OpListAt     !a       -- ^ Index list
  | OpListAppend !a       -- ^ Append lists
  | OpListLength !a       -- ^ Length of list
  | OpListFoldr  !a !a    -- ^ Foldr
  | OpListFoldl  !a !a    -- ^ Foldl
  | OpListFilter !a
  | OpListSum             -- ^ Sum
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

$(makeLensesWithL ''Scomb)


----------------------------------------------------------------
-- Names
----------------------------------------------------------------

-- | Name of monomorphic primop which is used in high level language
monoPrimopName :: PrimOp a -> Maybe Name
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
  OpSigAnd       -> Just "&&&"
  OpSigOr        -> Just "|||"
  OpSigPK        -> Just "pk"
  OpSigBool      -> Just "toSigma"
  OpSigListAnd   -> Just "andSigma"
  OpSigListOr    -> Just "orSigma"
  OpSigListAll _ -> Nothing
  OpSigListAny _ -> Nothing
  --
  OpSHA256      -> Just Const.sha256
  OpTextLength  -> Just Const.lengthText
  OpBytesLength -> Just Const.lengthBytes
  OpTextAppend  -> Just Const.appendText
  OpBytesAppend -> Just Const.appendBytes
  OpToBytes   t -> Just $ Const.serialiseBytes $ argTypeName t
  OpFromBytes t -> Just $ Const.deserialiseBytes $ argTypeName t
  --
  OpArgs t       -> Just $ "get" <> argTypeName t <> "Args"
  OpGetBoxId     -> Just Const.getBoxId
  OpGetBoxScript -> Just Const.getBoxScript
  OpGetBoxValue  -> Just Const.getBoxValue
  OpGetBoxArgs t -> Just $ Const.getBoxArgs $ argTypeName t
  OpMakeBox      -> Just "Box"
  --
  OpEnvGetHeight  -> Just "getHeight"
  OpEnvGetSelf    -> Just "getSelf"
  OpEnvGetInputs  -> Just "getInputs"
  OpEnvGetOutputs -> Just "getOutputs"
  -- Polymorphic functions
  OpShow _ -> Nothing
  OpEQ _   -> Nothing
  OpNE _   -> Nothing
  OpGT _   -> Nothing
  OpGE _   -> Nothing
  OpLT _   -> Nothing
  OpLE _   -> Nothing
  --
  OpListMap{}    -> Nothing
  OpListAt{}     -> Nothing
  OpListAppend{} -> Nothing
  OpListLength{} -> Nothing
  OpListFoldr{}  -> Nothing
  OpListFoldl{}  -> Nothing
  OpListFilter{} -> Nothing
  OpListSum      -> Just "sum"
  OpListAnd      -> Just "and"
  OpListOr       -> Just "or"
  OpListAll{}    -> Nothing
  OpListAny{}    -> Nothing

-- | List of all monomorphic primops
monomorphicPrimops :: [PrimOp a]
monomorphicPrimops =
  [ OpAdd, OpSub, OpMul, OpDiv, OpNeg
  , OpBoolAnd, OpBoolOr, OpBoolXor, OpBoolNot
  , OpSigAnd, OpSigOr, OpSigPK, OpSigBool, OpSigListAnd, OpSigListOr
  , OpSHA256, OpTextLength, OpBytesLength, OpTextAppend, OpBytesAppend
  , OpEnvGetHeight, OpEnvGetSelf, OpEnvGetInputs, OpEnvGetOutputs
  , OpGetBoxId, OpGetBoxScript, OpGetBoxValue, OpMakeBox
  , OpListSum
  , OpListAnd
  , OpListOr
  ]
  ++ (OpToBytes <$> argTypes)
  ++ (OpFromBytes <$> argTypes)
  ++ (OpGetBoxArgs <$> argTypes)
  ++ (OpArgs <$> argTypes)

-- | Name map for substitution of monomorphic primops
monoPrimopNameMap :: Map.Map Name (PrimOp a)
monoPrimopNameMap = Map.fromList
  [ (nm,op) | op      <- monomorphicPrimops
            , Just nm <- [ monoPrimopName op ]
            ]
