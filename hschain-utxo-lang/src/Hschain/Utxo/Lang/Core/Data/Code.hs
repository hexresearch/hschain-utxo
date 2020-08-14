-- | Module defines type for sequence of Code instructions
module Hschain.Utxo.Lang.Core.Data.Code(
    Code
  , Instr(..)
) where

import Prelude hiding (null, init)

import Control.DeepSeq
import Data.IntMap (IntMap)
import Data.Sequence (Seq)
import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Expr (ArgType)
import Hschain.Utxo.Lang.Core.Data.Prim

-- | Code to feed to machine
newtype Code = Code { unCode :: Seq Instr }
  deriving newtype (Semigroup, Monoid, Show, Eq, NFData)

-- | Instructions for G-machine
data Instr
  = Unwind
  -- ^ finish the execution of the combinator body
  | PushGlobal !GlobalName
  -- ^ save the global name of supercombinator
  | PushPrim !Prim
  -- ^ save the constant primitive
  | PushBasic !Prim
  -- ^ push value to V-stack
  | Push !Int
  -- ^ push address on stack
  | Mkap
  -- ^ make application, removes two top elements from stack
  -- creates application on heap, and puts address of the result on stack
  | Update !Int
  -- ^ overwrite (N+1)'th element of the stack with indirection node on top of the stack
  | Pop !Int
  -- ^ pop N elements from top of the stack
  | Slide !Int
  -- ^ removes N elements after the top element
  | Alloc !Int
  -- ^ Allocates N place-holder nodes on the heap
  | Eval
  -- ^ Evaluate the expression which is referenced from the top of the stack to WHNF
  | Cond !Code !Code
  -- ^ low-level implementation of if
  | Pack !Int !Int
  -- ^ constructs saturated (all argumentss are applied) constructor
  | CaseJump CaseMap
  -- ^ performs execution of case-expression
  | Split !Int
  -- ^ uesd to gain access to the components of the constructor
  | Print
  -- ^ Prints result
  | MkPrim
  -- ^ moves primitive result from V-stack to heap
  | Get
  -- ^ moves value from heap (it's addressed from top of the stack) to V-stack
  | UpdatePrim !Int
  -- ^ synonym for the sequence [MkPrim, Update n]
  | Add | Sub | Mul | Div | Neg
  -- ^ Arithmetic operations
  | Eq | Ne | Lt | Le | Gt | Ge
  -- ^ Comparison operations
  | And | Or | Not | Xor
  -- ^ boolean operators
  | SigAnd | SigOr | SigPk | SigBool
  -- ^ sigma operators
  | TextLength | TextAppend | HashBlake| HashSha | ShowInt | ShowBool
  -- ^ text operators
  | BytesAppend | ToBytes ArgType | FromBytes ArgType | Sha256
  -- ^ bytes operations
  | Bottom
  -- ^ Failed termination
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

type CaseMap = IntMap Code

data GlobalName
  = GlobalName !Name
  -- ^ name of the global supercombinator
  | ConstrName !Int !Int
  -- ^ name of the global constructor (int tag, arity)
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

