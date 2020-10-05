{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Types for core language and its compiled form.
module Hschain.Utxo.Lang.Core.Compile.Expr(
    PrimOp(..)
  , Typed(..)
  , TypeCore
  , Core(..)
  , ExprCore
  , CaseAlt(..)
  , coreProgToScript
  , coreProgFromScript
    -- * Type classes for binding
  , Arity(..)
  , BindName(..)
  , BindDB(..)
  , Scope(..)
  , Bound(..)
  , Context(..)
  ) where

import Codec.Serialise
import Control.DeepSeq
import Control.Monad.Except
import Data.String
import Data.Foldable
import Data.Map        (Map)
import Data.Text       (Text)
import qualified Data.Map.Strict as Map
import GHC.Generics

import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Types (Script(..),ArgType)
-- import qualified Data.ByteString.Lazy as LB


coreProgToScript :: ExprCore -> Script
coreProgToScript = undefined -- Script . LB.toStrict . serialise

coreProgFromScript :: Script -> Maybe ExprCore
coreProgFromScript = undefined -- either (const Nothing) Just . deserialiseOrFail . LB.fromStrict . unScript

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
  | OpListAnd             -- ^ AND for all elements
  | OpListOr              -- ^ OR for all elements
  | OpListAll    !a       -- ^ Every element of list satisfy predicate
  | OpListAny    !a       -- ^ Any element of list satisfy predicate
  deriving stock    (Show, Eq, Generic , Functor, Foldable, Traversable)
  deriving anyclass (Serialise)

-- | Expressions of the Core-language
data Core b a
  = EVar !a
  -- ^ variables
  | EPrim !Prim
  -- ^ constant primitive
  | EPrimOp !(PrimOp TypeCore)
  -- ^ Primitive operation
  | ELam !TypeCore (Scope b 'One a)
  -- ^ Lambda abstraction
  | EAp  (Core b a) (Core b a)
  -- ^ application
  | ELet (Core b a) (Scope b 'One a)
  -- ^ Let bindings
  | EIf (Core b a) (Core b a) (Core b a)
  -- ^ if expressions
  | ECase !(Core b a) [CaseAlt b a]
  -- ^ case alternatives
  | EConstr TypeCore !Int
  -- ^ Constructor of ADT. First field is a type of value being
  --   constructed. For example both constructors of @ListT IntT@ will
  --   have that type as parameter. Second is constructor's tag.
  | EBottom
  -- ^ failed termination for the program

instance IsString a => IsString (Core b a) where
  fromString = EVar . fromString

type ExprCore = Core BindName Name

-- | Case alternatives
data CaseAlt b a = CaseAlt
  { caseAlt'tag   :: !Int
  -- ^ Tag of the constructor. Instead of names we use numbers
  , caseAlt'rhs   :: Scope b 'Many a
  -- ^ Right-hand side of the case-alternative
  }

data Arity
  = One
  | Many

data BindName arity a where
  BindName1 ::  a  -> BindName 'One  a
  BindNameN :: [a] -> BindName 'Many a

data BindDB arity a where
  BindDB1 ::        BindDB 'One  a
  BindDBN :: Int -> BindDB 'Many a

data Scope b arity a = Scope (b arity a) (Core b a)

class Bound bnd where
  data Ctx bnd :: * -> * -> *
  -- | Bind single value of type @x@ to variable . This operation
  --   always succeed.
  bindOne  :: Ord a => bnd 'One a -> x -> Ctx bnd a x -> Ctx bnd a x
  -- | Bind multiple values at once.
  bindMany
    :: (Ord a, MonadError TypeCoreError m)
    => bnd 'Many a
    -> [x]
    -> Ctx bnd a x
    -> m (Ctx bnd a x)

class (Ord a, Bound bnd) => Context bnd a where
  -- Check whether variable is bound
  isBound   :: Ctx bnd a x -> a -> Bool
  -- Lookup variable in context
  lookupVar :: Ctx bnd a x -> a -> Maybe x
  --
  emptyContext :: Ctx bnd a x

instance Bound BindName where
  newtype Ctx BindName a b = CtxName (Map a b)
  bindOne  (BindName1 a ) x  (CtxName m0) = CtxName $ Map.insert a x m0
  bindMany (BindNameN as) xs (CtxName m0) = do
    binders <- zipB as xs
    pure $ CtxName $ foldl' (\m (a,x) -> Map.insert a x m) m0 binders
    where
      zipB []     []     = pure []
      zipB (b:bs) (v:vs) = ((b,v):) <$> zipB bs vs
      zipB  _      _     = throwError BadCase

instance Ord a => Context BindName a where
  isBound   (CtxName m) a = Map.member a m
  lookupVar (CtxName m) a = Map.lookup a m
  emptyContext = CtxName Map.empty

instance Bound BindDB where
  newtype Ctx BindDB a b = CtxDB [b]
  bindOne  _           x  (CtxDB bound) = CtxDB (x : bound)
  bindMany (BindDBN n) xs (CtxDB bound)
    | length xs /= n = throwError BadCase
    | otherwise      = pure $ CtxDB $ xs <> bound

-- FIXME: Deal with negative indices
--
-- FIXME: What to do with "free" out of range indices. Are they free
--        or are they malformed program?
instance Context BindDB Int where
  -- FIXME: Should we carry length around
  isBound   (CtxDB bound) i = i < length bound
  lookupVar (CtxDB bound) i
    | i < length bound = Just (bound !! i)
    | otherwise        = Nothing
  emptyContext = CtxDB []

----------------------------------------------------------------
-- Transformations
----------------------------------------------------------------



----------------------------------------------------------------
-- Instances Zoo
----------------------------------------------------------------

deriving instance Show a => Show (BindName arity a)
deriving instance Eq   a => Eq   (BindName arity a)
deriving instance Functor     (BindName arity)
deriving instance Foldable    (BindName arity)
deriving instance Traversable (BindName arity)


deriving instance Show (BindDB arity a)
deriving instance Eq   (BindDB arity a)
deriving instance Functor     (BindDB arity)
deriving instance Foldable    (BindDB arity)
deriving instance Traversable (BindDB arity)


deriving instance (forall x. Show (b x a), Show a) => Show (Scope b arity a)
deriving instance (forall x. Eq   (b x a), Eq   a) => Eq   (Scope b arity a)
deriving instance (forall a. Functor     (b a)) => Functor     (Scope b arity)
deriving instance (forall a. Foldable    (b a)) => Foldable    (Scope b arity)
deriving instance (forall a. Traversable (b a)) => Traversable (Scope b arity)


deriving instance ( forall x. Show (b x a), Show a) => Show (Core b a)
deriving instance ( forall x. Eq   (b x a), Eq   a) => Eq   (Core b a)
deriving instance (forall a. Functor     (b a)) => Functor     (Core    b)
deriving instance (forall a. Foldable    (b a)) => Foldable    (Core    b)
deriving instance (forall a. Traversable (b a)) => Traversable (Core    b)


deriving instance ( forall x. Show (b x a)
                  , Show a
                  ) => Show (CaseAlt b a)
deriving instance ( forall x. Eq (b x a)
                  , Eq a
                  ) => Eq (CaseAlt b a)
deriving instance (forall a. Functor     (b a)) => Functor     (CaseAlt b)
deriving instance (forall a. Foldable    (b a)) => Foldable    (CaseAlt b)
deriving instance (forall a. Traversable (b a)) => Traversable (CaseAlt b)
