{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
-- | Types for core language and its compiled form.
module Hschain.Utxo.Lang.Core.Compile.Expr(
    PrimOp(..)
  , Typed(..)
  , TypeCore
  , Core(..)
  , CaseAlt(..)
  , coreProgToScript
  , coreProgFromScript
    -- * Type classes for binding
  , substVar
  , abstract1
  , abstractN
  , abstractGen
  , isClosed
  , lookupVar
  , Identity(..)
  , Proxy
  , Void
  ) where

import Codec.Serialise
import qualified Codec.Serialise.Encoding as CBOR
import qualified Codec.Serialise.Decoding as CBOR
-- import Control.DeepSeq
import Control.Monad.Except
import Data.String
import Data.List (elemIndex)
import Data.Functor.Identity
import Data.Proxy
import Data.Void
import GHC.Generics

import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Types (Script(..),ArgType)
import qualified Data.ByteString.Lazy as LB


coreProgToScript :: Core Void -> Script
coreProgToScript = Script . LB.toStrict . serialise

coreProgFromScript :: Script -> Maybe (Core Void)
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
  deriving stock    (Show, Eq, Generic , Functor, Foldable, Traversable)
  deriving anyclass (Serialise)

-- | Expressions of the Core-language
data Core a
  = EVar !a
  -- ^ Free variable in expression
  | BVar !Int
  -- ^ Bound variable (de-Brujin index)
  | EPrim !Prim
  -- ^ Literal expression
  | EPrimOp !(PrimOp TypeCore)
  -- ^ Primitive operation
  | ELam !TypeCore (Core a)
  -- ^ Lambda abstraction
  | EAp  (Core a) (Core a)
  -- ^ Function application
  | ELet (Core a) (Core a)
  -- ^ Nonrecursive let bindings
  | EIf (Core a) (Core a) (Core a)
  -- ^ If expressions
  | ECase !(Core a) [CaseAlt a]
  -- ^ Case expression
  | EConstr TypeCore !Int
  -- ^ Constructor of ADT. First field is a type of value being
  --   constructed. For example both constructors of @ListT IntT@ will
  --   have that type as parameter. Second is constructor's tag.
  | EBottom
  -- ^ failed termination for the program
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance IsString a => IsString (Core a) where
  fromString = EVar . fromString

-- | Case alternatives
data CaseAlt a = CaseAlt
  { caseAlt'tag   :: !Int
  -- ^ Tag of the constructor. Instead of names we use numbers
  , caseAlt'nVars :: !Int
  -- ^ Number of variables bound in the expression
  , caseAlt'rhs   :: Core a
  -- ^ Right-hand side of the case-alternative
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

lookupVar :: [a] -> Int -> Maybe a
lookupVar []    !_ = Nothing
lookupVar (x:_)  0 = Just x
lookupVar (_:xs) n = lookupVar xs (n-1)

----------------------------------------------------------------
-- Transformations
----------------------------------------------------------------

-- | Substitute free variables in expression.
substVar
  :: (v -> Maybe (Core v))    -- ^ Substitution function
  -> Core v
  -> Core v
substVar fun = go
  where
    go expr = case expr of
      EVar v
        | Just e <- fun v -> e
        | otherwise       -> expr
      BVar{}    -> expr
      -- Noops
      EPrim{}   -> expr
      EPrimOp{} -> expr
      EBottom   -> expr
      EConstr{} -> expr
      -- No binders
      EAp a b   -> EAp (go a) (go b)
      EIf c t f -> EIf (go c) (go t) (go f)
      -- Constructors with binders
      ELet e  body -> ELet (go e) (go body)
      ELam ty body -> ELam ty (go body)
      ECase e alts -> ECase (go e)
        [ CaseAlt tag n (go alt)
        | CaseAlt tag n alt <- alts
        ]

isClosed :: Core v -> Either v (Core a)
isClosed = traverse Left

abstract1 :: Eq a => a -> Core a -> Core a
abstract1 x = abstractGen $ \y -> 0 <$ guard (x == y)

abstractN :: Eq a => [a] -> Core a -> Core a
abstractN xs = abstractGen $ \x -> elemIndex x xs

-- FIXME: naive use of abstractGen is depth of expression. Will do for
--        now but we'll need better way to bind variables
abstractGen :: (a -> Maybe Int) -> Core a -> Core a
abstractGen fun = go 0
  where
    go n expr = case expr of
      EVar x | Just i <- fun x -> BVar (n + i)
             | otherwise       -> expr
      BVar{}       -> expr
      EPrim{}      -> expr
      EPrimOp{}    -> expr
      ELam ty e    -> ELam ty $ go (n+1) e
      EAp f a      -> EAp (go n f) (go n a)
      ELet e b     -> ELet (go n e) (go (n+1) b)
      EIf  c t f   -> EIf (go n c) (go n t) (go n f)
      ECase e alts -> ECase (go n e)
        [ CaseAlt i k (go (n+k) a) | CaseAlt i k a <- alts ]
      EConstr{}    -> expr
      EBottom      -> expr


----------------------------------------------------------------
-- Serialization of core
----------------------------------------------------------------

instance ( Serialise v) => Serialise (Core v) where
  encode expr = prefix <> case expr of
    EVar v       -> encode v
    BVar i       -> encode i
    EPrim p      -> encode p
    EPrimOp op   -> encode op
    ELam ty lam  -> encode ty <> encode lam
    EAp   a b    -> encode a  <> encode b
    ELet e body  -> encode e  <> encode body
    EIf c t f    -> encode c  <> encode t <> encode f
    ECase e alts -> encode e  <> encode alts
    EConstr ty i -> encode ty <> encode i
    EBottom      -> mempty
    where
      (conN,numFld) = constructorInfo expr
      prefix        = CBOR.encodeListLen (fromIntegral $ numFld + 1)
                   <> CBOR.encodeWord (fromIntegral conN)
  --
  decode = do
    listLen <- CBOR.decodeListLen
    when (listLen == 0) $ fail "Core: list of zero length"
    tag <- CBOR.decodeWord
    case (tag, listLen) of
      (0 ,2) -> EVar    <$> decode
      (1 ,2) -> BVar    <$> decode
      (2 ,2) -> EPrim   <$> decode
      (3 ,2) -> EPrimOp <$> decode
      (4 ,3) -> ELam    <$> decode <*> decode
      (5 ,3) -> EAp     <$> decode <*> decode
      (6 ,3) -> ELet    <$> decode <*> decode
      (7 ,4) -> EIf     <$> decode <*> decode <*> decode
      (8 ,3) -> ECase   <$> decode <*> decode
      (9 ,3) -> EConstr <$> decode <*> decode
      (10,1) -> pure EBottom
      _     -> fail "Invalid encoding"

instance (Serialise v) => Serialise (CaseAlt v) where
  encode (CaseAlt i n e) = CBOR.encodeListLen 3
                        <> encode i
                        <> encode n
                        <> encode e
  decode = do
    3 <- CBOR.decodeListLen
    CaseAlt <$> decode <*> decode <*> decode



constructorInfo :: (GConstrutorInfo (Rep a), Generic a) => a -> (Int,Int)
constructorInfo = gConInfo . from

-- | Obtain information about constructor number and number of fields
--   using generics
class GConstrutorInfo f where
  gConInfo       :: f p -> (Int,Int)
  gNConstructors :: Proxy f -> Int

instance GConstrutorInfo f => GConstrutorInfo (M1 D c f) where
  gConInfo (M1 f) = gConInfo f
  gNConstructors _ = gNConstructors (Proxy @f)

instance (GConstrutorInfo f, GConstrutorInfo g) => GConstrutorInfo (f :+: g) where
  gConInfo (L1 f) = gConInfo f
  gConInfo (R1 g) = let (conN, nFld) = gConInfo g
                    in  ( conN + gNConstructors (Proxy @f)
                        , nFld)
  gNConstructors _ = gNConstructors (Proxy @f) + gNConstructors (Proxy @g)

instance (GFieldInfo f) => GConstrutorInfo (M1 C i f) where
  gConInfo (M1 f) = (0, nFields f)
  gNConstructors _ = 1

class GFieldInfo f where
  nFields :: f p -> Int

instance GFieldInfo U1 where
  nFields _ = 0

instance GFieldInfo (M1 S i f) where
  nFields _ = 1

instance (GFieldInfo f, GFieldInfo g) => GFieldInfo (f :*: g) where
  nFields (f :*: g) = nFields f + nFields g


----------------------------------------------------------------
-- Instances Zoo
----------------------------------------------------------------

instance Serialise Void where
  encode = absurd
  decode = fail "Cannot decode Void"
