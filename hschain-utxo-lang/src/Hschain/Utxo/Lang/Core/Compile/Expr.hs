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
  , Scope1(..)
  , ScopeN(..)
  , substVar
  , toDeBrujin
  , eraseCoreLabels
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
import Data.List       (elemIndex)
import Data.Functor.Identity
import Data.Proxy
import Data.Void
import GHC.Generics

import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Types (Script(..),ArgType)
import qualified Data.ByteString.Lazy as LB


coreProgToScript :: Core Proxy Void -> Script
coreProgToScript = Script . LB.toStrict . serialise

coreProgFromScript :: Script -> Maybe (Core Proxy Void)
coreProgFromScript = either (const Nothing) Just . deserialiseOrFail . LB.fromStrict . unScript

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
  | OpListAnd             -- ^ AND for all elements
  | OpListOr              -- ^ OR for all elements
  | OpListAll    !a       -- ^ Every element of list satisfy predicate
  | OpListAny    !a       -- ^ Any element of list satisfy predicate
  deriving stock    (Show, Eq, Generic , Functor, Foldable, Traversable)
  deriving anyclass (Serialise)

-- | Expressions of the Core-language
data Core f a
  = EVar !a
  -- ^ Free variable in expression
  | BVar !Int
  -- ^ Bound variable (de-Brujin index)
  | EPrim !Prim
  -- ^ Literal expression
  | EPrimOp !(PrimOp TypeCore)
  -- ^ Primitive operation
  | ELam !TypeCore (Scope1 f a)
  -- ^ Lambda abstraction
  | EAp  (Core f a) (Core f a)
  -- ^ Function application
  | ELet (Core f a) (Scope1 f a)
  -- ^ Nonrecursive let bindings
  | EIf (Core f a) (Core f a) (Core f a)
  -- ^ If expressions
  | ECase !(Core f a) [CaseAlt f a]
  -- ^ Case expression
  | EConstr TypeCore !Int
  -- ^ Constructor of ADT. First field is a type of value being
  --   constructed. For example both constructors of @ListT IntT@ will
  --   have that type as parameter. Second is constructor's tag.
  | EBottom
  -- ^ failed termination for the program
  deriving (Generic, Functor, Foldable, Traversable)

instance IsString a => IsString (Core b a) where
  fromString = EVar . fromString

-- | Case alternatives
data CaseAlt f a = CaseAlt
  { caseAlt'tag   :: !Int
  -- ^ Tag of the constructor. Instead of names we use numbers
  , caseAlt'rhs   :: ScopeN f a
  -- ^ Right-hand side of the case-alternative
  }
  deriving (Generic, Functor, Foldable, Traversable)

-- | Binder for single variable
data Scope1 f a = Scope1 (f Name) (Core f a)
  deriving (Functor, Foldable, Traversable)

-- | Binder for multiple variables. We need number of variables for
--   evaluation and
data ScopeN f a = ScopeN !Int (f [Name]) (Core f a)
  deriving (Functor, Foldable, Traversable)

lookupVar :: [a] -> Int -> Maybe a
lookupVar []    !_ = Nothing
lookupVar (x:_)  0 = Just x
lookupVar (_:xs) n = lookupVar xs (n-1)

----------------------------------------------------------------
-- Transformations
----------------------------------------------------------------

-- | Substitute free variables in expression.
substVar
  :: (v -> Maybe (Core f v))    -- ^ Substitution function
  -> Core f v
  -> Core f v
substVar fun = go []
  where
    go ctx expr = case expr of
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
      EAp a b   -> EAp (go ctx a) (go ctx b)
      EIf c t f -> EIf (go ctx c) (go ctx t) (go ctx f)
      -- Constructors with binders
      ELet e  body -> ELet (go ctx e) (go1 ctx body)
      ELam ty body -> ELam ty (go1 ctx body)
      ECase e alts -> ECase (go ctx e)
        [ CaseAlt tag (goN ctx alt)
        | CaseAlt tag alt <- alts
        ]
    --
    go1 ctx (Scope1   b e) = Scope1   b $ go ctx e
    goN ctx (ScopeN n b e) = ScopeN n b $ go ctx e

-- | Substitute free variables in expression.
eraseCoreLabels :: Core f v -> Core Proxy v
eraseCoreLabels = go
  where
    go = \case
      EVar v      -> EVar v
      BVar i      -> BVar i
      EPrim p     -> EPrim p
      EPrimOp op  -> EPrimOp op
      EBottom     -> EBottom
      EConstr i t -> EConstr i t
      EAp a b     -> EAp (go a) (go b)
      EIf c t f   -> EIf (go c) (go t) (go f)
      -- Constructors with binders
      ELet e  body -> ELet (go e) (go1 body)
      ELam ty body -> ELam ty (go1 body)
      ECase e alts -> ECase (go e)
        [ CaseAlt tag (goN alt)
        | CaseAlt tag alt <- alts
        ]
    --
    go1 (Scope1   _ e) = Scope1   Proxy $ go e
    goN (ScopeN n _ e) = ScopeN n Proxy $ go e

isClosed :: Core f v -> Either v (Core f a)
isClosed = go
  where
    go = \case
      EVar v -> Left v
      BVar i -> pure $ BVar i
      -- Noops
      EPrim   p   -> pure $ EPrim p
      EPrimOp op  -> pure $ EPrimOp op
      EBottom     -> pure   EBottom
      EConstr i t -> pure $ EConstr i t
      -- No binders
      EAp a b   -> EAp <$> go a <*> go b
      EIf c t f -> EIf <$> go c <*> go t <*> go f
      -- -- Constructors with binders
      ELet e  body -> ELet <$> go e <*> go1 body
      ELam ty body -> ELam ty <$> go1 body
      ECase e alts -> ECase <$> go e <*> sequence
        [ CaseAlt tag <$> goN alt
        | CaseAlt tag alt <- alts
        ]
    --
    go1 (Scope1   b e) = Scope1   b <$> go e
    goN (ScopeN n b e) = ScopeN n b <$> go e


-- | Bind all variables in the core expression
toDeBrujin :: Core Identity Name -> Core Identity Name
toDeBrujin = go []
  where
    go ctx expr = case expr of
      EVar a
        | Just i <- elemIndex a ctx -> BVar i
        | otherwise                 -> expr
      BVar{}                        -> expr
      --
      EPrim{}   -> expr
      EPrimOp{} -> expr
      EBottom   -> expr
      EConstr{} -> expr
      --
      EAp a b   -> EAp (go ctx a) (go ctx b)
      EIf c t f -> EIf (go ctx c) (go ctx t) (go ctx f)
      --
      ELet e  body -> ELet (go ctx e) (go1 ctx body)
      ELam ty body -> ELam ty (go1 ctx body)
      ECase e alts -> ECase (go ctx e) (goAlt ctx <$> alts)
    --
    goAlt ctx (CaseAlt i (ScopeN n (Identity xs) e))
      | length xs /= n = error "Internal error: size mismatch in case"
      | otherwise      = CaseAlt i $ ScopeN n (Identity xs) $ go (xs <> ctx) e
    --
    go1 ctx (Scope1 (Identity x) e) = Scope1 (Identity x) $ go (x : ctx) e


----------------------------------------------------------------
-- Serialization of core
----------------------------------------------------------------

instance ( Serialise v
         , Serialise (Scope1 f v)
         , Serialise (ScopeN f v)
         ) => Serialise (Core f v) where
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

instance (Serialise (ScopeN f v)) => Serialise (CaseAlt f v) where
  encode (CaseAlt i e) = CBOR.encodeListLen 2
                      <> encode i
                      <> encode e
  decode = do
    2 <- CBOR.decodeListLen
    CaseAlt <$> decode <*> decode

instance (Serialise v) => Serialise (Scope1 Identity v) where
  encode (Scope1 (Identity v) e) = CBOR.encodeListLen 2
                                <> encode v
                                <> encode e
  decode = do
    2 <- CBOR.decodeListLen
    Scope1 <$> (Identity <$> decode) <*> decode

instance (Serialise v) => Serialise (ScopeN Identity v) where
  encode (ScopeN _ (Identity xs) e) = CBOR.encodeListLen 2
                                   <> encode xs
                                   <> encode e
  decode = do
    2  <- CBOR.decodeListLen
    xs <- decode
    e  <- decode
    pure $ ScopeN (length xs) (Identity xs) e

instance (Serialise v) => Serialise (Scope1 Proxy v) where
  encode (Scope1 Proxy e) = encode e
  decode = Scope1 Proxy <$> decode

instance (Serialise v) => Serialise (ScopeN Proxy v) where
  encode (ScopeN n Proxy e) = CBOR.encodeListLen 2
                           <> encode n
                           <> encode e
  decode = do
    2 <- CBOR.decodeListLen
    ScopeN <$> decode <*> pure Proxy <*> decode
    



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

deriving instance (Show a, forall x. Show (f x)) => Show (Scope1 f a)
deriving instance (Eq   a, forall x. Eq   (f x)) => Eq   (Scope1 f a)

deriving instance (Show a, forall x. Show (f x)) => Show (ScopeN f a)
deriving instance (Eq   a, forall x. Eq   (f x)) => Eq   (ScopeN f a)

deriving instance (Show a, forall x. Show (f x)) => Show (CaseAlt f a)
deriving instance (Eq   a, forall x. Eq   (f x)) => Eq   (CaseAlt f a)
 
deriving instance (Show a, forall x. Show (f x)) => Show (Core f a)
deriving instance (Eq   a, forall x. Eq   (f x)) => Eq   (Core f a)

instance Serialise Void where
  encode = absurd
  decode = fail "Cannot decode Void"
