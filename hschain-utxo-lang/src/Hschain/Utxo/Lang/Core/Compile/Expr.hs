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
  , Arity(..)
  , BindName(..)
  , BindDB(..)
  , Scope(..)
  , Bound(..)
  , Context(..)
  , substVar
  , toDeBrujin
  ) where

import Codec.Serialise
import qualified Codec.Serialise.Encoding as CBOR
import qualified Codec.Serialise.Decoding as CBOR
-- import Control.DeepSeq
import Control.Monad.Except
import Data.String
import Data.Foldable
import Data.Map        (Map)
import Data.Proxy
import qualified Data.Map.Strict as Map
import GHC.Generics

import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Types (Script(..),ArgType)
import qualified Data.ByteString.Lazy as LB


coreProgToScript :: Core BindDB Int -> Script
coreProgToScript = Script . LB.toStrict . serialise

coreProgFromScript :: Script -> Maybe (Core BindDB Int)
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
  deriving (Generic)

instance IsString a => IsString (Core b a) where
  fromString = EVar . fromString

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
  -- | Bind multiple values at once.
  bindMany_
    :: (Ord a)
    => bnd 'Many a
    -> Ctx bnd a ()
    -> Ctx bnd a ()

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
  --
  bindMany (BindNameN as) xs (CtxName m0) = do
    binders <- zipB as xs
    pure $ CtxName $ foldl' (\m (a,x) -> Map.insert a x m) m0 binders
    where
      zipB []     []     = pure []
      zipB (b:bs) (v:vs) = ((b,v):) <$> zipB bs vs
      zipB  _      _     = throwError BadCase
  --
  bindMany_ (BindNameN as) (CtxName m0) =
    CtxName $ foldl' (\m a -> Map.insert a () m) m0 as

instance Ord a => Context BindName a where
  isBound   (CtxName m) a = Map.member a m
  lookupVar (CtxName m) a = Map.lookup a m
  emptyContext = CtxName Map.empty

instance Bound BindDB where
  newtype Ctx BindDB a b = CtxDB [b]
  bindOne  _           x  (CtxDB bound) = CtxDB (x : bound)
  --
  bindMany (BindDBN n) xs (CtxDB bound)
    | length xs /= n = throwError BadCase
    | otherwise      = pure $ CtxDB $ xs <> bound
  --
  bindMany_ (BindDBN b) (CtxDB bound) = CtxDB $ replicate b () <> bound

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

-- | Substitute variables in expression. Note! This is not capture
--   avoiding substitution. Take care when using.
substVar
  :: (Context b v)
  => (v -> Maybe (Core b v))    -- ^ Substitution function
  -> Core b v
  -> Core b v
substVar fun = go emptyContext
  where
    go ctx = \case
      var@(EVar v)
        | isBound ctx v   -> var
        | Just e <- fun v -> e
        | otherwise       -> var
      -- Noops
      p@EPrim{}   -> p
      p@EPrimOp{} -> p
      p@EBottom   -> p
      p@EConstr{} -> p
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
    go1 ctx (Scope b e) = Scope b $ go (bindOne b () ctx) e
    goN ctx (Scope b e) = Scope b $ go (bindMany_ b ctx) e


-- | Convert expression with named variables to de-Brujin form. It
--   assumes that expression is closed and will return name of free
--   variable if it encounters one.
toDeBrujin :: Eq a => Core BindName a -> Either a (Core BindDB Int)
toDeBrujin = go []
  where
    go ctx = \case
      EVar a -> case find ((==a) . snd) $ zip [0..] ctx of
        Nothing    -> Left a
        Just (i,_) -> Right (EVar i)
      --
      EPrim   p    -> pure $ EPrim p
      EPrimOp op   -> pure $ EPrimOp op
      EBottom      -> pure   EBottom
      EConstr ty i -> pure $ EConstr ty i
      --
      EAp a b   -> EAp <$> go ctx a <*> go ctx b
      EIf c t f -> EIf <$> go ctx c <*> go ctx t <*> go ctx f
      --
      ELet e  body -> ELet <$> go ctx e <*> go1 ctx body
      ELam ty body -> ELam ty <$> go1 ctx body
      ECase e alts -> ECase <$> go ctx e <*>
        traverse (goAlt ctx) alts
    --
    goAlt ctx (CaseAlt i (Scope (BindNameN xs) e)) = do
      e' <- go (xs ++ ctx) e
      pure $ CaseAlt i $ Scope (BindDBN (length xs)) e'
    --
    go1 ctx (Scope (BindName1 x) e) = Scope BindDB1 <$> go (x : ctx) e



----------------------------------------------------------------
-- Serialization of core
----------------------------------------------------------------

instance ( Serialise v
         , Serialise (Scope b 'One  v)
         , Serialise (Scope b 'Many v)
         ) => Serialise (Core b v) where
  encode expr = prefix <> case expr of
    EVar v       -> encode v
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
      (0,2) -> EVar    <$> decode
      (1,2) -> EPrim   <$> decode
      (2,2) -> EPrimOp <$> decode
      (3,3) -> ELam    <$> decode <*> decode
      (4,3) -> EAp     <$> decode <*> decode
      (5,3) -> ELet    <$> decode <*> decode
      (6,4) -> EIf     <$> decode <*> decode <*> decode
      (7,3) -> ECase   <$> decode <*> decode
      (8,3) -> EConstr <$> decode <*> decode
      (9,1) -> pure EBottom
      _     -> fail "Invalid encoding"

instance (Serialise (Scope b 'Many v)) => Serialise (CaseAlt b v) where
  encode (CaseAlt i e) = CBOR.encodeListLen 2
                      <> encode i
                      <> encode e
  decode = do
    2 <- CBOR.decodeListLen
    CaseAlt <$> decode <*> decode



instance (Serialise v) => Serialise (Scope BindName 'One v) where
  encode (Scope (BindName1 v) e) = CBOR.encodeListLen 2
                                <> encode v
                                <> encode e
  decode = do
    2 <- CBOR.decodeListLen
    Scope <$> (BindName1 <$> decode) <*> decode

instance (Serialise v) => Serialise (Scope BindName 'Many v) where
  encode (Scope (BindNameN v) e) = CBOR.encodeListLen 2
                                <> encode v
                                <> encode e
  decode = do
    2 <- CBOR.decodeListLen
    Scope <$> (BindNameN <$> decode) <*> decode

instance (Serialise v) => Serialise (Scope BindDB 'One v) where
  encode (Scope BindDB1 e) = encode e
  decode = Scope BindDB1 <$> decode

instance (Serialise v) => Serialise (Scope BindDB 'Many v) where
  encode (Scope (BindDBN i) e) = CBOR.encodeListLen 2
                              <> encode i
                              <> encode e
  decode = do
    2 <- CBOR.decodeListLen
    Scope <$> (BindDBN <$> decode) <*> decode
    



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
