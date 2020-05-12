module Hschain.Utxo.Lang.Expr where

import Hex.Common.Text

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad

import Codec.Serialise

import Data.Aeson

import Data.Fix
import Data.Fixed
import Data.Function (on)
import Data.Functor.Classes
import Data.Foldable
import Data.Graph
import Data.Int
import Data.Map.Strict (Map)
import Data.Maybe
import Data.String
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Generics

import Text.Show.Deriving

import Hschain.Utxo.Lang.Sigma

import qualified Language.HM as H
import qualified Language.HM.Pretty as H
import qualified Language.Haskell.Exts.SrcLoc as Hask

import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Vector as V

type Loc = Hask.SrcSpanInfo
type Type = H.Type Loc Text
type TypeError = H.TypeError Loc Text
type Signature = H.Signature Loc Text

noLoc :: Loc
noLoc = Hask.noSrcSpan

data UserTypeCtx = UserTypeCtx
  { userTypeCtx'types      :: Map VarName UserType
  , userTypeCtx'recConstrs :: Map ConsName RecordFieldOrder
  } deriving (Show, Eq)

instance Semigroup UserTypeCtx where
  UserTypeCtx a1 b1 <> UserTypeCtx a2 b2 = UserTypeCtx (a1 <> a2) (b1 <> b2)

instance Monoid UserTypeCtx where
  mempty = UserTypeCtx mempty mempty

setupRecConstrs :: UserTypeCtx -> UserTypeCtx
setupRecConstrs ctx = ctx { userTypeCtx'recConstrs = recConstrs }
  where
    recConstrs = M.fromList $ mapMaybe getConstr . M.toList . userType'cases =<< types

    getConstr (cons, def) = case def of
      ConsDef _ -> Nothing
      RecordCons fields -> Just $ (cons, RecordFieldOrder $
          fmap (varName'name . recordField'name) $ V.toList fields)


    types = M.elems $ userTypeCtx'types ctx

data UserType = UserType
  { userType'name       :: !VarName
  , userType'args       :: ![VarName]
  , userType'cases      :: !(Map ConsName ConsDef)
  } deriving (Show, Eq)

getConsTypes :: ConsDef -> Vector Type
getConsTypes = \case
  ConsDef ts        -> ts
  RecordCons fields -> fmap recordField'type fields

data ConsDef
  = ConsDef (Vector Type)
  | RecordCons (Vector RecordField)
  deriving (Show, Eq)

data RecordField = RecordField
  { recordField'name :: VarName
  , recordField'type :: Type
  } deriving (Show, Eq)

newtype RecordFieldOrder = RecordFieldOrder
  { unRecordFieldOrder :: [Text]
  } deriving (Show, Eq)

type Money = Int64

newtype Expr a = Expr Lang
  deriving (Show, Eq)

data VarName = VarName
  { varName'loc   :: !Loc
  , varName'name  :: !Text
  } deriving (Show)

instance IsString VarName where
  fromString = VarName noLoc . fromString

instance H.IsVar Text where
  intToVar n = mappend "$$" (showt n)
  prettyLetters = fmap fromString $ [1..] >>= flip replicateM ['a'..'z']

instance H.HasPrefix Text where
  getFixity = const Nothing

data ConsName = ConsName
  { consName'loc  :: !Loc
  , consName'name :: !Text
  } deriving (Show)

instance IsString ConsName where
  fromString = ConsName noLoc . fromString

consToVarName :: ConsName -> VarName
consToVarName (ConsName loc name) = VarName loc name

varToConsName :: VarName -> ConsName
varToConsName VarName{..} = ConsName varName'loc varName'name

type Args = Map Text (Prim )

newtype BoxId = BoxId { unBoxId :: Text }
  deriving newtype  (Show, Eq, Ord, NFData, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving stock    (Generic)
  deriving anyclass (Serialise)

newtype Script = Script { unScript :: Text }
  deriving newtype  (Show, Eq, Ord, NFData, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving stock    (Generic)
  deriving anyclass (Serialise)

data Box = Box
  { box'id     :: !BoxId
  , box'value  :: !Money
  , box'script :: !Script
  , box'args   :: !Args
  }
  deriving (Show, Eq, Ord, Generic, Serialise, NFData)

data Pat
  = PVar Loc VarName
  | PPrim Loc Prim
  | PCons Loc ConsName [Pat]
  | PTuple Loc [Pat]
  | PWildCard Loc
  deriving (Show, Eq, Ord)

instance IsString Pat where
  fromString = PVar noLoc . fromString

data Module = Module
  { module'loc       :: !Loc
  , module'userTypes :: !UserTypeCtx
  , module'binds     :: !(BindGroup Lang)
  } deriving (Show)

type TypeContext = H.Context Loc Text

newtype ExecCtx = ExecCtx
  { execCtx'vars  :: Map VarName Lang
  } deriving newtype (Show, Eq)

instance Semigroup ExecCtx where
  ExecCtx a1 <> ExecCtx a2 = ExecCtx (a1 <> a2)

instance Monoid ExecCtx where
  mempty = ExecCtx mempty

data InferCtx = InferCtx
  { inferCtx'binds :: TypeContext
  , inferCtx'types :: UserTypeCtx
  } deriving (Show, Eq)

instance Semigroup InferCtx where
  a <> b = InferCtx
      { inferCtx'binds = inferCtx'binds a <> inferCtx'binds b
      , inferCtx'types = inferCtx'types a <> inferCtx'types b
      }

instance Monoid InferCtx where
  mempty = InferCtx mempty mempty

-- | Evaluated module
data ModuleCtx = ModuleCtx
  { moduleCtx'types  :: !InferCtx
  , moduleCtx'exprs  :: !ExecCtx
  } deriving (Show, Eq)

getModuleCtxNames :: ModuleCtx -> [Text]
getModuleCtxNames = M.keys . H.unContext . inferCtx'binds . moduleCtx'types

instance Semigroup ModuleCtx where
  (<>) a b = ModuleCtx
    { moduleCtx'types = moduleCtx'types a <> moduleCtx'types b
    , moduleCtx'exprs = moduleCtx'exprs a <> moduleCtx'exprs b
    }

instance Monoid ModuleCtx where
  mempty = ModuleCtx
    { moduleCtx'types = mempty
    , moduleCtx'exprs = mempty
    }

data Alt a = Alt
  { alt'pats  :: [Pat]
  , alt'expr  :: Rhs a
  , alt'where :: Maybe (BindGroup a)
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Rhs a
  = UnguardedRhs a
  | GuardedRhs [Guard a]
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Guard a = Guard
  { guard'predicate :: a
  , guard'rhs       :: a
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)


type BindGroup a = [Bind a]

data Bind a = Bind
  { bind'name  :: VarName
  , bind'type  :: Maybe Signature
  , bind'alts  :: [Alt a]
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

type Lang = Fix E

data E a
  -- lambda calculus
  = Var Loc VarName
  | Apply Loc a a
  | InfixApply Loc a VarName a
  | Lam Loc Pat a
  | LamList Loc [Pat] a
  | Let Loc (BindGroup a) a
  | Ascr Loc a Signature
  -- case
  | Cons Loc ConsName (Vector a)
  | CaseOf Loc a [CaseExpr a]
  -- records
  | RecConstr Loc ConsName [(VarName, a)]
  | RecUpdate Loc a [(VarName, a)]
  -- Alternatives
  | AltE Loc a a
  | FailCase Loc
  -- primitives
  | PrimE Loc Prim
  -- logic
  | If Loc a a a
  | Pk Loc a
  -- tuples
  | Tuple Loc (Vector a)
  -- operations
  | UnOpE Loc UnOp a
  | BinOpE Loc BinOp a a
  -- environment
  | GetEnv Loc (EnvId a)
  -- vectors
  | VecE Loc (VecExpr a)
  -- text
  | TextE Loc (TextExpr a)
  -- boxes
  | BoxE Loc (BoxExpr a)
  -- undefined
  | Undef Loc
  -- debug
  | Trace Loc a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data UnOp = Not | Neg | TupleAt Int Int
  deriving (Show, Eq)

data BinOp
  = And | Or | Plus | Minus | Times | Div
  | Equals | NotEquals | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals
  deriving (Show, Eq)

data CaseExpr a
  = CaseExpr
      { caseExpr'lhs :: Pat
      , caseExpr'rhs :: a
      }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CaseLhs = CaseLhs
  { caseLhs'cons :: ConsName
  , caseLhs'vars :: [VarName]
  } deriving (Eq, Show)

data BoxExpr a
  = PrimBox Loc Box
  | BoxAt Loc a (BoxField a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data VecExpr a
  = NewVec Loc (Vector a)
  | VecAppend Loc a a
  | VecAt Loc a a
  | VecLength Loc
  | VecMap Loc
  | VecFold Loc
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TextTypeTag = IntToText | BoolToText | ScriptToText
  deriving (Eq, Show)

data TextExpr a
  = TextAppend Loc a a
  | ConvertToText TextTypeTag Loc
  | TextLength Loc
  | TextHash Loc HashAlgo
  deriving (Eq, Show, Functor, Foldable, Traversable)

data HashAlgo = Sha256 | Blake2b256
  deriving (Eq, Show)

data Prim
  = PrimInt     Int64
  | PrimString  Text
  | PrimBool    Bool
  | PrimSigma   (Sigma PublicKey)
  deriving (Show, Eq, Ord, Generic, Serialise, NFData)

data EnvId a
  = Height Loc
  | Input Loc  a
  | Output Loc a
  | Self Loc
  | Inputs Loc
  | Outputs Loc
  | GetVar Loc a
  -- ^ refers to the box where it's defined
  deriving (Show, Eq, Functor, Foldable, Traversable)

data BoxField a = BoxFieldId | BoxFieldValue | BoxFieldScript | BoxFieldArg a
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance ToJSON Prim where
  toJSON x = object $ pure $ case x of
    PrimInt n      -> "int"    .= n
    PrimString txt -> "text"   .= txt
    PrimBool b     -> "bool"   .= b
    PrimSigma s    -> "sigma"  .= toJSON s

-- todo: rewrite this instance
-- to distinguish between numeric types of int, double and money
instance FromJSON Prim where
  parseJSON = withObject "prim" $ \v ->
        fmap PrimInt    (v .: "int")
    <|> fmap PrimString (v .: "text")
    <|> fmap PrimBool   (v .: "bool")
    <|> (fmap PrimSigma . parseJSON =<< (v .: "sigma"))

---------------------------------
-- type constants

intT, boolT, boxT, scriptT, textT :: Type

intT = intT' noLoc
boolT = boolT' noLoc
boxT  = boxT' noLoc
scriptT = scriptT' noLoc
textT = textT' noLoc

constType :: Text -> Loc -> Type
constType name loc = H.conT loc name []

boxT' :: Loc -> Type
boxT' = constType "Box"

textT' :: Loc -> Type
textT' = constType "Text"

intT' :: Loc -> Type
intT' = constType "Int"

boolT' :: Loc -> Type
boolT' = constType "Bool"

scriptT' :: Loc -> Type
scriptT' = constType "Script"

vectorT :: Type -> Type
vectorT = vectorT' noLoc

vectorT' :: Loc -> Type -> Type
vectorT' loc a = H.listT loc a


tupleT :: [Type] -> Type
tupleT = tupleT' noLoc

tupleT' ::  Loc -> [Type] -> Type
tupleT' loc ts = H.tupleT loc ts

--------------------------------
-- instances

instance H.HasLoc VarName where
  type Loc VarName = Loc
  getLoc (VarName loc _) = loc

instance H.HasLoc ConsName where
  type Loc ConsName = Loc
  getLoc (ConsName loc _) = loc

instance H.HasLoc Lang where
  type Loc Lang = Loc
  getLoc (Fix expr) = H.getLoc expr

instance H.HasLoc Pat where
  type Loc Pat = Loc
  getLoc = \case
    PVar loc _ -> loc
    PPrim loc _ -> loc
    PCons loc _ _ -> loc
    PTuple loc _ -> loc
    PWildCard loc -> loc

instance Show a => H.HasLoc (E a) where
  type Loc (E a) = Loc
  getLoc = \case
    Var loc _ -> loc
    Apply loc _ _ -> loc
    InfixApply loc _ _ _ -> loc
    Lam loc _ _ -> loc
    LamList loc _ _ -> loc
    Let loc _ _ -> loc
    Ascr loc _ _ -> loc
    -- case-expr
    Cons loc _ _ -> loc
    CaseOf loc _ _ -> loc
    -- record
    RecConstr loc _ _ -> loc
    RecUpdate loc _ _ -> loc
    -- primitives
    PrimE loc _ -> loc
    -- logic
    If loc _ _ _ -> loc
    Pk loc _ -> loc
    -- tuples
    Tuple loc _ -> loc
    -- operations
    UnOpE loc _ _ -> loc
    BinOpE loc _ _ _ -> loc
    -- environment
    GetEnv loc _ -> loc
    -- vectors
    VecE loc _ -> loc
    -- text
    TextE loc _ -> loc
    -- boxes
    BoxE loc _ -> loc
    -- undefined
    Undef loc -> loc
    -- debug
    Trace loc _ _ -> loc
    AltE loc _ _ -> loc
    FailCase loc -> loc

instance H.HasLoc a => H.HasLoc (Alt a) where
  type Loc (Alt a) = H.Loc a
  getLoc = H.getLoc . alt'expr

instance H.HasLoc a => H.HasLoc (Rhs a) where
  type Loc (Rhs a) = H.Loc a
  getLoc = \case
    UnguardedRhs a -> H.getLoc a
    GuardedRhs  as -> case as of
      a:_ -> H.getLoc $ guard'rhs a
      []  -> error "Empty guard"

instance H.HasLoc (Bind a) where
  type Loc (Bind a) = Loc
  getLoc = H.getLoc . bind'name

instance H.HasLoc (BindGroup Lang) where
  type Loc (BindGroup Lang) = Loc
  getLoc = \case
    []  -> noLoc
    a:_ -> H.getLoc a

-------------------------------------------------------------------
-- unique instances for Eq and Ord (ingnores source location)
--

instance Eq VarName where
  (==) = (==) `on` varName'name

instance Ord VarName where
  compare = compare `on` varName'name

instance Eq ConsName where
  (==) = (==) `on` consName'name

instance Ord ConsName where
  compare = compare `on` consName'name

-------------------------------------------------------------------

freeVars :: Lang -> Set VarName
freeVars = cata $ \case
  Var _ v         -> Set.singleton v
  InfixApply _ a _ b -> a <> b
  Apply _ a b      -> a <> b
  Lam _ v a        -> a `Set.difference`  freeVarsPat v
  LamList _ vs a   -> a `Set.difference` (foldMap freeVarsPat vs)
  Let _ bg a       -> (a `Set.difference` getBgNames bg) <> freeVarsBg bg
  Ascr _ a _       -> a
  Cons _ _ vs      -> mconcat $ V.toList vs
  CaseOf _ a alts  -> mappend a (foldMap freeCaseExpr alts)
  RecConstr _ _ ts -> mconcat $ fmap snd ts
  RecUpdate _ a ts -> mconcat $ a : fmap snd ts
  PrimE _ _        -> Set.empty
  If _ a b c       -> mconcat [a, b, c]
  Pk _ a           -> a
  Tuple _ vs       -> fold $ V.toList vs
  UnOpE _ _ a      -> a
  BinOpE _ _ a b   -> mconcat [a, b]
  GetEnv _ env     -> fold env
  VecE _ vec       -> fold vec
  TextE _ txt      -> fold txt
  BoxE _ box       -> fold box
  Undef _          -> Set.empty
  Trace _ a b      -> mconcat [a, b]
  AltE _ a b       -> mappend a b
  FailCase _       -> Set.empty
  where
    getBgNames :: BindGroup a -> Set VarName
    getBgNames bs = Set.fromList $ fmap bind'name bs

    freeVarsBg = foldMap (foldMap localFreeVarsAlt . bind'alts)
    localFreeVarsAlt Alt{..} =
      (freeVarsRhs alt'expr <> foldMap getBgNames alt'where)
      `Set.difference` (foldMap freeVarsPat alt'pats)

    freeCaseExpr CaseExpr{..} = caseExpr'rhs `Set.difference` (freeVarsPat caseExpr'lhs)

freeVarsRhs :: Rhs (Set VarName) -> Set VarName
freeVarsRhs = \case
  UnguardedRhs a -> a
  GuardedRhs as -> foldMap freeVarsGuard as

freeVarsGuard Guard{..} = guard'predicate <> guard'rhs


freeVarsPat :: Pat -> Set VarName
freeVarsPat = \case
  PVar _ name -> Set.singleton name
  PPrim _ _ -> Set.empty
  PCons _ _ vs -> foldMap freeVarsPat vs
  PTuple _ vs -> foldMap freeVarsPat vs
  PWildCard _ -> Set.empty

freeVarsAlt :: Alt Lang -> Set VarName
freeVarsAlt Alt{..} =
  freeVarsRhs (fmap freeVars alt'expr) `Set.difference` foldMap freeVarsPat alt'pats

-------------------------------------------------------------------

sortBindGroups :: BindGroup Lang -> BindGroup Lang
sortBindGroups = (flattenSCC =<<) . stronglyConnComp . fmap toNode
   where
     toNode s = (s, bind'name s, Set.toList $ foldMap freeVarsAlt $ bind'alts s)

-------------------------------------------------------------------

$(deriveShow1 ''Alt)
$(deriveShow1 ''Rhs)
$(deriveShow1 ''Guard)
$(deriveShow1 ''Bind)
$(deriveShow1 ''E)
$(deriveShow1 ''EnvId)
$(deriveShow1 ''CaseExpr)
$(deriveShow1 ''BoxField)
$(deriveShow1 ''TextExpr)
$(deriveShow1 ''VecExpr)
$(deriveShow1 ''BoxExpr)

