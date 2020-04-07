module Hschain.Utxo.Lang.Expr where

import Hex.Common.Text

import Control.Applicative
import Control.DeepSeq (NFData)

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
import Data.String
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Generics

import Text.Show.Deriving

import Hschain.Utxo.Lang.Sigma

import qualified Language.HM as H
import qualified Language.Haskell.Exts.SrcLoc as Hask

import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Vector as V

type Loc = Hask.SrcSpanInfo
type Type = H.Type Loc
type TypeError = H.TypeError Loc
type Signature = H.Signature Loc

noLoc :: Loc
noLoc = Hask.noSrcSpan

newtype UserTypeCtx = UserTypeCtx (Map VarName UserType)
  deriving newtype (Show, Eq, Semigroup, Monoid)

data UserType = UserType
  { userType'name  :: !VarName
  , userType'args  :: ![VarName]
  , userType'cases :: !(Map ConsName (Vector Type))
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

data ConsName = ConsName
  { consName'loc  :: !Loc
  , consName'name :: !Text
  } deriving (Show)

instance IsString ConsName where
  fromString = ConsName noLoc . fromString

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

type TypeContext = H.Context Loc

newtype ExecContext = ExecContext
  { unExecContext :: Map VarName Lang
  } deriving newtype (Show, Eq, Semigroup, Monoid)

-- | Evaluated module
data ModuleCtx = ModuleCtx
  { moduleCtx'types  :: !TypeContext
  , moduleCtx'exprs  :: !ExecContext
  } deriving (Show, Eq)

getModuleCtxNames :: ModuleCtx -> [Text]
getModuleCtxNames = M.keys . H.unContext . moduleCtx'types

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
  { alt'pats :: [Pat]
  , alt'expr :: a
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
  | LetRec Loc VarName a a
  | Ascr Loc a Signature
  -- case
  | Cons Loc ConsName (Vector a)
  | CaseOf Loc a [CaseExpr a]
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

boolT, boxT, scriptT, textT :: Type

boolT = boolT' noLoc
boxT  = boxT' noLoc
scriptT = scriptT' noLoc
textT = textT' noLoc

boxT' :: Loc -> Type
boxT' loc = H.conT loc "Box"

textT' :: Loc -> Type
textT' loc = H.conT loc "Text"

boolT' :: Loc -> Type
boolT' loc = H.conT loc "Bool"

scriptT' :: Loc -> Type
scriptT' loc = H.conT loc "Script"

vectorT :: Type -> Type
vectorT = vectorT' noLoc

vectorT' :: Loc -> Type -> Type
vectorT' loc a = H.appT loc (H.conT loc "Vector") a

{-
tupleT :: [Type] -> Type
tupleT = tupleT' noLoc
-}
tupleT' :: Loc -> [Type] -> Type
tupleT' loc ts = foldl (H.appT loc) cons ts
  where
    arity = length ts
    cons = H.conT loc $ mappend "Tuple" (showt arity)

tupleT :: [Type] -> Type
tupleT vs = foldl (\z n -> H.appT noLoc z n) (H.conT noLoc (mappend "Tuple" (showt size))) vs
  where
    size = length vs

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
    LetRec loc _ _ _ -> loc
    Ascr loc _ _ -> loc
    -- case-expr
    Cons loc _ _ -> loc
    CaseOf loc _ _ -> loc
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
  Apply _ a b     -> a <> b
  Lam _ v a       -> a `Set.difference`  freeVarsPat v
  LamList _ vs a  -> a `Set.difference` (foldMap freeVarsPat vs)
  Let _ bg a      -> (a `Set.difference` getBgNames bg) <> freeVarsBg bg
  LetRec _ v a b  -> a <> Set.filter (/= v) b
  Ascr _ a _      -> a
  Cons _ _ vs     -> mconcat $ V.toList vs
  CaseOf _ a alts -> mappend a (foldMap freeCaseExpr alts)
  PrimE _ _       -> Set.empty
  If _ a b c      -> mconcat [a, b, c]
  Pk _ a          -> a
  Tuple _ vs      -> fold $ V.toList vs
  UnOpE _ _ a     -> a
  BinOpE _ _ a b  -> mconcat [a, b]
  GetEnv _ env    -> fold env
  VecE _ vec      -> fold vec
  TextE _ txt     -> fold txt
  BoxE _ box      -> fold box
  Undef _         -> Set.empty
  Trace _ a b     -> mconcat [a, b]
  AltE _ a b      -> mappend a b
  FailCase _      -> Set.empty
  where
    getBgNames :: BindGroup a -> Set VarName
    getBgNames bs = Set.fromList $ fmap bind'name bs

    freeVarsBg = foldMap (foldMap localFreeVarsAlt . bind'alts)
    localFreeVarsAlt Alt{..} = alt'expr `Set.difference` (foldMap freeVarsPat alt'pats)

    freeCaseExpr CaseExpr{..} = caseExpr'rhs `Set.difference` (freeVarsPat caseExpr'lhs)


freeVarsPat :: Pat -> Set VarName
freeVarsPat = \case
  PVar _ name -> Set.singleton name
  PPrim _ _ -> Set.empty
  PCons _ _ vs -> foldMap freeVarsPat vs
  PTuple _ vs -> foldMap freeVarsPat vs
  PWildCard _ -> Set.empty

freeVarsAlt :: Alt Lang -> Set VarName
freeVarsAlt Alt{..} = freeVars alt'expr `Set.difference` foldMap freeVarsPat alt'pats

-------------------------------------------------------------------

sortBindGroups :: BindGroup Lang -> BindGroup Lang
sortBindGroups = (flattenSCC =<<) . stronglyConnComp . fmap toNode
   where
     toNode s = (s, bind'name s, Set.toList $ foldMap freeVarsAlt $ bind'alts s)

-------------------------------------------------------------------

$(deriveShow1 ''Alt)
$(deriveShow1 ''Bind)
$(deriveShow1 ''E)
$(deriveShow1 ''EnvId)
$(deriveShow1 ''CaseExpr)
$(deriveShow1 ''BoxField)
$(deriveShow1 ''TextExpr)
$(deriveShow1 ''VecExpr)
$(deriveShow1 ''BoxExpr)

