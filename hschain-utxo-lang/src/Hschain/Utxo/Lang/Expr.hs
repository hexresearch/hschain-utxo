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

type Money = Int64

newtype Expr a = Expr Lang
  deriving (Show, Eq)

data VarName = VarName
  { varName'loc   :: Maybe Loc
  , varName'name  :: Text
  } deriving (Show)

instance IsString VarName where
  fromString = VarName Nothing . fromString

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
  = PVar (Maybe Loc) VarName
  deriving (Show, Eq, Ord)

data Module = Module
  { module'loc   :: !(Maybe Loc)
  , module'binds :: !(BindGroup Lang)
  } deriving (Show)

type TypeContext = H.Context Loc

newtype ExecContext = ExecContext
  { unExecContext :: Map VarName Lang
  } deriving newtype (Show, Eq, Semigroup, Monoid)

-- | Evaluated module
data ModuleCtx = ModuleCtx
  { moduleCtx'types  :: TypeContext
  , moduleCtx'exprs  :: ExecContext
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
  , bind'alt  :: Alt a
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

type Lang = Fix E

data E a
  -- lambda calculus
  = Var (Maybe Loc) VarName
  | Apply (Maybe Loc) a a
  | InfixApply (Maybe Loc) a VarName a
  | Lam (Maybe Loc) VarName a
  | LamList (Maybe Loc) [VarName] a
  | Let (Maybe Loc) (BindGroup a) a
  | LetRec (Maybe Loc) VarName a a
  | Ascr (Maybe Loc) a Type
  -- primitives
  | PrimE (Maybe Loc) Prim
  -- logic
  | If (Maybe Loc) a a a
  | Pk (Maybe Loc) a
  -- tuples
  | Tuple (Maybe Loc) (Vector a)
  -- operations
  | UnOpE (Maybe Loc) UnOp a
  | BinOpE (Maybe Loc) BinOp a a
  -- environment
  | GetEnv (Maybe Loc) (EnvId a)
  -- vectors
  | VecE (Maybe Loc) (VecExpr a)
  -- text
  | TextE (Maybe Loc) (TextExpr a)
  -- boxes
  | BoxE (Maybe Loc) (BoxExpr a)
  -- undefined
  | Undef (Maybe Loc)
  -- debug
  | Trace (Maybe Loc) a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data UnOp = Not | Neg | TupleAt Int Int
  deriving (Show, Eq)

data BinOp
  = And | Or | Plus | Minus | Times | Div
  | Equals | NotEquals | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals
  deriving (Show, Eq)

data BoxExpr a
  = PrimBox (Maybe Loc) Box
  | BoxAt (Maybe Loc) a (BoxField a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data VecExpr a
  = NewVec (Maybe Loc) (Vector a)
  | VecAppend (Maybe Loc) a a
  | VecAt (Maybe Loc) a a
  | VecLength (Maybe Loc)
  | VecMap (Maybe Loc)
  | VecFold (Maybe Loc)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TextTypeTag = IntToText | BoolToText | ScriptToText
  deriving (Eq, Show)

data TextExpr a
  = TextAppend (Maybe Loc) a a
  | ConvertToText TextTypeTag (Maybe Loc)
  | TextLength (Maybe Loc)
  | TextHash (Maybe Loc) HashAlgo
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
  = Height (Maybe Loc)
  | Input (Maybe Loc)  a
  | Output (Maybe Loc) a
  | Self (Maybe Loc)
  | Inputs (Maybe Loc)
  | Outputs (Maybe Loc)
  | GetVar (Maybe Loc) a
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

boolT = boolT' Nothing
boxT  = boxT' Nothing
scriptT = scriptT' Nothing
textT = textT' Nothing

boxT' :: Maybe Loc -> Type
boxT' loc = H.conT loc "Box"

textT' :: Maybe Loc -> Type
textT' loc = H.conT loc "Text"

boolT' :: Maybe Loc -> Type
boolT' loc = H.conT loc "Bool"

scriptT' :: Maybe Loc -> Type
scriptT' loc = H.conT loc "Script"

vectorT :: Type -> Type
vectorT = vectorT' Nothing

vectorT' :: Maybe Loc -> Type -> Type
vectorT' loc a = H.appT loc (H.conT loc "Vector") a

{-
tupleT :: [Type] -> Type
tupleT = tupleT' Nothing
-}
tupleT' :: Maybe Loc -> [Type] -> Type
tupleT' loc ts = foldl (H.appT loc) cons ts
  where
    arity = length ts
    cons = H.conT loc $ mappend "Tuple" (showt arity)

tupleT :: [Type] -> Type
tupleT vs = foldl (\z n -> H.appT Nothing z n) (H.conT Nothing (mappend "Tuple" (showt size))) vs
  where
    size = length vs

--------------------------------
-- instances

instance H.HasLoc VarName where
  type Loc VarName = Loc
  getLoc (VarName loc _) = loc

instance H.HasLoc Lang where
  type Loc Lang = Loc
  getLoc (Fix expr) = H.getLoc expr

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

instance H.HasLoc a => H.HasLoc (Alt a) where
  type Loc (Alt a) = H.Loc a
  getLoc = H.getLoc . alt'expr

instance H.HasLoc (Bind a) where
  type Loc (Bind a) = Loc
  getLoc = H.getLoc . bind'name

instance H.HasLoc (BindGroup Lang) where
  type Loc (BindGroup Lang) = Loc
  getLoc = \case
    []  -> Nothing
    a:_ -> H.getLoc a

-------------------------------------------------------------------
-- unique instances for Eq and Ord (ingnores source location)
--

data VarName' = VarName' Text
  deriving (Eq, Ord)

uniqueVarName :: VarName -> VarName'
uniqueVarName (VarName _ name) = VarName' name

instance Eq VarName where
  (==) = (==) `on` uniqueVarName

instance Ord VarName where
  compare = compare `on` uniqueVarName

-------------------------------------------------------------------

freeVars :: Lang -> Set VarName
freeVars = cata $ \case
  Var _ v         -> Set.singleton v
  InfixApply _ a _ b -> a <> b
  Apply _ a b     -> a <> b
  Lam _ v a       -> Set.filter (/= v) a
  LamList _ vs a  -> a `Set.difference` (Set.fromList vs)
  Let _ bg a      -> (a `Set.difference` getBgNames bg) <> freeVarsBg bg
  LetRec _ v a b  -> a <> Set.filter (/= v) b
  Ascr _ a _      -> a
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
  where
    getBgNames :: BindGroup a -> Set VarName
    getBgNames bs = Set.fromList $ fmap bind'name bs

    freeVarsBg = foldMap (localFreeVarsAlt . bind'alt)
    localFreeVarsAlt Alt{..} = alt'expr `Set.difference` (freeVarsPat alt'pats)

freeVarsPat :: [Pat] -> Set VarName
freeVarsPat = Set.fromList . fmap (\(PVar _ name) -> name)

freeVarsAlt :: Alt Lang -> Set VarName
freeVarsAlt Alt{..} = freeVars alt'expr `Set.difference` freeVarsPat alt'pats

-------------------------------------------------------------------

sortBindGroups :: BindGroup Lang -> BindGroup Lang
sortBindGroups = (flattenSCC =<<) . stronglyConnComp . fmap toNode
   where
     toNode s = (s, bind'name s, Set.toList $ freeVarsAlt $ bind'alt s)

-------------------------------------------------------------------

$(deriveShow1 ''Alt)
$(deriveShow1 ''Bind)
$(deriveShow1 ''E)
$(deriveShow1 ''EnvId)
$(deriveShow1 ''BoxField)
$(deriveShow1 ''TextExpr)
$(deriveShow1 ''VecExpr)
$(deriveShow1 ''BoxExpr)

