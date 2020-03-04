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

import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Generics

-- import Language.HM ()

import Text.Show.Deriving

import Hschain.Utxo.Lang.Sigma

import qualified Language.HM as H
import qualified Language.Haskell.Exts.SrcLoc as Hask

type Loc = Hask.SrcSpanInfo
type Type = H.Type Loc
type Signature = H.Signature Loc

noLoc :: Loc
noLoc = Hask.noSrcSpan

type Money = Pico

newtype Expr a = Expr Lang
  deriving (Show, Eq)

data VarName = VarName
  { varName'loc   :: Loc
  , varName'name  :: Text
  } deriving (Show)

instance IsString VarName where
  fromString = VarName noLoc . fromString

{-
fromVarName :: VarName -> Text
fromVarName (VarName loc name) = Id loc name

toVarName :: Id -> VarName
toVarName (Id loc txt) = VarName loc txt
-}

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
  deriving (Show, Eq, Ord)
  -- | PWildcard Loc
  -- | PLit Loc Prim
  -- | PAs Id Pat
  -- | PNpk Id Integer
  -- | PCon Assump [Pat]

data Module = Module
  { module'loc   :: !Loc
  , module'binds :: ![BindGroup Lang]
  } deriving (Show)

data Alt a = Alt
  { alt'pats :: [Pat]
  , alt'expr :: a
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data BindGroup a = BindGroup
  { bindGroup'expl :: [Expl a]
  , bindGroup'impl :: [[Impl a]]
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)


data Expl a = Expl
  { expl'name  :: VarName
  , expl'type  :: Signature
  , expl'alts  :: [Alt a]
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Impl a = Impl
  { impl'name  :: VarName
  , impl'alts  :: [Alt a]
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

type Lang = Fix E

data E a
  -- lambda calculus
  = Var Loc VarName
  | Apply Loc a a
  | InfixApply Loc a VarName a
  | Lam Loc VarName a
  | LamList Loc [VarName] a
  | Let Loc (BindGroup a) a
  | LetRec Loc VarName a a
  | Ascr Loc a Type
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

data UnOp = Not | Neg | TupleAt Int
  deriving (Show, Eq)

data BinOp
  = And | Or | Plus | Minus | Times | Div
  | Equals | NotEquals | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals
  | ComposeFun
  deriving (Show, Eq)

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

data TextTypeTag = IntToText | DoubleToText | BoolToText | ScriptToText
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
  = PrimInt     Int
  | PrimDouble  Pico
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
    PrimDouble d   -> "double" .= d
    PrimString txt -> "text"   .= txt
    PrimBool b     -> "bool"   .= b
    PrimSigma s    -> "sigma"  .= toJSON s

-- todo: rewrite this instance
-- to distinguish between numeric types of int, double and money
instance FromJSON Prim where
  parseJSON = withObject "prim" $ \v ->
        fmap PrimInt    (v .: "int")
    <|> fmap PrimDouble (v .: "double")
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

tupleT :: [Type] -> Type
tupleT = tupleT' noLoc

tupleT' :: Loc -> [Type] -> Type
tupleT' loc ts = foldl (H.appT loc) cons ts
  where
    arity = length ts
    cons = H.conT loc $ mappend "Tuple" (showt arity)

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

instance H.HasLoc (Expl a) where
  type Loc (Expl a) = Loc
  getLoc = H.getLoc . expl'name

instance H.HasLoc (Impl a) where
  type Loc (Impl a) = Loc
  getLoc = H.getLoc . impl'name

instance H.HasLoc (BindGroup Lang) where
  type Loc (BindGroup Lang) = Loc
  getLoc BindGroup{..} = case bindGroup'expl of
    a:_ -> H.getLoc $ expl'name a
    []  -> case bindGroup'impl of
      iss:_ -> case iss of
        is:_ -> H.getLoc $ impl'name is
        []   -> noLoc
      []   -> noLoc

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

$(deriveShow1 ''Alt)
$(deriveShow1 ''Impl)
$(deriveShow1 ''Expl)
$(deriveShow1 ''BindGroup)
$(deriveShow1 ''E)
$(deriveShow1 ''EnvId)
$(deriveShow1 ''BoxField)
$(deriveShow1 ''TextExpr)
$(deriveShow1 ''VecExpr)
$(deriveShow1 ''BoxExpr)

