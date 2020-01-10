module Hschain.Utxo.Lang.Expr where

import Hex.Common.Text

import Control.Applicative

import Data.Aeson

import Data.Fix
import Data.Fixed
import Data.Function (on)
import Data.Functor.Classes

import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)

import Type.Loc
import Type.Type

import Text.Show.Deriving

import Hschain.Utxo.Lang.Sigma

newtype Expr a = Expr Lang
  deriving (Show, Eq)

data VarName = VarName
  { varName'loc   :: Loc
  , varName'name  :: Text
  } deriving (Show)

instance IsString VarName where
  fromString = VarName noLoc . fromString

fromVarName :: VarName -> Id
fromVarName (VarName loc name) = Id loc name

toVarName :: Id -> VarName
toVarName (Id loc txt) = VarName loc txt

type Args = Map Text (Prim )

newtype BoxId = BoxId { unBoxId :: Text }
  deriving (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype Script = Script { unScript :: Text }
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

data Box = Box
  { box'id     :: !BoxId
  , box'value  :: !Money
  , box'script :: !Script
  , box'args   :: !Args
  }
  deriving (Show, Eq)

data Pat
  = PVar Loc Id
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
  { expl'name  :: Id
  , expl'type  :: Scheme
  , expl'alts  :: [Alt a]
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Impl a = Impl
  { impl'name  :: Id
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

type Money  = Pico

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

data TextExpr a
  = TextAppend Loc a a
  | ConvertToText Loc
  | TextLength Loc
  | TextHash Loc HashAlgo
  deriving (Eq, Show, Functor, Foldable, Traversable)

data HashAlgo = Sha256 | Blake2b256
  deriving (Eq, Show)

data Prim
  = PrimInt     Int
  | PrimMoney   Money
  | PrimDouble  Double
  | PrimString  Text
  | PrimBool    Bool
  | PrimSigma   (Sigma PublicKey)
  deriving (Show, Eq, Ord)

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
    PrimMoney m    -> "money"  .= m
    PrimDouble d   -> "double" .= d
    PrimString txt -> "text"   .= txt
    PrimBool b     -> "bool"   .= b
    PrimSigma s    -> "sigma"  .= toJSON s

-- todo: rewrite this instance
-- to distinguish between numeric types of int, double and money
instance FromJSON Prim where
  parseJSON = withObject "prim" $ \v ->
        fmap PrimInt    (v .: "int")
    <|> fmap PrimMoney  (v .: "money")
    <|> fmap PrimDouble (v .: "double")
    <|> fmap PrimString (v .: "text")
    <|> fmap PrimBool   (v .: "bool")
    <|> (fmap PrimSigma . parseJSON =<< (v .: "sigma"))

---------------------------------
-- type constants

boolT, boxT, scriptT, textT, moneyT :: Type

boolT = boolT' noLoc
boxT  = boxT' noLoc
scriptT = scriptT' noLoc
textT = textT' noLoc
moneyT = moneyT' noLoc

boxT' :: Loc -> Type
boxT' loc = TCon loc (Tycon loc (Id loc "Box") (Star loc))

moneyT' :: Loc -> Type
moneyT' loc = TCon loc (Tycon loc (Id loc "Money") (Star loc))

textT' :: Loc -> Type
textT' loc = TCon loc (Tycon loc (Id loc "Text") (Star loc))

boolT' :: Loc -> Type
boolT' loc = TCon loc (Tycon loc (Id loc "Bool") (Star loc))

scriptT' :: Loc -> Type
scriptT' loc = TCon loc (Tycon loc (Id loc "Script") (Star loc))

vectorT :: Type -> Type
vectorT = vectorT' noLoc

vectorT' :: Loc -> Type -> Type
vectorT' loc a = TAp loc (TCon loc (Tycon loc (Id loc "Vector") (Kfun loc (Star loc) (Star loc)))) a

tupleT :: [Type] -> Type
tupleT = tupleT' noLoc

tupleT' :: Loc -> [Type] -> Type
tupleT' loc ts = foldl (TAp loc) cons ts
  where
    arity = length ts

    kind = foldr1 (Kfun loc) $ replicate arity (Star loc)
    cons = TCon loc $ Tycon loc (Id loc $ mappend "Tuple" (showt arity)) kind

--------------------------------
-- instances

instance HasLoc VarName where
  getLoc (VarName loc _) = loc

instance HasLoc Lang where
  getLoc (Fix expr) = getLoc expr

instance Show a => HasLoc (E a) where
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

instance HasLoc a => HasLoc (Alt a) where
  getLoc = getLoc . alt'expr

instance HasLoc (Expl a) where
  getLoc = getLoc . expl'name

instance HasLoc (Impl a) where
  getLoc = getLoc . impl'name

instance HasLoc (BindGroup Lang) where
  getLoc BindGroup{..} = case bindGroup'expl of
    a:_ -> getLoc $ expl'name a
    []  -> case bindGroup'impl of
      iss:_ -> case iss of
        is:_ -> getLoc $ impl'name is
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

