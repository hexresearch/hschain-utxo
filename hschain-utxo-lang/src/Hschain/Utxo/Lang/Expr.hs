module Hschain.Utxo.Lang.Expr where

import Hex.Common.Text

import Control.Applicative

import Data.Aeson

import Data.Fix
import Data.Fixed
import Data.Functor.Classes

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import Type.Type

import Text.Show.Deriving

newtype Expr a = Expr Lang
  deriving (Show, Read, Eq)

type VarName = Text

type Args = Map Text Prim

newtype BoxId = BoxId { unBoxId :: Text }
  deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype Script = Script { unScript :: Text }
  deriving (Show, Read, Eq, Ord, ToJSON, FromJSON)

data Box = Box
  { box'id     :: !BoxId
  , box'value  :: !Money
  , box'script :: !Script
  , box'args   :: !Args
  }
  deriving (Show, Read, Eq)

data Pat
  = PVar Id
  | PWildcard
  | PLit Prim
  deriving (Show, Read, Eq, Ord)
  -- | PAs Id Pat
  -- | PNpk Id Integer
  -- | PCon Assump [Pat]

type Program = [BindGroup Lang]

data Alt a = Alt
  { alt'pats :: [Pat]
  , alt'expr :: a
  } deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)

data BindGroup a = BindGroup
  { bindGroup'expl :: [Expl a]
  , bindGroup'impl :: [[Impl a]]
  } deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)

data Expl a = Expl
  { expl'name  :: Id
  , expl'type  :: Scheme
  , expl'alts  :: [Alt a]
  } deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)

data Impl a = Impl
  { impl'name  :: Id
  , impl'alts  :: [Alt a]
  } deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)

data TypeExpr a
  = UknownType
  | BoolType
  | IntType
  | MoneyType
  | DoubleType
  | StringType
  | BoxType
  | VectorType a
  | PairType a a
  | FunctionType a a
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type Lang = Fix E

data E a
  -- lambda calculus
  = Var VarName
  | Apply a a
  | Lam VarName a
  | LamList [VarName] a
  | Let (BindGroup a) a
  | LetArg VarName [VarName] a a
  | LetRec VarName a a
  | Ascr a Type
  -- primitives
  | PrimE Prim
  -- logic
  | If a a a
  | Pk a
  -- tuples
  | Tuple (Vector a)
  -- operations
  | UnOpE UnOp a
  | BinOpE BinOp a a
  -- environment
  | GetEnv (EnvId a)
  -- vectors
  | VecE (VecExpr a)
  -- text
  | TextE (TextExpr a)
  -- boxes
  | BoxE (BoxExpr a)
  -- undefined
  | Undef
  -- debug
  | Trace a a
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data UnOp = Not | Neg | TupleAt Int
  deriving (Show, Read, Eq)

data BinOp
  = And | Or | Plus | Minus | Times | Div
  | Equals | NotEquals | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals
  | ComposeFun
  deriving (Show, Read, Eq)

type Money  = Pico

data BoxExpr a
  = PrimBox Box
  | BoxAt a (BoxField a)
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data VecExpr a
  = NewVec (Vector a)
  | VecAppend a a
  | VecAt a a
  | VecLength
  | VecMap
  | VecFold
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data TextExpr a
  = TextAppend a a
  | ConvertToText
  | TextLength
  | TextHash HashAlgo
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data HashAlgo = Sha256 | Blake2b256
  deriving (Eq, Show, Read)

data Prim
  = PrimInt    Int
  | PrimMoney  Money
  | PrimDouble Double
  | PrimString Text
  | PrimBool   Bool
  deriving (Show, Read, Eq, Ord)

data EnvId a
  = Height
  | Input  a
  | Output a
  | Self
  | Inputs
  | Outputs
  | GetVar a
  -- ^ refers to the box where it's defined
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

data BoxField a = BoxFieldId | BoxFieldValue | BoxFieldScript | BoxFieldArg a
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance ToJSON Prim where
  toJSON x = object $ pure $ case x of
    PrimInt n      -> "int"    .= n
    PrimMoney m    -> "money"  .= m
    PrimDouble d   -> "double" .= d
    PrimString txt -> "text"   .= txt
    PrimBool b     -> "bool"   .= b

-- todo: rewrite this instance
-- to distinguish between numeric types of int, double and money
instance FromJSON Prim where
  parseJSON = withObject "prim" $ \v ->
        fmap PrimInt    (v .: "int")
    <|> fmap PrimMoney  (v .: "money")
    <|> fmap PrimDouble (v .: "double")
    <|> fmap PrimString (v .: "text")
    <|> fmap PrimBool   (v .: "bool")

$(deriveShow1 ''TypeExpr)
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

---------------------------------
-- type constants

boxT :: Type
boxT = TCon (Tycon "Box" Star)

moneyT :: Type
moneyT = TCon (Tycon "Money" Star)

textT :: Type
textT = TCon (Tycon "Text" Star)

boolT :: Type
boolT = TCon (Tycon "Bool" Star)

scriptT :: Type
scriptT = TCon (Tycon "Script" Star)

vectorT :: Type -> Type
vectorT a = TAp (TCon (Tycon "Vector" (Kfun Star Star))) a

tupleT :: [Type] -> Type
tupleT ts = foldl TAp cons ts
  where
    arity = length ts

    kind = foldr1 Kfun $ replicate arity Star
    cons = TCon $ Tycon (mappend "Tuple" (showt arity)) kind


