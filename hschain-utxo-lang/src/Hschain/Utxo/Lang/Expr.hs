module Hschain.Utxo.Lang.Expr where

import Control.Applicative

import Data.Aeson

import Data.Fix
import Data.Fixed
import Data.Functor.Classes

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import Text.Show.Deriving

newtype Expr a = Expr Lang
  deriving (Show, Read, Eq)

type VarName = Text

type Type = Fix TypeExpr

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
  | Lam VarName Type a
  | LamList [(VarName, Type)] a
  | Let VarName a a
  | LetArg VarName [VarName] a a
  | LetRec VarName Type a a
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
  | GetEnv (Id a)
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

data Id a
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
$(deriveShow1 ''E)
$(deriveShow1 ''Id)
$(deriveShow1 ''BoxField)
$(deriveShow1 ''TextExpr)
$(deriveShow1 ''VecExpr)
$(deriveShow1 ''BoxExpr)

