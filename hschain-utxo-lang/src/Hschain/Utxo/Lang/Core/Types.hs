-- | Common primitive type definitions
module Hschain.Utxo.Lang.Core.Types (
    Name
  , TypeCore(..)
  , boxTuple
  , argsTuple
  , Typed(..)
  , Prim(..)
  , TypeCoreError(..)
  , typeCoreToType
    -- * Lens
  , typed'typeL
  , typed'valueL
  ) where

import Codec.Serialise
import Control.Applicative
import Control.DeepSeq

import Data.Aeson
import Data.Data
import Data.Function (on)
import Data.Int
import Data.String
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)

import Hex.Common.Lens (makeLensesWithL)
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Const (intT, boolT, bytesT, textT, unitT, maybeT
                               , sumT, tupleT, listT, sigmaT, boxT, arrowT)
import HSChain.Crypto.Classes (ViaBase58(..))

import qualified Type.Check.HM as H

-- | Errors for core language type-checker.
data TypeCoreError
  = ExpressionIsBottom                  -- ^ Expression as whole always evaluates to bottom
  | VarIsNotDefined Text                -- ^ Variable is used but not defined
  | ArrowTypeExpected TypeCore          -- ^ Function type expected, but got
  | TypeCoreMismatch  TypeCore TypeCore -- ^ Got type a while expected b
  | EmptyCaseExpression                 -- ^ Case has no alternatives
  | PolymorphicLet                      -- ^ Let is used to bind variable that always evaluate to bottom
  | BadEquality TypeCore                -- ^ Equality used on types that don't support it
  | BadShow     TypeCore                -- ^ Show is used on types that don't support it
  | BadCase
  | BadConstructor
  deriving stock    (Show,Eq,Generic,Data)
  deriving anyclass (NFData)

-- | Type tags for values
data Typed ty a = Typed
  { typed'value :: a
  , typed'type  :: ty
  }
  deriving stock    (Show, Eq, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Serialise)

-- | Name identifiers for variables or global functions
type Name = Text

-- | Primitive types
data Prim
  = PrimInt   !Int64
  | PrimText  !Text
  | PrimBytes !ByteString
  | PrimBool  !Bool
  | PrimSigma !(Sigma ProofInput)
  deriving stock    (Show, Eq, Ord, Generic, Data)
  deriving anyclass (NFData, Serialise)

-- | Data types of core language
data TypeCore
  = IntT                        -- ^ Integer
  | BoolT                       -- ^ Boolean
  | BytesT                      -- ^ Byte sequence
  | TextT                       -- ^ Text
  | SigmaT                      -- ^ Sigma expression
  | TypeCore :-> TypeCore       -- ^ Function type
  | ListT TypeCore              -- ^ List
  | TupleT [TypeCore]           -- ^ Tuple. Nullary tuple doubles as unit
  | SumT [TypeCore]             -- ^ Sum type
  | UnitT                       -- ^ Unit
  | MaybeT TypeCore             -- ^ Maybe
  | BoxT
    -- ^ Box. 5-tuple of box ID, spend script, value of box, arguments and post-height
  deriving stock    (Show, Eq, Ord, Generic,Data)
  deriving anyclass (NFData,Serialise)
infixr 5 :->

boxTuple :: TypeCore
boxTuple = TupleT [BytesT, BytesT, IntT, argsTuple, IntT]

argsTuple :: TypeCore
argsTuple = TupleT [ListT IntT, ListT TextT, ListT BoolT, ListT BytesT]

typeCoreToType :: (H.DefLoc loc, IsString v) => TypeCore -> H.Type loc v
typeCoreToType = \case
  IntT      -> intT
  BoolT     -> boolT
  BytesT    -> bytesT
  TextT     -> textT
  SigmaT    -> sigmaT
  BoxT      -> boxT
  a :-> b   -> (arrowT `on` typeCoreToType) a b
  ListT a   -> listT (typeCoreToType a)
  TupleT xs -> tupleT $ typeCoreToType <$> xs
  UnitT     -> unitT
  MaybeT a  -> maybeT (typeCoreToType a)
  SumT ts   -> sumT (fmap typeCoreToType ts)

-----------------------------------------------------
-- instances

$(makeLensesWithL ''Typed)

----------------------------------------------------------------
-- Pretty-printing
----------------------------------------------------------------

instance Pretty TypeCore where
  pretty = go False
    where
      go needParens = \case
        IntT      -> "Int"
        BoolT     -> "Bool"
        BytesT    -> "Bytes"
        TextT     -> "Text"
        SigmaT    -> "Sigma"
        a :-> b   -> withParens $ go True a <> " -> " <> go False b
        ListT  a  -> brackets $ go False a
        TupleT xs -> parens $ hsep $ punctuate comma $ go False <$> xs
        BoxT      -> "Box"
        UnitT     -> "Unit"
        MaybeT a  -> withParens $ hsep ["Maybe", (go True a)]
        SumT ts   -> withParens $ hsep $ ("Sum" <> pretty (length ts)) : fmap (go True) ts
        where
          withParens = if needParens then parens else id

instance ToJSON Prim where
  toJSON x = object $ pure $ case x of
    PrimInt   n   -> "int"    .= n
    PrimText  txt -> "text"   .= txt
    PrimBool  b   -> "bool"   .= b
    PrimSigma s   -> "sigma"  .= toJSON s
    PrimBytes s   -> "bytes"  .= toJSON (ViaBase58 s)

-- todo: rewrite this instance
-- to distinguish between numeric types of int, double and money
instance FromJSON Prim where
  parseJSON = withObject "prim" $ \v ->
        fmap PrimInt  (v .: "int")
    <|> fmap PrimText (v .: "text")
    <|> fmap (\(ViaBase58 s :: ViaBase58 "Prim" ByteString) -> PrimBytes s) (v .: "bytes")
    <|> fmap PrimBool   (v .: "bool")
    <|> (fmap PrimSigma . parseJSON =<< (v .: "sigma"))
