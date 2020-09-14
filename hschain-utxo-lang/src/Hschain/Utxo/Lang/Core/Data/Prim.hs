{-# OPTIONS_GHC -Wno-orphans #-}
-- | Common primitive type definitions
module Hschain.Utxo.Lang.Core.Data.Prim(
    Name
  , TypeCore(..)
  , SignatureCore
  , Typed(..)
  , Prim(..)
    -- * Lens
  , typed'typeL
  , typed'valueL
  ) where

import Codec.Serialise
import Control.DeepSeq
import Control.Lens

import Data.Fix
import Data.Int
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Language.HM as H
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Sigma

import Language.HM (IsVar, stringIntToVar, stringPrettyLetters)
import Language.HM.Pretty (HasPrefix(..))

type SignatureCore = H.Signature () Name

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
  | PrimSigma !(Sigma PublicKey)
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise)

-- | Data types of core language
data TypeCore
  = IntT                        -- ^ Integer
  | BoolT                       -- ^ Boolean
  | BytesT                      -- ^ Bytes
  | TextT                       -- ^ Text
  | SigmaT                      -- ^ Sigma expression
  | TypeCore :-> TypeCore       -- ^ Function type
  | ListT TypeCore              -- ^ List
  | TupleT [TypeCore]           -- ^ Tuple. Nullary tuple doubles as unit
  | ArgsT
  | BoxT
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)
infixr 5 :->


-----------------------------------------------------
-- instnaces

instance IsVar Name where
  intToVar = stringIntToVar
  prettyLetters = stringPrettyLetters

instance HasPrefix Name where
  getFixity = const Nothing

instance Serialise (Fix (H.TypeF () Text))
instance (Serialise loc, Serialise var, Serialise a) => Serialise (H.TypeF loc var a)
instance Serialise TypeCore

$(makeLensesWith
   (defaultFieldRules & lensField .~ (mappingNamer (\nm -> [nm++"L"])))
   ''Typed)




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
        a :-> b   -> (if needParens then parens else id)
                   $ go True a <> " -> " <> go False b
        ListT  a  -> brackets $ go False a
        TupleT xs -> parens $ hsep $ punctuate comma $ go False <$> xs
        ArgsT     -> "Args"
        BoxT      -> "Box"
