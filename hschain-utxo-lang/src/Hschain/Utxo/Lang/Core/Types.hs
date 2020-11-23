-- | Common primitive type definitions
module Hschain.Utxo.Lang.Core.Types (
    Name
  , TypeCore(..)
  , argsTuple
  , Typed(..)
  , Prim(..)
    -- * Lens
  , typed'typeL
  , typed'valueL
  ) where

import Codec.Serialise
import Control.DeepSeq

import Data.Data
import Data.Int
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)

import Hex.Common.Lens (makeLensesWithL)
import Hschain.Utxo.Lang.Sigma


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
  | BytesT                      -- ^ Byte sequence
  | TextT                       -- ^ Text
  | SigmaT                      -- ^ Sigma expression
  | TypeCore :-> TypeCore       -- ^ Function type
  | ListT TypeCore              -- ^ List
  | TupleT [TypeCore]           -- ^ Tuple. Nullary tuple doubles as unit
  | BoxT
    -- ^ Box. 4-tuple of box ID, spend script, value of box, and arguments
  deriving stock    (Show, Eq, Generic,Data)
  deriving anyclass (NFData,Serialise)
infixr 5 :->

argsTuple :: TypeCore
argsTuple = TupleT [ListT IntT, ListT TextT, ListT BoolT, ListT BytesT]

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
        a :-> b   -> (if needParens then parens else id)
                   $ go True a <> " -> " <> go False b
        ListT  a  -> brackets $ go False a
        TupleT xs -> parens $ hsep $ punctuate comma $ go False <$> xs
        BoxT      -> "Box"
