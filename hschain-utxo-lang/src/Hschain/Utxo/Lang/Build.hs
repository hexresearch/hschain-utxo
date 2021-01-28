{-# OPTIONS_GHC -Wno-orphans #-}
-- | Functions to construct AST for our language programmatically (not parsed from the code).
-- They are well-typed with usage of phantom type but under the hood they all use type Lang.
module Hschain.Utxo.Lang.Build(
    Expr(..)
  , SigmaBool
  , simpleModule
  , mainExprModule
  , mainScript
  , mainScriptUnsafe
  , bind
  , int
  , text
  , bytes
  , pk
  , pk'
  , toSigma
  , getHeight
  , getSelf, getInput, getOutput
  , getBoxId, getBoxValue, getBoxScript, getBoxArgs, getBoxPostHeight
  , getInputs, getOutputs, getDataInputs
  , getArgs
  , fromVec, mapVec, foldlVec, lengthVec, andVec, orVec, concatVec, listAt
  , andSigma, orSigma
  , checkSig
  , checkMultiSig
  , var
  , def
  , (=:)
  , lam
  , lam2
  , app
  , concatText
  , lengthText
  , showExpr
  , sha256
  , serialiseFrom
  , lengthBytes
  , pair
  , tuple3
  , tuple4
  , module Data.Boolean
) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang.Compile
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Module
import Hschain.Utxo.Lang.Core.Types (Prim(..))
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Utils.ByteString

import qualified Data.Text   as T
import Data.Boolean
import Data.ByteString (ByteString)
import Data.Fix
import Data.String
import Data.Vector (Vector)

import qualified Data.Vector as V

import HSChain.Crypto (ByteRepr(..))
import Hschain.Utxo.Lang.Sigma (PublicKey)

import qualified Hschain.Utxo.Lang.Const as Const

-- | Type for expression of our language that has type.
--
-- This is phantom type for covenience of type-checker.
newtype Expr a = Expr Lang

-- | Type tag for type-safe construction
data SigmaBool

-- | Creates module  out of single main expression and ignores the errors of compilation.
mainScriptUnsafe :: Expr SigmaBool -> Script
mainScriptUnsafe expr =
  either (error . T.unpack . renderText) id $ mainScript expr

-- | Creates module  out of single main expression.
mainScript :: Expr SigmaBool -> Either Error Script
mainScript expr = toCoreScript $ mainExprModule expr

mainExprModule :: Expr SigmaBool -> Module
mainExprModule expr = simpleModule (bind "main" expr)

simpleModule :: Binds Lang -> Module
simpleModule = Module noLoc mempty

bind :: Text -> Expr a -> Binds Lang
bind name (Expr expr) = simpleBind (VarName noLoc name) expr

(=:) :: Text -> Expr a -> (Expr a -> Expr b) -> Expr b
(=:) = def

primExpr :: Prim -> Expr a
primExpr p = Expr $ Fix $ PrimE noLoc p

int :: Int -> Expr Int
int x = primExpr $ PrimInt $ fromIntegral x

text :: Text -> Expr Text
text x = primExpr $ PrimText x

bytes :: ByteString -> Expr ByteString
bytes x = primExpr $ PrimBytes x

mkBool :: Bool -> Expr Bool
mkBool x = primExpr $ PrimBool x

primAp1 :: Text -> Expr a -> Expr b
primAp1 = primOp1

primAp2 :: Text -> Expr a -> Expr b -> Expr c
primAp2 name a (Expr b) = case primAp1 name a of
  Expr f -> Expr $ Fix $ Apply noLoc f b

primAp3 :: Text -> Expr a -> Expr b -> Expr c -> Expr d
primAp3 name a b (Expr c) = case primAp2 name a b of
  Expr f -> Expr $ Fix $ Apply noLoc f c

primVar :: Text -> Expr a
primVar name = Expr $ Fix $ Var noLoc $ VarName noLoc name

primOp1 :: Text -> Expr a -> Expr b
primOp1 name (Expr a) = Expr $ Fix $ Apply noLoc (Fix $ Var noLoc (VarName noLoc name)) a

primOp2 :: Text -> Expr a -> Expr b -> Expr c
primOp2 name (Expr a) (Expr b) = Expr $ Fix $ InfixApply noLoc a (VarName noLoc name) b

-- variables

var :: Text -> Expr a
var name = Expr $ Fix $ Var noLoc $ VarName noLoc name

def :: Text -> Expr a -> (Expr a -> Expr b) -> Expr b
def name (Expr a) bodyFun =
  case (bodyFun (var name)) of
    Expr body -> Expr $ singleLet noLoc (VarName noLoc name) a body

lam :: Text -> (Expr a -> Expr b) -> Expr (a -> b)
lam name bodyFun =
  case bodyFun (var name) of
    Expr body -> Expr $ Fix $ Lam noLoc (PVar noLoc $ VarName noLoc name) body

lam2 :: Text -> Text -> (Expr a -> Expr b -> Expr c) -> Expr (a -> b -> c)
lam2 v1 v2  bodyFun =
  case bodyFun (var v1) (var v2) of
    Expr body -> Expr $ Fix $ LamList noLoc [PVar noLoc $ VarName noLoc v1, PVar noLoc $ VarName noLoc v2] body

app :: Expr (a -> b) -> Expr a -> Expr b
app (Expr fun) (Expr arg) = Expr $ Fix $ Apply noLoc fun arg

pair :: Expr a -> Expr b -> Expr (a, b)
pair (Expr a) (Expr b) = Expr $ Fix $ Tuple noLoc $ V.fromList [a, b]

tuple3 :: Expr a -> Expr b -> Expr c -> Expr (a, b, c)
tuple3 (Expr a) (Expr b) (Expr c) = Expr $ Fix $ Tuple noLoc $ V.fromList [a, b, c]

tuple4 :: Expr a -> Expr b -> Expr c -> Expr d -> Expr (a, b, c, d)
tuple4 (Expr a) (Expr b) (Expr c) (Expr d) = Expr $ Fix $ Tuple noLoc $ V.fromList [a, b, c, d]

----------------------------------------------
-- string
--

instance IsString (Expr Text) where
  fromString = text . fromString

----------------------------------------------
-- boolean

instance Boolean (Expr Bool) where
  true = mkBool True
  false = mkBool False
  notB = primOp1 "not"
  (&&*) = primOp2 "&&"
  (||*) = primOp2 "||"

instance Boolean (Expr SigmaBool) where
  true  = toSigma true
  false = toSigma false
  notB = error "Not is not defined for sigma-expressions"
  (&&*) = primOp2 Const.sigmaAnd
  (||*) = primOp2 Const.sigmaOr

pk' :: PublicKey -> Expr SigmaBool
pk' = pk . bytes . encodeToBS

pk :: Expr ByteString -> Expr SigmaBool
pk = primAp1 Const.pk

toSigma :: Expr Bool -> Expr SigmaBool
toSigma = primAp1 Const.toSigma

getSelf :: Expr Box
getSelf = primVar Const.getSelf

getInput :: Expr Int -> Expr Box
getInput = primAp1 Const.getInput

getOutput :: Expr Int -> Expr Box
getOutput = primAp1 Const.getOutput

getBoxId :: Expr Box -> Expr Text
getBoxId = primAp1 Const.getBoxId

getBoxValue :: Expr Box -> Expr Int
getBoxValue = primAp1 Const.getBoxValue

getBoxScript :: Expr Box -> Expr ByteString
getBoxScript = primAp1 Const.getBoxScript

getBoxPostHeight :: Expr Box -> Expr Int
getBoxPostHeight = primAp1 Const.getBoxPostHeight

getBoxArgs :: IsTerm a => Expr Box -> Expr a
getBoxArgs = primAp1 $ Const.getBoxArgs

getHeight :: Expr Int
getHeight = primVar Const.getHeight

getArgs :: IsTerm a => Expr a
getArgs = primVar $ Const.getArgs

getInputs :: Expr (Vector Box)
getInputs = primVar Const.getInputs

getOutputs :: Expr (Vector Box)
getOutputs = primVar Const.getOutputs

getDataInputs :: Expr (Vector Box)
getDataInputs = primVar Const.getDataInputs

fromVec :: Vector (Expr a) -> Expr (Vector a)
fromVec vs = Expr $ Fix $ List noLoc $ fmap (\(Expr a) -> a) vs

listAt :: Expr (Vector a) -> Expr Int -> Expr a
listAt = primAp2 Const.listAt

mapVec :: Expr (a -> b) -> Expr (Vector a) -> Expr (Vector b)
mapVec = primAp2 Const.map

foldlVec :: Expr (a -> b -> a) -> Expr a -> Expr (Vector b) -> Expr a
foldlVec = primAp3 Const.foldl

lengthVec :: Expr (Vector a) -> Expr Int
lengthVec = primAp1 Const.length

concatVec :: Expr (Vector a) -> Expr (Vector a) -> Expr (Vector a)
concatVec = primOp2 Const.appendList

andVec :: Expr (Vector Bool) -> Expr Bool
andVec = primAp1 Const.and

orVec :: Expr (Vector Bool) -> Expr Bool
orVec = primAp1 Const.or

andSigma :: Expr (Vector SigmaBool) -> Expr SigmaBool
andSigma = primAp1 Const.andSigma

orSigma :: Expr (Vector SigmaBool) -> Expr SigmaBool
orSigma = primAp1 Const.orSigma

type instance BooleanOf (Expr a) = Expr Bool

instance IfB (Expr a) where
  ifB (Expr c) (Expr t) (Expr e) = Expr $ Fix $ If noLoc c t e

-------------------------------------------------
-- numeric

instance Num (Expr Int) where
  (+) = primOp2 "+"
  (*) = primOp2 "*"
  negate = primAp1 "negate"
  fromInteger n = primExpr $ PrimInt $ fromIntegral n
  abs = error "abs is not defined for Expr"
  signum = error "signum is not defined for Expr"

-- equals
--

instance EqB (Expr Int) where
  (==*) = primOp2 Const.equals
  (/=*) = primOp2 Const.nonEquals

instance EqB (Expr Text) where
  (==*) = primOp2 Const.equals
  (/=*) = primOp2 Const.nonEquals

instance EqB (Expr ByteString) where
  (==*) = primOp2 Const.equals
  (/=*) = primOp2 Const.nonEquals

instance EqB (Expr Script) where
  (==*) = primOp2 Const.equals
  (/=*) = primOp2 Const.nonEquals

instance EqB (Expr Bool) where
  (==*) = primOp2 Const.equals
  (/=*) = primOp2 Const.nonEquals

-- order

instance OrdB (Expr Int) where
  (<*) = primOp2 Const.less

instance OrdB (Expr Text) where
  (<*) = primOp2 Const.less

instance OrdB (Expr Bool) where
  (<*) = primOp2 Const.less

instance OrdB (Expr ByteString) where
  (<*) = primOp2 Const.less

instance OrdB (Expr Script) where
  (<*) = primOp2 Const.less

-------------------------
-- btc-like signatures

checkSig :: Expr ByteString -> Expr Int -> Expr Bool
checkSig = primAp2 Const.checkSig

checkMultiSig :: Expr Int -> Expr (Vector ByteString) -> Expr (Vector Int) -> Expr Bool
checkMultiSig = primAp3 Const.checkMultiSig

--------------------------
-- text

concatText :: Expr Text -> Expr Text -> Expr Text
concatText = primOp2 Const.appendText

concatBytes :: Expr ByteString -> Expr ByteString -> Expr ByteString
concatBytes = primAp2 Const.appendBytes

lengthText :: Expr Text -> Expr Int
lengthText = primAp1 Const.lengthText

showExpr :: Expr a -> Expr Text
showExpr = primAp1 Const.show

sha256 :: Expr ByteString -> Expr ByteString
sha256 = primAp1 Const.sha256

serialiseFrom :: Expr a -> Expr ByteString
serialiseFrom = primAp1 $ Const.serialiseBytes

lengthBytes :: Expr ByteString -> Expr Int
lengthBytes = primAp1 Const.lengthBytes

-------------------------------
-- monoids

instance Semigroup (Expr Text) where
  (<>) = concatText

instance Semigroup (Expr ByteString) where
  (<>) = concatBytes

instance Monoid (Expr Text) where
  mempty = ""

instance Semigroup (Expr (Vector a)) where
  (<>) = concatVec

instance Monoid (Expr (Vector a)) where
  mempty = fromVec mempty

instance Pretty (Expr a) where
  pretty (Expr a) = pretty a

deriving stock   instance Show (Expr a)
deriving stock   instance Eq   (Expr a)
