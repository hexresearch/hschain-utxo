{-# OPTIONS_GHC -Wno-orphans #-}
-- | Functions to construct AST for our language programmatically (not parsed from the code).
-- They are well-typed with usage of phantom type but under the hood they all use type Lang.
module Hschain.Utxo.Lang.Build(
    simpleModule
  , mainExprModule
  , mainScript
  , mainScriptUnsafe
  , bind
  , int
  , text
  , bytes
  , pk
  , pk'
  , SigmaBool
  , toSigma
  , sigmaAnd
  , sigmaOr
  , getHeight
  , getSelf, getInput, getOutput
  , getBoxId, getBoxValue, getBoxScript, getBoxIntArgList, getBoxTextArgList, getBoxBoolArgList, getBoxBytesArgList, getBoxPostHeight
  , getInputs, getOutputs
  , getIntVars, getBoolVars, getTextVars, getBytesVars
  , fromVec, mapVec, foldVec, lengthVec, allVec, anyVec, concatVec, listAt
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
  , showInt
  , showScript
  , sha256
  , serialiseInt
  , serialiseBytes
  , serialiseBool
  , serialiseText
  , lengthBytes
  , trace
  , pair
  , pairAt1
  , pairAt2
  , tuple3
  , tuple4
) where

import Data.Boolean
import Data.ByteString (ByteString)
import Data.Fix
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text   as T
import qualified Data.Vector as V

import Hschain.Utxo.Lang.Compile
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Sigma (PublicKey, publicKeyToText)
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Expr

-- | Creates module  out of single main expression and ignores the errors of compilation.
mainScriptUnsafe :: Expr SigmaBool -> Script
mainScriptUnsafe expr =
  either (error . T.unpack . renderText) id $ mainScript expr

-- | Creates module  out of single main expression.
mainScript :: Expr SigmaBool -> Either Error Script
mainScript expr = toCoreScript $ mainExprModule expr

mainExprModule :: Expr SigmaBool -> Module
mainExprModule expr = simpleModule [bind "main" expr]

simpleModule :: [Bind Lang] -> Module
simpleModule = Module noLoc mempty

bind :: Text -> Expr a -> Bind Lang
bind name (Expr expr) = simpleBind (VarName noLoc name) expr

(=:) :: Text -> Expr a -> (Expr a -> Expr b) -> Expr b
(=:) = def

primExpr :: Prim -> Expr a
primExpr p = Expr $ Fix $ PrimE noLoc p

int :: Int -> Expr Int
int x = primExpr $ PrimInt $ fromIntegral x

text :: Text -> Expr Text
text x = primExpr $ PrimString x

bytes :: ByteString -> Expr ByteString
bytes x = primExpr $ PrimBytes x

mkBool :: Bool -> Expr Bool
mkBool x = primExpr $ PrimBool x

op1 :: (Lang -> E Lang) -> Expr a -> Expr b
op1 f (Expr a) = Expr $ Fix $ f a

op2 :: (Lang -> Lang -> E Lang) -> Expr a -> Expr b -> Expr c
op2 f (Expr a) (Expr b) = Expr $ Fix $ f a b

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

pairAt1 :: Expr (a, b) -> Expr a
pairAt1 (Expr a) = Expr $ Fix $ UnOpE noLoc (TupleAt 2 0) a

pairAt2 :: Expr (a, b) -> Expr b
pairAt2 (Expr a) = Expr $ Fix $ UnOpE noLoc (TupleAt 2 1) a

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
  notB = op1 (UnOpE noLoc Not)
  (&&*) = op2 (BinOpE noLoc And)
  (||*) = op2 (BinOpE noLoc Or)

instance Boolean (Expr SigmaBool) where
  true  = toSigma true
  false = toSigma false
  notB = error "Not is not defined for sigma-expressions"
  (&&*) = sigmaAnd
  (||*) = sigmaOr

pk' :: PublicKey -> Expr SigmaBool
pk' = pk . text . publicKeyToText

pk :: Expr Text -> Expr SigmaBool
pk (Expr key) = Expr $ Fix $ SigmaE noLoc $ Pk noLoc key

sigmaAnd :: Expr SigmaBool -> Expr SigmaBool -> Expr SigmaBool
sigmaAnd (Expr a) (Expr b) = Expr $ Fix $ SigmaE noLoc $ SAnd noLoc a b

sigmaOr :: Expr SigmaBool -> Expr SigmaBool -> Expr SigmaBool
sigmaOr (Expr a) (Expr b) = Expr $ Fix $ SigmaE noLoc $ SOr noLoc a b

toSigma :: Expr Bool -> Expr SigmaBool
toSigma (Expr x) = Expr $ Fix $ SigmaE noLoc $ SPrimBool noLoc x

getSelf :: Expr Box
getSelf = Expr $ Fix $ GetEnv noLoc (Self noLoc)

getInput :: Expr Int -> Expr Box
getInput (Expr n) = Expr $ Fix $ GetEnv noLoc $ Input noLoc n

getOutput :: Expr Int -> Expr Box
getOutput (Expr n) = Expr $ Fix $ GetEnv noLoc $ Output noLoc n

getBoxId :: Expr Box -> Expr Text
getBoxId (Expr box) = Expr $ Fix $ BoxE noLoc $ BoxAt noLoc box BoxFieldId

getBoxValue :: Expr Box -> Expr Int
getBoxValue (Expr box) = Expr $ Fix $ BoxE noLoc $ BoxAt noLoc box BoxFieldValue

getBoxScript :: Expr Box -> Expr ByteString
getBoxScript (Expr box) = Expr $ Fix $ BoxE noLoc $ BoxAt noLoc box BoxFieldScript

getBoxPostHeight :: Expr Box -> Expr Int
getBoxPostHeight (Expr box) = Expr $ Fix $ BoxE noLoc $ BoxAt noLoc box BoxFieldPostHeight

getBoxBytesArgList :: Expr Box -> Expr (Vector ByteString)
getBoxBytesArgList (Expr box) = Expr $ Fix $ BoxE noLoc $ BoxAt noLoc box (BoxFieldArgList BytesArg)

getBoxIntArgList :: Expr Box -> Expr (Vector Int)
getBoxIntArgList (Expr box) = Expr $ Fix $ BoxE noLoc $ BoxAt noLoc box (BoxFieldArgList IntArg)

getBoxTextArgList :: Expr Box -> Expr (Vector Text)
getBoxTextArgList (Expr box) = Expr $ Fix $ BoxE noLoc $ BoxAt noLoc box (BoxFieldArgList TextArg)

getBoxBoolArgList :: Expr Box -> Expr (Vector Bool)
getBoxBoolArgList (Expr box) = Expr $ Fix $ BoxE noLoc $ BoxAt noLoc box (BoxFieldArgList BoolArg)

getHeight :: Expr Int
getHeight = Expr $ Fix $ GetEnv noLoc (Height noLoc)

getIntVars :: Expr (Vector Int)
getIntVars = Expr $ Fix $ GetEnv noLoc $ GetVar noLoc IntArg

getBoolVars :: Expr (Vector Bool)
getBoolVars = Expr $ Fix $ GetEnv noLoc $ GetVar noLoc BoolArg

getTextVars :: Expr (Vector Text)
getTextVars = Expr $ Fix $ GetEnv noLoc $ GetVar noLoc TextArg

getBytesVars :: Expr (Vector ByteString)
getBytesVars = Expr $ Fix $ GetEnv noLoc $ GetVar noLoc BytesArg

getInputs :: Expr (Vector Box)
getInputs = Expr $ Fix $ GetEnv noLoc (Inputs noLoc)

getOutputs :: Expr (Vector Box)
getOutputs = Expr $ Fix $ GetEnv noLoc (Outputs noLoc)

fromVec :: Vector (Expr a) -> Expr (Vector a)
fromVec vs = Expr $ Fix $ VecE noLoc $ NewVec noLoc $ fmap (\(Expr a) -> a) vs

listAt :: Expr (Vector a) -> Expr Int -> Expr a
listAt (Expr vector) (Expr index) = Expr $ Fix $ VecE noLoc $ VecAt noLoc vector index

mapVec :: Expr (a -> b) -> Expr (Vector a) -> Expr (Vector b)
mapVec (Expr f) (Expr v) = Expr $ Fix $ Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecMap noLoc)) f) v

foldVec :: Expr (a -> b -> a) -> Expr a -> Expr (Vector b) -> Expr a
foldVec (Expr f) (Expr z) (Expr v) = Expr $ Fix $ Apply noLoc (Fix $ Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z) v

lengthVec :: Expr (Vector a) -> Expr Int
lengthVec (Expr v) = Expr $ Fix $ Apply noLoc (Fix $ VecE noLoc (VecLength noLoc)) v

concatVec :: Expr (Vector a) -> Expr (Vector a) -> Expr (Vector a)
concatVec (Expr a) (Expr b) = Expr $ Fix $ VecE noLoc $ VecAppend noLoc a b

allVec :: Expr (Vector Bool) -> Expr Bool
allVec (Expr v) = Expr $ Fix $ Apply noLoc (Fix $ Var noLoc "all") v

anyVec :: Expr (Vector Bool) -> Expr Bool
anyVec (Expr v) = Expr $ Fix $ Apply noLoc (Fix $ Var noLoc "any") v

andSigma :: Expr (Vector SigmaBool) -> Expr SigmaBool
andSigma (Expr v) = Expr $ Fix $ Apply noLoc (Fix $ Var noLoc "andSigma") v

orSigma :: Expr (Vector SigmaBool) -> Expr SigmaBool
orSigma (Expr v) = Expr $ Fix $ Apply noLoc (Fix $ Var noLoc "orSigma") v

type instance BooleanOf (Expr a) = Expr Bool

instance IfB (Expr a) where
  ifB (Expr c) (Expr t) (Expr e) = Expr $ Fix $ If noLoc c t e

-------------------------------------------------
-- numeric

instance Num (Expr Int) where
  (+) = op2 (BinOpE noLoc Plus)
  (*) = op2 (BinOpE noLoc Times)
  negate = op1 (UnOpE noLoc Neg)
  fromInteger n = primExpr $ PrimInt $ fromIntegral n
  abs = error "abs is not defined for Expr"
  signum = error "signum is not defined for Expr"

-- equals
--

instance EqB (Expr Int) where
  (==*) = op2 (BinOpE noLoc Equals)
  (/=*) = op2 (BinOpE noLoc NotEquals)

instance EqB (Expr Text) where
  (==*) = op2 (BinOpE noLoc Equals)
  (/=*) = op2 (BinOpE noLoc NotEquals)

instance EqB (Expr ByteString) where
  (==*) = op2 (BinOpE noLoc Equals)
  (/=*) = op2 (BinOpE noLoc NotEquals)

instance EqB (Expr Script) where
  (==*) = op2 (BinOpE noLoc Equals)
  (/=*) = op2 (BinOpE noLoc NotEquals)

-- order

instance OrdB (Expr Int) where
  (<*) = op2 (BinOpE noLoc LessThan)

instance OrdB (Expr Text) where
  (<*) = op2 (BinOpE noLoc LessThan)

instance OrdB (Expr ByteString) where
  (<*) = op2 (BinOpE noLoc LessThan)

-------------------------
-- btc-like signatures

checkSig :: Expr Text -> Expr Int -> Expr Bool
checkSig (Expr a) (Expr b) = Expr $ Fix $ CheckSig noLoc a b

checkMultiSig :: Expr Int -> Expr (Vector Text) -> Expr (Vector Int) -> Expr Bool
checkMultiSig (Expr a) (Expr b) (Expr c) = Expr $ Fix $ CheckMultiSig noLoc a b c

--------------------------
-- text

concatText :: Expr Text -> Expr Text -> Expr Text
concatText (Expr a) (Expr b) = Expr $ Fix $ TextE noLoc $ TextAppend noLoc a b

concatBytes :: Expr ByteString -> Expr ByteString -> Expr ByteString
concatBytes (Expr a) (Expr b) = Expr $ Fix $ BytesE noLoc $ BytesAppend noLoc a b

lengthText :: Expr Text -> Expr Int
lengthText (Expr a) = Expr $ Fix $ Apply noLoc (Fix $ TextE noLoc (TextLength noLoc)) a

showInt :: Expr Int -> Expr Text
showInt (Expr a) = Expr $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText noLoc IntToText)) a

showScript :: Expr Script -> Expr Text
showScript (Expr a) = Expr $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText noLoc ScriptToText)) a

sha256 :: Expr ByteString -> Expr ByteString
sha256 (Expr a) = Expr $ Fix $ BytesE noLoc $ BytesHash noLoc Sha256 a

serialiseInt :: Expr Int -> Expr ByteString
serialiseInt = serialiseBy IntArg

serialiseText :: Expr Text -> Expr ByteString
serialiseText = serialiseBy TextArg

serialiseBytes :: Expr ByteString -> Expr ByteString
serialiseBytes = serialiseBy BytesArg

serialiseBool :: Expr Bool -> Expr ByteString
serialiseBool = serialiseBy BoolArg

serialiseBy :: ArgType -> Expr a -> Expr ByteString
serialiseBy tag (Expr expr) = Expr $ Fix $ BytesE noLoc $ SerialiseToBytes noLoc tag expr

lengthBytes :: Expr ByteString -> Expr Int
lengthBytes (Expr a) = Expr $ Fix $ BytesE noLoc $ BytesLength noLoc a

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

------------------------------------
-- debug

trace :: Expr Text -> Expr a -> Expr a
trace (Expr str) (Expr a) = Expr $ Fix $ Trace noLoc str a
