module Hschain.Utxo.Lang.Build(
    int
  , text
  , pk
  , pk'
  , getHeight
  , getSelf, getInput, getOutput
  , getBoxId, getBoxValue, getBoxScript, getBoxArg
  , getInputs, getOutputs
  , getVar
  , fromVec, mapVec, foldVec, lengthVec, allVec, anyVec, concatVec
  , var
  , def
  , (=:)
  , lam
  , lam2
  , app
  , toScriptBytes
  , concatText
  , lengthText
  , showInt
  , showScript
  , sha256
  , blake2b256
  , trace
) where

import Data.Boolean
import Data.Fix
import Data.Fixed
import Data.Int
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (toScript)

import Hschain.Utxo.Lang.Expr
import qualified Hschain.Utxo.Lang.Sigma as S

(=:) :: Text -> Expr a -> (Expr a -> Expr b) -> Expr b
(=:) = def

primExpr :: Prim -> Expr a
primExpr p = Expr $ Fix $ PrimE Nothing p

int :: Int64 -> Expr Int64
int x = primExpr $ PrimInt x

text :: Text -> Expr Text
text x = primExpr $ PrimString x

mkBool :: Bool -> Expr Bool
mkBool x = primExpr $ PrimBool x

op1 :: (Lang -> E Lang) -> Expr a -> Expr b
op1 f (Expr a) = Expr $ Fix $ f a

op2 :: (Lang -> Lang -> E Lang) -> Expr a -> Expr b -> Expr c
op2 f (Expr a) (Expr b) = Expr $ Fix $ f a b

-- variables

var :: Text -> Expr a
var name = Expr $ Fix $ Var Nothing $ VarName Nothing name

def :: Text -> Expr a -> (Expr a -> Expr b) -> Expr b
def name (Expr a) bodyFun =
  case (bodyFun (var name)) of
    Expr body -> Expr $ singleLet Nothing (VarName Nothing name) a body

lam :: Text -> (Expr a -> Expr b) -> Expr (a -> b)
lam name bodyFun =
  case bodyFun (var name) of
    Expr body -> Expr $ Fix $ Lam Nothing (VarName Nothing name) body

lam2 :: Text -> Text -> (Expr a -> Expr b -> Expr c) -> Expr (a -> b -> c)
lam2 v1 v2  bodyFun =
  case bodyFun (var v1) (var v2) of
    Expr body -> Expr $ Fix $ LamList Nothing [VarName Nothing v1, VarName Nothing v2] body

app :: Expr (a -> b) -> Expr a -> Expr b
app (Expr fun) (Expr arg) = Expr $ Fix $ Apply Nothing fun arg

pair :: Expr a -> Expr b -> Expr (a, b)
pair (Expr a) (Expr b) = Expr $ Fix $ Tuple Nothing $ V.fromList [a, b]

pairAt1 :: Expr (a, b) -> Expr a
pairAt1 (Expr a) = Expr $ Fix $ UnOpE Nothing (TupleAt 2 0) a

pairAt2 :: Expr (a, b) -> Expr b
pairAt2 (Expr a) = Expr $ Fix $ UnOpE Nothing (TupleAt 2 1) a

tuple3 :: Expr a -> Expr b -> Expr c -> Expr (a, b, c)
tuple3 (Expr a) (Expr b) (Expr c) = Expr $ Fix $ Tuple Nothing $ V.fromList [a, b, c]

tuple4 :: Expr a -> Expr b -> Expr c -> Expr d -> Expr (a, b, c, d)
tuple4 (Expr a) (Expr b) (Expr c) (Expr d) = Expr $ Fix $ Tuple Nothing $ V.fromList [a, b, c, d]

----------------------------------------------
-- string
--

instance IsString (Expr Text) where
  fromString = text . fromString

----------------------------------------------
-- boolean

class PrimTy a where

instance PrimTy Double
instance PrimTy Int64
instance PrimTy Bool
instance PrimTy Text

instance Boolean (Expr Bool) where
  true = mkBool True
  false = mkBool False
  notB = op1 (UnOpE Nothing Not)
  (&&*) = op2 (BinOpE Nothing And)
  (||*) = op2 (BinOpE Nothing Or)

pk' :: PublicKey -> Expr Bool
pk' = pk . text . publicKeyToText

pk :: Expr Text -> Expr Bool
pk (Expr key) = Expr $ Fix $ Pk Nothing key

getSelf :: Expr Box
getSelf = Expr $ Fix $ GetEnv Nothing (Self Nothing)

getInput :: Expr Int64 -> Expr Box
getInput (Expr n) = Expr $ Fix $ GetEnv Nothing $ Input Nothing n

getOutput :: Expr Int64 -> Expr Box
getOutput (Expr n) = Expr $ Fix $ GetEnv Nothing $ Output Nothing n

getBoxId :: Expr Box -> Expr Text
getBoxId (Expr box) = Expr $ Fix $ BoxE Nothing $ BoxAt Nothing box BoxFieldId

getBoxValue :: Expr Box -> Expr Money
getBoxValue (Expr box) = Expr $ Fix $ BoxE Nothing $ BoxAt Nothing box BoxFieldValue

getBoxScript :: Expr Box -> Expr Script
getBoxScript (Expr box) = Expr $ Fix $ BoxE Nothing $ BoxAt Nothing box BoxFieldScript

getBoxArg :: Expr Box -> Expr Text -> Expr a
getBoxArg (Expr box) (Expr field) = Expr $ Fix $ BoxE Nothing $ BoxAt Nothing box (BoxFieldArg field)

getHeight :: Expr Int64
getHeight = Expr $ Fix $ GetEnv Nothing (Height Nothing)

getVar :: Expr Text -> Expr a
getVar (Expr arg) = Expr $ Fix $ GetEnv Nothing $ GetVar Nothing arg

toScriptBytes :: Expr Bool -> Expr Script
toScriptBytes expr = unsafeCoerceExpr $ text $ unScript $ toScript expr

unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr (Expr a) = Expr a

getInputs :: Expr (Vector Box)
getInputs = Expr $ Fix $ GetEnv Nothing (Inputs Nothing)

getOutputs :: Expr (Vector Box)
getOutputs = Expr $ Fix $ GetEnv Nothing (Outputs Nothing)

fromVec :: Vector (Expr a) -> Expr (Vector a)
fromVec vs = Expr $ Fix $ VecE Nothing $ NewVec Nothing $ fmap (\(Expr a) -> a) vs

mapVec :: Expr (a -> b) -> Expr (Vector a) -> Expr (Vector b)
mapVec (Expr f) (Expr v) = Expr $ Fix $ Apply Nothing (Fix $ Apply Nothing (Fix $ VecE Nothing (VecMap Nothing)) f) v

foldVec :: Expr (a -> b -> a) -> Expr a -> Expr (Vector b) -> Expr a
foldVec (Expr f) (Expr z) (Expr v) = Expr $ Fix $ Apply Nothing (Fix $ Apply Nothing (Fix $ Apply Nothing (Fix $ VecE Nothing (VecFold Nothing)) f) z) v

lengthVec :: Expr (Vector a) -> Expr Int64
lengthVec (Expr v) = Expr $ Fix $ Apply Nothing (Fix $ VecE Nothing (VecLength Nothing)) v

concatVec :: Expr (Vector a) -> Expr (Vector a) -> Expr (Vector a)
concatVec (Expr a) (Expr b) = Expr $ Fix $ VecE Nothing $ VecAppend Nothing a b

allVec :: Expr (Vector Bool) -> Expr Bool
allVec (Expr v) = Expr $ Fix $ Apply Nothing (Fix $ Var Nothing "all") v

anyVec :: Expr (Vector Bool) -> Expr Bool
anyVec (Expr v) = Expr $ Fix $ Apply Nothing (Fix $ Var Nothing "any") v

type instance BooleanOf (Expr Bool) = Expr Bool
type instance BooleanOf (Expr Int64) = Expr Bool
type instance BooleanOf (Expr Text) = Expr Bool
type instance BooleanOf (Expr Script) = Expr Bool
type instance BooleanOf (Expr (a, b)) = Expr Bool
type instance BooleanOf (Expr (a, b, c)) = Expr Bool

instance IfB (Expr Int64) where
  ifB = ifExpr

instance IfB (Expr Bool) where
  ifB = ifExpr

instance IfB (Expr Text) where
  ifB = ifExpr

instance IfB (Expr Script) where
  ifB = ifExpr

instance IfB (Expr (a, b)) where
  ifB = ifExpr

instance IfB (Expr (a, b, c)) where
  ifB = ifExpr

ifExpr :: Expr Bool -> Expr a -> Expr a -> Expr a
ifExpr (Expr cond) (Expr t) (Expr e) =  Expr $ ifExprLang cond t e

ifExprLang :: Lang -> Lang -> Lang -> Lang
ifExprLang cond t e = Fix $ If Nothing cond t e

-------------------------------------------------
-- numeric

instance Num (Expr Int64) where
  (+) = op2 (BinOpE Nothing Plus)
  (*) = op2 (BinOpE Nothing Times)
  negate = op1 (UnOpE Nothing Neg)
  fromInteger n = primExpr $ PrimInt $ fromIntegral n
  abs = error "abs is not defined for Expr"
  signum = error "signum is not defined for Expr"

-- equals
--

instance EqB (Expr Int64) where
  (==*) = op2 (BinOpE Nothing Equals)
  (/=*) = op2 (BinOpE Nothing NotEquals)

instance EqB (Expr Text) where
  (==*) = op2 (BinOpE Nothing Equals)
  (/=*) = op2 (BinOpE Nothing NotEquals)

instance EqB (Expr Script) where
  (==*) = op2 (BinOpE Nothing Equals)
  (/=*) = op2 (BinOpE Nothing NotEquals)

-- order

instance OrdB (Expr Int64) where
  (<*) = op2 (BinOpE Nothing LessThan)

instance OrdB (Expr Text) where
  (<*) = op2 (BinOpE Nothing LessThan)

--------------------------
-- text

concatText :: Expr Text -> Expr Text -> Expr Text
concatText (Expr a) (Expr b) = Expr $ Fix $ TextE Nothing $ TextAppend Nothing a b

lengthText :: Expr Text -> Expr Int64
lengthText (Expr a) = Expr $ Fix $ Apply Nothing (Fix $ TextE Nothing (TextLength Nothing)) a

showInt :: Expr Int64 -> Expr Text
showInt (Expr a) = Expr $ Fix $ Apply Nothing (Fix $ TextE Nothing (ConvertToText IntToText Nothing)) a

showScript :: Expr Script -> Expr Text
showScript (Expr a) = Expr $ Fix $ Apply Nothing (Fix $ TextE Nothing (ConvertToText ScriptToText Nothing)) a

sha256 :: Expr Text -> Expr Text
sha256 (Expr a) = Expr $ Fix $ Apply Nothing (Fix $ TextE Nothing $ TextHash Nothing Sha256) a

blake2b256 :: Expr Text -> Expr Text
blake2b256 (Expr a) = Expr $ Fix $ Apply Nothing (Fix $ TextE Nothing $ TextHash Nothing Blake2b256) a

-------------------------------
-- monoids

instance Semigroup (Expr Text) where
  (<>) = concatText

instance Monoid (Expr Text) where
  mempty = ""

instance Semigroup (Expr (Vector a)) where
  (<>) = concatVec

instance Monoid (Expr (Vector a)) where
  mempty = fromVec mempty

------------------------------------
-- debug

trace :: Expr Text -> Expr a -> Expr a
trace (Expr str) (Expr a) = Expr $ Fix $ Trace Nothing str a


