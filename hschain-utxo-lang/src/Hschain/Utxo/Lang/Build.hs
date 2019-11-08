module Hschain.Utxo.Lang.Build(
    int
  , double
  , text
  , money
  , pk
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
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (toScript)

import Hschain.Utxo.Lang.Expr
import qualified Hschain.Utxo.Lang.Sigma as S

(=:) :: Text -> Expr a -> (Expr a -> Expr b) -> Expr b
(=:) = def

primExpr :: Prim -> Expr a
primExpr p = Expr $ Fix $ PrimE p

int :: Int -> Expr Int
int x = primExpr $ PrimInt x

double :: Double -> Expr Double
double x = primExpr $ PrimDouble x

text :: Text -> Expr Text
text x = primExpr $ PrimString x

money :: Pico -> Expr Money
money x = primExpr $ PrimMoney x

mkBool :: Bool -> Expr Bool
mkBool x = primExpr $ PrimBool x

op1 :: (Lang -> E Lang) -> Expr a -> Expr b
op1 f (Expr a) = Expr $ Fix $ f a

op2 :: (Lang -> Lang -> E Lang) -> Expr a -> Expr b -> Expr c
op2 f (Expr a) (Expr b) = Expr $ Fix $ f a b

-- variables

var :: Text -> Expr a
var name = Expr $ Fix $ Var name

def :: VarName -> Expr a -> (Expr a -> Expr b) -> Expr b
def name (Expr a) bodyFun =
  case (bodyFun (var name)) of
    Expr body -> Expr $ Fix $ Let name a body

lam :: VarName -> (Expr a -> Expr b) -> Expr (a -> b)
lam name bodyFun =
  case bodyFun (var name) of
    Expr body -> Expr $ Fix $ Lam name (Fix UknownType) body

lam2 :: VarName -> VarName -> (Expr a -> Expr b -> Expr c) -> Expr (a -> b -> c)
lam2 v1 v2  bodyFun =
  case bodyFun (var v1) (var v2) of
    Expr body -> Expr $ Fix $ LamList [(v1, Fix UknownType), (v2, Fix UknownType)] body

app :: Expr (a -> b) -> Expr a -> Expr b
app (Expr fun) (Expr arg) = Expr $ Fix $ App fun arg

pair :: Expr a -> Expr b -> Expr (a, b)
pair (Expr a) (Expr b) = Expr $ Fix $ Tuple $ V.fromList [a, b]

pairAt1 :: Expr (a, b) -> Expr a
pairAt1 (Expr a) = Expr $ Fix $ UnOpE (TupleAt 0) a

pairAt2 :: Expr (a, b) -> Expr b
pairAt2 (Expr a) = Expr $ Fix $ UnOpE (TupleAt 1) a

tuple3 :: Expr a -> Expr b -> Expr c -> Expr (a, b, c)
tuple3 (Expr a) (Expr b) (Expr c) = Expr $ Fix $ Tuple $ V.fromList [a, b, c]

tuple4 :: Expr a -> Expr b -> Expr c -> Expr d -> Expr (a, b, c, d)
tuple4 (Expr a) (Expr b) (Expr c) (Expr d) = Expr $ Fix $ Tuple $ V.fromList [a, b, c, d]

----------------------------------------------
-- string
--

instance IsString (Expr Text) where
  fromString = text . fromString

----------------------------------------------
-- boolean

class PrimTy a where

instance PrimTy Double
instance PrimTy Int
instance PrimTy Money
instance PrimTy Bool
instance PrimTy Text

instance Boolean (Expr Bool) where
  true = mkBool True
  false = mkBool False
  notB = op1 (UnOpE Not)
  (&&*) = op2 (BinOpE And)
  (||*) = op2 (BinOpE Or)

pk :: Expr PubKey -> Expr Bool
pk (Expr key) = Expr $ Fix $ Pk key

getSelf :: Expr Box
getSelf = Expr $ Fix $ GetEnv Self

getInput :: Expr Int -> Expr Box
getInput (Expr n) = Expr $ Fix $ GetEnv $ Input n

getOutput :: Expr Int -> Expr Box
getOutput (Expr n) = Expr $ Fix $ GetEnv $ Output n

getBoxId :: Expr Box -> Expr Text
getBoxId (Expr box) = Expr $ Fix $ BoxE $ BoxAt box BoxFieldId

getBoxValue :: Expr Box -> Expr Money
getBoxValue (Expr box) = Expr $ Fix $ BoxE $ BoxAt box BoxFieldValue

getBoxScript :: Expr Box -> Expr Script
getBoxScript (Expr box) = Expr $ Fix $ BoxE $ BoxAt box BoxFieldScript

getBoxArg :: Expr Box -> Expr Text -> Expr a
getBoxArg (Expr box) (Expr field) = Expr $ Fix $ BoxE $ BoxAt box (BoxFieldArg field)

getHeight :: Expr Int
getHeight = Expr $ Fix $ GetEnv Height

getVar :: Expr Text -> Expr a
getVar (Expr arg) = Expr $ Fix $ GetEnv $ GetVar arg

toScriptBytes :: Expr Bool -> Expr Script
toScriptBytes expr = unsafeCoerceExpr $ text $ unScript $ toScript expr

unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr (Expr a) = Expr a

getInputs :: Expr (Vector Box)
getInputs = Expr $ Fix $ GetEnv Inputs

getOutputs :: Expr (Vector Box)
getOutputs = Expr $ Fix $ GetEnv Outputs

fromVec :: Vector (Expr a) -> Expr (Vector a)
fromVec vs = Expr $ Fix $ VecE $ NewVec $ fmap (\(Expr a) -> a) vs

mapVec :: Expr (a -> b) -> Expr (Vector a) -> Expr (Vector b)
mapVec (Expr f) (Expr v) = Expr $ Fix $ App (Fix $ App (Fix $ VecE VecMap) f) v

foldVec :: Expr (a -> b -> a) -> Expr a -> Expr (Vector b) -> Expr a
foldVec (Expr f) (Expr z) (Expr v) = Expr $ Fix $ App (Fix $ App (Fix $ App (Fix $ VecE VecFold) f) z) v

lengthVec :: Expr (Vector a) -> Expr Int
lengthVec (Expr v) = Expr $ Fix $ App (Fix $ VecE VecLength) v

concatVec :: Expr (Vector a) -> Expr (Vector a) -> Expr (Vector a)
concatVec (Expr a) (Expr b) = Expr $ Fix $ VecE $ VecAppend a b

allVec :: Expr (Vector Bool) -> Expr Bool
allVec (Expr v) = Expr $ Fix $ App (Fix $ Var "all") v

anyVec :: Expr (Vector Bool) -> Expr Bool
anyVec (Expr v) = Expr $ Fix $ App (Fix $ Var "any") v

type instance BooleanOf (Expr Bool) = Expr Bool
type instance BooleanOf (Expr Int) = Expr Bool
type instance BooleanOf (Expr Money) = Expr Bool
type instance BooleanOf (Expr Double) = Expr Bool
type instance BooleanOf (Expr Text) = Expr Bool
type instance BooleanOf (Expr Script) = Expr Bool
type instance BooleanOf (Expr (a, b)) = Expr Bool
type instance BooleanOf (Expr (a, b, c)) = Expr Bool

instance IfB (Expr Int) where
  ifB = ifExpr

instance IfB (Expr Bool) where
  ifB = ifExpr

instance IfB (Expr Money) where
  ifB = ifExpr

instance IfB (Expr Double) where
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
ifExprLang cond t e = Fix $ If cond t e

-------------------------------------------------
-- numeric

instance Num (Expr Int) where
  (+) = op2 (BinOpE Plus)
  (*) = op2 (BinOpE Times)
  negate = op1 (UnOpE Neg)
  fromInteger n = primExpr $ PrimInt $ fromIntegral n
  abs = error "abs is not defined for Expr"
  signum = error "signum is not defined for Expr"

instance Num (Expr Double) where
  (+) = op2 (BinOpE Plus)
  (*) = op2 (BinOpE Times)
  negate = op1 (UnOpE Neg)
  fromInteger n = primExpr $ PrimDouble $ fromIntegral n
  abs = error "abs is not defined for Expr"
  signum = error "signum is not defined for Expr"

instance Num (Expr Money) where
  (+) = op2 (BinOpE Plus)
  (*) = op2 (BinOpE Times)
  negate = op1 (UnOpE Neg)
  fromInteger n = primExpr $ PrimMoney $ fromIntegral n
  abs = error "abs is not defined for Expr"
  signum = error "signum is not defined for Expr"

-- equals
--

instance EqB (Expr Int) where
  (==*) = op2 (BinOpE Equals)
  (/=*) = op2 (BinOpE NotEquals)

instance EqB (Expr Money) where
  (==*) = op2 (BinOpE Equals)
  (/=*) = op2 (BinOpE NotEquals)

instance EqB (Expr Text) where
  (==*) = op2 (BinOpE Equals)
  (/=*) = op2 (BinOpE NotEquals)

instance EqB (Expr Double) where
  (==*) = op2 (BinOpE Equals)
  (/=*) = op2 (BinOpE NotEquals)

instance EqB (Expr Script) where
  (==*) = op2 (BinOpE Equals)
  (/=*) = op2 (BinOpE NotEquals)

-- order

instance OrdB (Expr Int) where
  (<*) = op2 (BinOpE LessThan)

instance OrdB (Expr Money) where
  (<*) = op2 (BinOpE LessThan)

instance OrdB (Expr Text) where
  (<*) = op2 (BinOpE LessThan)

instance OrdB (Expr Double) where
  (<*) = op2 (BinOpE LessThan)

--------------------------
-- text

concatText :: Expr Text -> Expr Text -> Expr Text
concatText (Expr a) (Expr b) = Expr $ Fix $ TextE $ TextAppend a b

lengthText :: Expr Text -> Expr Int
lengthText (Expr a) = Expr $ Fix $ App (Fix $ TextE TextLength) a

showInt :: Expr Int -> Expr Text
showInt (Expr a) = Expr $ Fix $ App (Fix $ TextE ConvertToText) a

showScript :: Expr Script -> Expr Text
showScript (Expr a) = Expr $ Fix $ App (Fix $ TextE ConvertToText) a

sha256 :: Expr Text -> Expr Text
sha256 (Expr a) = Expr $ Fix $ App (Fix $ TextE $ TextHash Sha256) a

blake2b256 :: Expr Text -> Expr Text
blake2b256 (Expr a) = Expr $ Fix $ App (Fix $ TextE $ TextHash Blake2b256) a

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
trace (Expr str) (Expr a) = Expr $ Fix $ Trace str a


