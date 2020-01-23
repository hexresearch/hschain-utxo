module Hschain.Utxo.Lang.Lib.Base(
    importBase
  , baseNames
  , baseTypeAssump
) where

import qualified Prelude as P
import Prelude (($))
import Data.Text (Text)

import Type.Type
import Data.Fix hiding ((~>))
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Infer

-- | Prelude functions
importBase :: Lang -> Lang
importBase = P.foldl (\f g x -> f (g x)) P.id
  [ all
  , any
  , sum
  , product
  , id
  , const
  , getHeight
  , getSelf
  , getOutput
  , getInput
  , getOutputs
  , getInputs
  , getBoxId
  , getBoxValue
  , getBoxScript
  , getBoxArg
  , sha256
  , blake2b256
  , getVar
  , trace
  , lengthVec
  , lengthText
  , showInt
  , showDouble
  , showMoney
  , showBool
  , showScript
  , plus
  , times
  , minus
  , division
  , appendText
  , appendVec
  , mapVec
  , foldVec
  , pk
  , atVec
  , andB
  , orB
  , notB
  , eq
  , neq
  , lt
  , gt
  , lteq
  , gteq
  ]

baseNames :: [Text]
baseNames =
  [ "all"
  , "any"
  , "sum"
  , "product"
  , "id"
  , "const"
  , "getHeight"
  , "getSelf"
  , "getOutput"
  , "getInput"
  , "getOutputs"
  , "getInputs"
  , "getBoxId"
  , "getBoxValue"
  , "getBoxScript"
  , "getBoxArg"
  , "sha256"
  , "blake2b256"
  , "getVar"
  , "trace"
  , "length"
  , "lengthText"
  , "showInt"
  , "showDouble"
  , "showMoney"
  , "showBool"
  , "showScript"
  , "+"
  , "*"
  , "-"
  , "/"
  , "<>"
  , "++"
  , "map"
  , "fold"
  , "length"
  , "pk"
  , "!!"
  , "&&"
  , "||"
  , "not"
  , "=="
  , "/="
  , "<"
  , ">"
  , "<="
  , ">="
  ]

baseTypeAssump :: [Assump]
baseTypeAssump =
  [ assumpType  "all" (vectorT boolT ~> boolT)
  , assumpType  "any" (vectorT boolT ~> boolT)
  , assumpType' "sum"      "Num" aT (vectorT aT ~> aT)
  , assumpType' "product"  "Num" aT (vectorT aT ~> aT)
  , assumpType  "."  ((bT ~> cT) ~> (aT ~> bT) ~> (aT ~> cT))
  , assumpType  "id"  (aT ~> aT)
  , assumpType "const" (aT ~> bT ~> aT)
  , assumpType "getHeight" intT
  , assumpType "getSelf" boxT
  , assumpType "getOutput" (intT ~> boxT)
  , assumpType "getInput"  (intT ~> boxT)
  , assumpType "getOutputs" (vectorT boxT)
  , assumpType "getInputs" (vectorT boxT)
  , assumpType "getBoxId" (boxT ~> textT)
  , assumpType "getBoxValue" (boxT ~> moneyT)
  , assumpType "getBoxScript" (boxT ~> scriptT)
  , assumpType "getBoxArg" (boxT ~> textT ~> aT)
  , assumpType "sha256" (textT ~> textT)
  , assumpType "blake2b256" (textT ~> textT)
  , assumpType "getVar" (textT ~> aT)
  , assumpType "trace" (textT ~> aT ~> aT)
  , assumpType "length" (vectorT aT ~> intT)
  , assumpType "lengthText" (textT ~> intT)
  , assumpType "showInt" (intT ~> textT)
  , assumpType "showDouble" (doubleT ~> textT)
  , assumpType "showMoney" (moneyT ~> textT)
  , assumpType "showBool" (boolT ~> textT)
  , assumpType "showScript" (scriptT ~> textT)
  , assumpType "&&" (boolT ~> boolT ~> boolT)
  , assumpType "||" (boolT ~> boolT ~> boolT)
  , assumpType "not" (boolT ~> boolT)
  , assumpType' "+" "Num" aT (aT ~> aT ~> aT)
  , assumpType' "-" "Num" aT (aT ~> aT ~> aT)
  , assumpType' "*" "Num" aT (aT ~> aT ~> aT)
  , assumpType' "/" "Num" aT (aT ~> aT ~> aT)
  , assumpType "++" (vectorT aT ~> vectorT aT ~> vectorT aT)
  , assumpType "<>" (textT ~> textT ~> textT)
  , assumpType "map" ((aT ~> bT) ~> vectorT aT ~> vectorT bT)
  , assumpType "fold" ((aT ~> bT ~> aT) ~> aT ~> vectorT bT ~> aT)
  , assumpType "length" (vectorT aT ~> intT)
  , assumpType "pk" (textT ~> boolT)
  , assumpType "!!" (vectorT aT ~> intT ~> aT)
  , assumpType' "==" "Eq" aT (aT ~> aT ~> boolT)
  , assumpType' "/=" "Eq" aT (aT ~> aT ~> boolT)
  , assumpType' "<" "Ord" aT (aT ~> aT ~> boolT)
  , assumpType' "<=" "Ord" aT (aT ~> aT ~> boolT)
  , assumpType' ">=" "Ord" aT (aT ~> aT ~> boolT)
  , assumpType' ">" "Ord" aT (aT ~> aT ~> boolT)
  ]


all :: Lang -> Lang
all = letIn "all" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc And (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool P.True

any :: Lang -> Lang
any = letIn "any" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Or (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool P.False

sum :: Lang -> Lang
sum = letIn "sum" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Plus (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 0

product :: Lang -> Lang
product = letIn "product" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Times (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 1

id :: Lang -> Lang
id = letIn "id" $ Fix $ Lam noLoc "x" $ Fix $ Var noLoc "x"

const :: Lang -> Lang
const = letIn "const" (Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ Var noLoc "x")

getHeight :: Lang -> Lang
getHeight x = letIn "getHeight" (Fix $ GetEnv noLoc (Height noLoc)) x

getSelf :: Lang -> Lang
getSelf = letIn "getSelf" (Fix $ GetEnv noLoc (Self noLoc))

getOutput :: Lang -> Lang
getOutput = letIn "getOutput" (Fix $ Lam noLoc "x" $ Fix $ GetEnv noLoc $ Output noLoc $ Fix $ Var noLoc "x")

getInput :: Lang -> Lang
getInput = letIn "getInput" (Fix $ Lam noLoc "x" $ Fix $ GetEnv noLoc $ Input noLoc $ Fix $ Var noLoc "x")

getOutputs :: Lang -> Lang
getOutputs = letIn "getOutputs" (Fix $ GetEnv noLoc $ Outputs noLoc)

getInputs :: Lang -> Lang
getInputs = letIn "getInputs" (Fix $ GetEnv noLoc $ Inputs noLoc)

getBoxId :: Lang -> Lang
getBoxId = letIn "getBoxId" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldId)

getBoxValue :: Lang -> Lang
getBoxValue = letIn "getBoxValue" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldValue)

getBoxScript :: Lang -> Lang
getBoxScript = letIn "getBoxScript" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldScript)

getBoxArg :: Lang -> Lang
getBoxArg = letIn "getBoxArg" (Fix $ Lam noLoc "arg" $ Fix $ Lam noLoc "box" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "box") (BoxFieldArg $ Fix $ Var noLoc "arg"))

sha256 :: Lang -> Lang
sha256 = letIn "sha256" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc $ TextHash noLoc Sha256) (Fix $ Var noLoc "x"))

blake2b256 :: Lang -> Lang
blake2b256 = letIn "blake2b256" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc $ TextHash noLoc Blake2b256) (Fix $ Var noLoc "x"))

getVar :: Lang -> Lang
getVar = letIn "getVar" (Fix $ Lam noLoc "x" $ Fix $ GetEnv noLoc $ GetVar noLoc $ Fix $ Var noLoc "x")

trace :: Lang -> Lang
trace = letIn "trace" (Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ Trace noLoc (Fix $ Var noLoc "x") (Fix $ Var noLoc "y"))

showInt :: Lang -> Lang
showInt = letIn "showInt" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText IntToText noLoc)) (Fix $ Var noLoc "x"))

showDouble :: Lang -> Lang
showDouble = letIn "showDouble" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText DoubleToText noLoc)) (Fix $ Var noLoc "x"))

showBool :: Lang -> Lang
showBool = letIn "showBool" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText BoolToText noLoc)) (Fix $ Var noLoc "x"))

showScript :: Lang -> Lang
showScript = letIn "showScript" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText ScriptToText noLoc)) (Fix $ Var noLoc "x"))

showMoney :: Lang -> Lang
showMoney = letIn "showMoney" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText MoneyToText noLoc)) (Fix $ Var noLoc "x"))

lengthVec :: Lang -> Lang
lengthVec = letIn "length" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ VecE noLoc (VecLength noLoc)) (Fix $ Var noLoc "x"))

lengthText :: Lang -> Lang
lengthText = letIn "lengthText" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (TextLength noLoc)) (Fix $ Var noLoc "x"))

biOp name op = letIn name (Fix $ LamList noLoc ["x", "y"] $ Fix $ BinOpE noLoc op x y)
unOp name op = letIn name (Fix $ Lam noLoc "x" $ Fix $ UnOpE noLoc op x)

eq :: Lang -> Lang
eq = biOp "==" Equals

neq :: Lang -> Lang
neq = biOp "/=" NotEquals

lt :: Lang -> Lang
lt = biOp "<" LessThan

lteq :: Lang -> Lang
lteq = biOp "<=" LessThanEquals

gt :: Lang -> Lang
gt = biOp ">" GreaterThan

gteq :: Lang -> Lang
gteq = biOp ">=" GreaterThanEquals

andB :: Lang -> Lang
andB = biOp "&&" And

orB :: Lang -> Lang
orB = biOp "||" Or

notB :: Lang -> Lang
notB = unOp "not" Not

plus :: Lang -> Lang
plus = biOp "+" Plus

times :: Lang -> Lang
times = biOp "*" Times

minus :: Lang -> Lang
minus = biOp "-" Minus

division :: Lang -> Lang
division = biOp "/" Div

mapVec :: Lang -> Lang
mapVec = letIn "map" (Fix $ LamList noLoc ["f", "x"] $ app2 (Fix $ VecE noLoc (VecMap noLoc)) f x)

foldVec :: Lang -> Lang
foldVec = letIn "fold" (Fix $ LamList noLoc ["f", "x", "y"] $ app3 (Fix $ VecE noLoc (VecFold noLoc)) f x y)

appendVec :: Lang -> Lang
appendVec = letIn "++" (Fix $ LamList noLoc ["x", "y"] $ Fix $ VecE noLoc $ VecAppend noLoc x y)

appendText :: Lang -> Lang
appendText = letIn "<>" (Fix $ LamList noLoc ["x", "y"] $ Fix $ TextE noLoc $ TextAppend noLoc x y)

atVec :: Lang -> Lang
atVec = letIn "!!" (Fix $ LamList noLoc ["x", "y"] $ Fix $ VecE noLoc $ VecAt noLoc x y)

pk :: Lang -> Lang
pk = letIn "pk" (Fix $ Lam noLoc "x" $ Fix $ Pk noLoc x)

f, x, y :: Lang

f = Fix $ Var noLoc "f"
x = Fix $ Var noLoc "x"
y = Fix $ Var noLoc "y"

aT, bT, cT :: Type
fT :: Type -> Type

aT = var "a"
bT = var "b"
cT = var "c"
fT ty = TAp noLoc (TVar noLoc (Tyvar noLoc "f" (Kfun noLoc (Star noLoc) (Star noLoc)))) ty

letIn :: Text -> Lang -> Lang -> Lang
letIn var body x = singleLet noLoc (VarName noLoc var) body x

assumpType :: Id -> Type -> Assump
assumpType idx ty = idx :>: (Forall noLoc [Star noLoc] $ Qual noLoc [] ty)

assumpType' :: Id -> Id -> Type -> Type -> Assump
assumpType' idx cls var ty = idx :>: (Forall noLoc [Star noLoc] $ Qual noLoc [IsIn noLoc cls var] ty)

