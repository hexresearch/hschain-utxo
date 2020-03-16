module Hschain.Utxo.Lang.Lib.Base(
    importBase
  , baseNames
  , baseLibTypeContext
  , langTypeContext
) where

import Hex.Common.Text

import qualified Prelude as P
import Prelude ((.))
import Prelude (($))
import Data.Text (Text)

import Data.Fix hiding ((~>))
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Infer

import Language.HM (monoT, forAllT)
import qualified Language.HM as H

import qualified Data.Map as M

langTypeContext :: H.Context Loc
langTypeContext =
  P.mappend defaultContext baseLibTypeContext

-- | Prelude functions
importBase :: Lang -> Lang
importBase = P.foldl (\f g x -> f (g x)) P.id $
  [ all
  , any
  , and
  , or
  , sum
  , product
  , sumInt
  , productInt
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
  , showBool
  , showScript
  , plus
  , times
  , minus
  , division
  , plusDouble
  , timesDouble
  , minusDouble
  , divisionDouble
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
  , fst
  , snd
  ] P.++ tupleFuns

baseNames :: [Text]
baseNames =
  [ "all"
  , "any"
  , "and"
  , "or"
  , "sumInt"
  , "productInt"
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
  , "showBool"
  , "showScript"
  , "+"
  , "*"
  , "-"
  , "/"
  , "+."
  , "*."
  , "-."
  , "/."
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
  , "fst"
  , "snd"
  ] P.++ tupleNames

tupleNames :: [Text]
tupleNames = P.fmap (P.uncurry toTupleName) tupleIndices

toTupleName :: P.Int -> P.Int -> Text
toTupleName size idx = P.mconcat ["tupleAt", showt size, "_", showt idx]

(~>) :: Type -> Type -> Type
(~>) a b = H.arrowT noLoc a b

forAllT' :: H.Var -> Signature -> Signature
forAllT' = forAllT noLoc

assumpType :: H.Var -> Signature -> (H.Var, Signature)
assumpType idx ty = (idx, ty)

baseLibTypeContext :: H.Context Loc
baseLibTypeContext = H.Context $ M.fromList $
  [ assumpType "and" (monoT $ vectorT boolT ~> boolT)
  , assumpType "or" (monoT $ vectorT boolT ~> boolT)
  , assumpType "all" (forA $ (aT ~> boolT) ~> vectorT aT ~> boolT)
  , assumpType "any" (forA $ (aT ~> boolT) ~> vectorT aT ~> boolT)
  , assumpType "sumInt"      (monoT $ vectorT intT ~> intT)
  , assumpType "productInt"  (monoT $ vectorT intT ~> intT)
  , assumpType "sum"      (monoT $ vectorT intT ~> intT)
  , assumpType "product"  (monoT $ vectorT intT ~> intT)
  , assumpType  "."  (forABC $ (bT ~> cT) ~> (aT ~> bT) ~> (aT ~> cT))
  , assumpType  "id"  (forA $ aT ~> aT)
  , assumpType "const" (forAB $ aT ~> bT ~> aT)
  , assumpType "getHeight" (monoT intT)
  , assumpType "getSelf" (monoT boxT)
  , assumpType "getOutput" (monoT $ intT ~> boxT)
  , assumpType "getInput"  (monoT $ intT ~> boxT)
  , assumpType "getOutputs" (monoT $ vectorT boxT)
  , assumpType "getInputs" (monoT $ vectorT boxT)
  , assumpType "getBoxId" (monoT $ boxT ~> textT)
  , assumpType "getBoxValue" (monoT $ boxT ~> intT)
  , assumpType "getBoxScript" (monoT $ boxT ~> scriptT)
  , assumpType "getBoxArg" (forA $ boxT ~> textT ~> aT)
  , assumpType "sha256" (monoT $ textT ~> textT)
  , assumpType "blake2b256" (monoT $ textT ~> textT)
  , assumpType "getVar" (forA $ textT ~> aT)
  , assumpType "trace" (forA $ textT ~> aT ~> aT)
  , assumpType "length" (forA $ vectorT aT ~> intT)
  , assumpType "lengthText" (monoT $ textT ~> intT)
  , assumpType "showInt" (monoT $ intT ~> textT)
  , assumpType "showDouble" (monoT $ intT ~> textT)
  , assumpType "showBool" (monoT $ boolT ~> textT)
  , assumpType "showScript" (monoT $ scriptT ~> textT)
  , assumpType "&&" (monoT $ boolT ~> boolT ~> boolT)
  , assumpType "||" (monoT $ boolT ~> boolT ~> boolT)
  , assumpType "not" (monoT $ boolT ~> boolT)
  , assumpType "+" (monoT $ intT ~> intT ~> intT)
  , assumpType "-" (monoT $ intT ~> intT ~> intT)
  , assumpType "*" (monoT $ intT ~> intT ~> intT)
  , assumpType "/" (monoT $ intT ~> intT ~> intT)
  , assumpType "+." (monoT $ intT ~> intT ~> intT)
  , assumpType "-." (monoT $ intT ~> intT ~> intT)
  , assumpType "*." (monoT $ intT ~> intT ~> intT)
  , assumpType "/." (monoT $ intT ~> intT ~> intT)
  , assumpType "++" (forA $ vectorT aT ~> vectorT aT ~> vectorT aT)
  , assumpType "<>" (monoT $ textT ~> textT ~> textT)
  , assumpType "map" (forAB $ (aT ~> bT) ~> vectorT aT ~> vectorT bT)
  , assumpType "fold" (forAB $ (aT ~> bT ~> aT) ~> aT ~> vectorT bT ~> aT)
  , assumpType "length" (forA $ vectorT aT ~> intT)
  , assumpType "pk" (monoT $ textT ~> boolT)
  , assumpType "!!" (forA $ vectorT aT ~> intT ~> aT)
  , assumpType "==" (forA $ aT ~> aT ~> boolT)
  , assumpType "/=" (forA $ aT ~> aT ~> boolT)
  , assumpType "<" (forA $ aT ~> aT ~> boolT)
  , assumpType "<=" (forA $ aT ~> aT ~> boolT)
  , assumpType ">=" (forA $ aT ~> aT ~> boolT)
  , assumpType ">" (forA $ aT ~> aT ~> boolT)
  , assumpType "fst" (forAB $ tupleT [aT, bT] ~> aT)
  , assumpType "snd" (forAB $ tupleT [aT, bT] ~> bT)
  ] P.++ tupleTypes
  where
    forA = forAllT' "a" . monoT
    forAB = forAllT' "a" . forAllT' "b" . monoT
    forABC = forAllT' "a" . forAllT' "b" . forAllT' "c" . monoT

    tupleTypes = P.fmap (P.uncurry toTupleType) tupleIndices
      where
        toTupleType size idx = assumpType (toTupleName size idx) (tupleAtType size idx)

        tupleAtType :: P.Int -> P.Int -> Signature
        tupleAtType size idx = pred $ monoT $ (tupleCon size) ~> (varT $ v idx)
          where
            pred :: Signature -> Signature
            pred = P.foldr (.) P.id $ P.fmap (\n -> forAllT noLoc (v n)) [0 .. size P.- 1]

        tupleCon size = tupleT $ P.fmap (varT . v) [0..size P.- 1]

        v n = P.mappend "a" (showt n)

tupleIndices :: [(P.Int, P.Int)]
tupleIndices = [ (size, idx) | size <- [2 .. maxTupleSize], idx <- [0 .. size P.- 1] ]

all :: Lang -> Lang
all = letIn "all" $ Fix $ LamList noLoc ["f", "xs"] $ app3 (Fix $ VecE noLoc (VecFold noLoc)) go z (Fix $ Var noLoc "xs")
  where
    z  = Fix $ PrimE noLoc $ PrimBool P.True
    go = Fix $ LamList noLoc ["x", "y"] $ Fix $ BinOpE noLoc And (Fix $ Var noLoc "x") (app1 (Fix $ Var noLoc "f") $ Fix $ Var noLoc "y")

any :: Lang -> Lang
any = letIn "any" $ Fix $ LamList noLoc ["f", "xs"] $ app3 (Fix $ VecE noLoc (VecFold noLoc)) go z (Fix $ Var noLoc "xs")
  where
    z  = Fix $ PrimE noLoc $ PrimBool P.False
    go = Fix $ LamList noLoc ["x", "y"] $ Fix $ BinOpE noLoc Or (Fix $ Var noLoc "x") (app1 (Fix $ Var noLoc "f") $ Fix $ Var noLoc "y")

and :: Lang -> Lang
and = letIn "and" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc And (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool P.True

or :: Lang -> Lang
or = letIn "or" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Or (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool P.False

sumInt :: Lang -> Lang
sumInt = letIn "sum" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Plus (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 0

productInt :: Lang -> Lang
productInt = letIn "product" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Times (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 1

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

showBool :: Lang -> Lang
showBool = letIn "showBool" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText BoolToText noLoc)) (Fix $ Var noLoc "x"))

showScript :: Lang -> Lang
showScript = letIn "showScript" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText ScriptToText noLoc)) (Fix $ Var noLoc "x"))

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

plusDouble :: Lang -> Lang
plusDouble = biOp "+." Plus

timesDouble :: Lang -> Lang
timesDouble = biOp "*." Times

minusDouble :: Lang -> Lang
minusDouble = biOp "-." Minus

divisionDouble :: Lang -> Lang
divisionDouble = biOp "/." Div

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

fst :: Lang -> Lang
fst = letIn "fst" (lam' "x" $ Fix $ UnOpE noLoc (TupleAt 2 0) (var' "x"))

snd :: Lang -> Lang
snd = letIn "snd" (lam' "x" $ Fix $ UnOpE noLoc (TupleAt 2 1) (var' "x"))

tupleFuns :: [Lang -> Lang]
tupleFuns = P.fmap (P.uncurry toFun) tupleIndices
  where
    toFun size idx = letIn (toTupleName size idx) $ lam' "x"  $ Fix $ UnOpE noLoc (TupleAt size idx) (var' "x")

lam' :: Text -> Lang -> Lang
lam' name expr = Fix $ Lam noLoc (VarName noLoc name) expr

var' :: Text -> Lang
var' name = Fix $ Var noLoc (VarName noLoc name)

f, x, y :: Lang

f = Fix $ Var noLoc "f"
x = Fix $ Var noLoc "x"
y = Fix $ Var noLoc "y"

aT, bT, cT :: Type
fT :: Type -> Type

varT :: H.Var -> Type
varT = H.varT noLoc

aT = varT "a"
bT = varT "b"
cT = varT "c"
fT ty = H.appT noLoc (varT "f") ty

letIn :: Text -> Lang -> Lang -> Lang
letIn var body x = singleLet noLoc (VarName noLoc var) body x


