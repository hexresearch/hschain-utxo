module Hschain.Utxo.Lang.Lib.Base(
    importBase
  , baseNames
  , baseLibTypeContext
  , langTypeContext
  , baseModuleCtx
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

infixr 6 ~>

baseModuleCtx :: ModuleCtx
baseModuleCtx = ModuleCtx
  { moduleCtx'types = baseLibTypeContext
  , moduleCtx'exprs = baseLibExecContext
  }

langTypeContext :: H.Context Loc
langTypeContext = baseLibTypeContext

-- | Prelude functions
importBase :: Lang -> Lang
importBase = bindGroupToLet baseFuns

baseLibExecContext :: ExecContext
baseLibExecContext = ExecContext $ M.fromList $ P.fmap fromBindToExec baseFuns
  where
    fromBindToExec Bind{..} = (bind'name, altToExpr bind'alt)

baseFuns :: [Bind Lang]
baseFuns =
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

all :: Bind Lang
all = bind "all" $ Fix $ LamList noLoc ["f", "xs"] $ app3 (Fix $ VecE noLoc (VecFold noLoc)) go z (Fix $ Var noLoc "xs")
  where
    z  = Fix $ PrimE noLoc $ PrimBool P.True
    go = Fix $ LamList noLoc ["x", "y"] $ Fix $ BinOpE noLoc And (Fix $ Var noLoc "x") (app1 (Fix $ Var noLoc "f") $ Fix $ Var noLoc "y")

any :: Bind Lang
any = bind "any" $ Fix $ LamList noLoc ["f", "xs"] $ app3 (Fix $ VecE noLoc (VecFold noLoc)) go z (Fix $ Var noLoc "xs")
  where
    z  = Fix $ PrimE noLoc $ PrimBool P.False
    go = Fix $ LamList noLoc ["x", "y"] $ Fix $ BinOpE noLoc Or (Fix $ Var noLoc "x") (app1 (Fix $ Var noLoc "f") $ Fix $ Var noLoc "y")

and :: Bind Lang
and = bind "and" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc And (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool P.True

or :: Bind Lang
or = bind "or" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Or (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool P.False

sumInt :: Bind Lang
sumInt = bind "sum" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Plus (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 0

productInt :: Bind Lang
productInt = bind "product" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Times (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 1

sum :: Bind Lang
sum = bind "sum" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Plus (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 0

product :: Bind Lang
product = bind "product" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Times (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 1

id :: Bind Lang
id = bind "id" $ Fix $ Lam noLoc "x" $ Fix $ Var noLoc "x"

const :: Bind Lang
const = bind "const" (Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ Var noLoc "x")

getHeight :: Bind Lang
getHeight = bind "getHeight" (Fix $ GetEnv noLoc (Height noLoc))

getSelf :: Bind Lang
getSelf = bind "getSelf" (Fix $ GetEnv noLoc (Self noLoc))

getOutput :: Bind Lang
getOutput = bind "getOutput" (Fix $ Lam noLoc "x" $ Fix $ GetEnv noLoc $ Output noLoc $ Fix $ Var noLoc "x")

getInput :: Bind Lang
getInput = bind "getInput" (Fix $ Lam noLoc "x" $ Fix $ GetEnv noLoc $ Input noLoc $ Fix $ Var noLoc "x")

getOutputs :: Bind Lang
getOutputs = bind "getOutputs" (Fix $ GetEnv noLoc $ Outputs noLoc)

getInputs :: Bind Lang
getInputs = bind "getInputs" (Fix $ GetEnv noLoc $ Inputs noLoc)

getBoxId :: Bind Lang
getBoxId = bind "getBoxId" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldId)

getBoxValue :: Bind Lang
getBoxValue = bind "getBoxValue" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldValue)

getBoxScript :: Bind Lang
getBoxScript = bind "getBoxScript" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldScript)

getBoxArg :: Bind Lang
getBoxArg = bind "getBoxArg" (Fix $ Lam noLoc "arg" $ Fix $ Lam noLoc "box" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "box") (BoxFieldArg $ Fix $ Var noLoc "arg"))

sha256 :: Bind Lang
sha256 = bind "sha256" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc $ TextHash noLoc Sha256) (Fix $ Var noLoc "x"))

blake2b256 :: Bind Lang
blake2b256 = bind "blake2b256" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc $ TextHash noLoc Blake2b256) (Fix $ Var noLoc "x"))

getVar :: Bind Lang
getVar = bind "getVar" (Fix $ Lam noLoc "x" $ Fix $ GetEnv noLoc $ GetVar noLoc $ Fix $ Var noLoc "x")

trace :: Bind Lang
trace = bind "trace" (Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ Trace noLoc (Fix $ Var noLoc "x") (Fix $ Var noLoc "y"))

showInt :: Bind Lang
showInt = bind "showInt" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText IntToText noLoc)) (Fix $ Var noLoc "x"))

showBool :: Bind Lang
showBool = bind "showBool" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText BoolToText noLoc)) (Fix $ Var noLoc "x"))

showScript :: Bind Lang
showScript = bind "showScript" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText ScriptToText noLoc)) (Fix $ Var noLoc "x"))

lengthVec :: Bind Lang
lengthVec = bind "length" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ VecE noLoc (VecLength noLoc)) (Fix $ Var noLoc "x"))

lengthText :: Bind Lang
lengthText = bind "lengthText" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (TextLength noLoc)) (Fix $ Var noLoc "x"))

biOp name op = bind name (Fix $ LamList noLoc ["x", "y"] $ Fix $ BinOpE noLoc op x y)
unOp name op = bind name (Fix $ Lam noLoc "x" $ Fix $ UnOpE noLoc op x)

eq :: Bind Lang
eq = biOp "==" Equals

neq :: Bind Lang
neq = biOp "/=" NotEquals

lt :: Bind Lang
lt = biOp "<" LessThan

lteq :: Bind Lang
lteq = biOp "<=" LessThanEquals

gt :: Bind Lang
gt = biOp ">" GreaterThan

gteq :: Bind Lang
gteq = biOp ">=" GreaterThanEquals

andB :: Bind Lang
andB = biOp "&&" And

orB :: Bind Lang
orB = biOp "||" Or

notB :: Bind Lang
notB = unOp "not" Not

plus :: Bind Lang
plus = biOp "+" Plus

times :: Bind Lang
times = biOp "*" Times

minus :: Bind Lang
minus = biOp "-" Minus

division :: Bind Lang
division = biOp "/" Div

plusDouble :: Bind Lang
plusDouble = biOp "+." Plus

timesDouble :: Bind Lang
timesDouble = biOp "*." Times

minusDouble :: Bind Lang
minusDouble = biOp "-." Minus

divisionDouble :: Bind Lang
divisionDouble = biOp "/." Div

mapVec :: Bind Lang
mapVec = bind "map" (Fix $ LamList noLoc ["f", "x"] $ app2 (Fix $ VecE noLoc (VecMap noLoc)) f x)

foldVec :: Bind Lang
foldVec = bind "fold" (Fix $ LamList noLoc ["f", "x", "y"] $ app3 (Fix $ VecE noLoc (VecFold noLoc)) f x y)

appendVec :: Bind Lang
appendVec = bind "++" (Fix $ LamList noLoc ["x", "y"] $ Fix $ VecE noLoc $ VecAppend noLoc x y)

appendText :: Bind Lang
appendText = bind "<>" (Fix $ LamList noLoc ["x", "y"] $ Fix $ TextE noLoc $ TextAppend noLoc x y)

atVec :: Bind Lang
atVec = bind "!!" (Fix $ LamList noLoc ["x", "y"] $ Fix $ VecE noLoc $ VecAt noLoc x y)

pk :: Bind Lang
pk = bind "pk" (Fix $ Lam noLoc "x" $ Fix $ Pk noLoc x)

fst :: Bind Lang
fst = bind "fst" (lam' "x" $ Fix $ UnOpE noLoc (TupleAt 2 0) (var' "x"))

snd :: Bind Lang
snd = bind "snd" (lam' "x" $ Fix $ UnOpE noLoc (TupleAt 2 1) (var' "x"))

tupleFuns :: [Bind Lang]
tupleFuns = P.fmap (P.uncurry toFun) tupleIndices
  where
    toFun size idx = bind (toTupleName size idx) $ lam' "x"  $ Fix $ UnOpE noLoc (TupleAt size idx) (var' "x")

lam' :: Text -> Lang -> Lang
lam' name expr = Fix $ Lam noLoc (PVar noLoc $ VarName noLoc name) expr

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

bind :: Text -> Lang -> Bind Lang
bind var body = Bind
  { bind'name = VarName noLoc var
  , bind'type = P.Nothing
  , bind'alt  = Alt [] body
  }

letIn :: Text -> Lang -> Lang -> Lang
letIn var body x = singleLet noLoc (VarName noLoc var) body x


