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
(~>) a b = H.arrowT P.Nothing a b

forAllT' :: H.Var -> Signature -> Signature
forAllT' = forAllT P.Nothing

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
            pred = P.foldr (.) P.id $ P.fmap (\n -> forAllT P.Nothing (v n)) [0 .. size P.- 1]

        tupleCon size = tupleT $ P.fmap (varT . v) [0..size P.- 1]

        v n = P.mappend "a" (showt n)

tupleIndices :: [(P.Int, P.Int)]
tupleIndices = [ (size, idx) | size <- [2 .. maxTupleSize], idx <- [0 .. size P.- 1] ]

all :: Bind Lang
all = bind "all" $ Fix $ LamList P.Nothing ["f", "xs"] $ app3 (Fix $ VecE P.Nothing (VecFold P.Nothing)) go z (Fix $ Var P.Nothing "xs")
  where
    z  = Fix $ PrimE P.Nothing $ PrimBool P.True
    go = Fix $ LamList P.Nothing ["x", "y"] $ Fix $ BinOpE P.Nothing And (Fix $ Var P.Nothing "x") (app1 (Fix $ Var P.Nothing "f") $ Fix $ Var P.Nothing "y")

any :: Bind Lang
any = bind "any" $ Fix $ LamList P.Nothing ["f", "xs"] $ app3 (Fix $ VecE P.Nothing (VecFold P.Nothing)) go z (Fix $ Var P.Nothing "xs")
  where
    z  = Fix $ PrimE P.Nothing $ PrimBool P.False
    go = Fix $ LamList P.Nothing ["x", "y"] $ Fix $ BinOpE P.Nothing Or (Fix $ Var P.Nothing "x") (app1 (Fix $ Var P.Nothing "f") $ Fix $ Var P.Nothing "y")

and :: Bind Lang
and = bind "and" (Fix (Apply P.Nothing (Fix $ Apply P.Nothing (Fix $ VecE P.Nothing (VecFold P.Nothing)) f) z))
  where
    f = Fix $ Lam P.Nothing "x" $ Fix $ Lam P.Nothing "y" $ Fix $ BinOpE P.Nothing And (Fix $ Var P.Nothing "x") (Fix $ Var P.Nothing "y")
    z = Fix $ PrimE P.Nothing $ PrimBool P.True

or :: Bind Lang
or = bind "or" (Fix (Apply P.Nothing (Fix $ Apply P.Nothing (Fix $ VecE P.Nothing (VecFold P.Nothing)) f) z))
  where
    f = Fix $ Lam P.Nothing "x" $ Fix $ Lam P.Nothing "y" $ Fix $ BinOpE P.Nothing Or (Fix $ Var P.Nothing "x") (Fix $ Var P.Nothing "y")
    z = Fix $ PrimE P.Nothing $ PrimBool P.False

sumInt :: Bind Lang
sumInt = bind "sum" (Fix (Apply P.Nothing (Fix $ Apply P.Nothing (Fix $ VecE P.Nothing (VecFold P.Nothing)) f) z))
  where
    f = Fix $ Lam P.Nothing "x" $ Fix $ Lam P.Nothing "y" $ Fix $ BinOpE P.Nothing Plus (Fix $ Var P.Nothing "x") (Fix $ Var P.Nothing "y")
    z = Fix $ PrimE P.Nothing $ PrimInt 0

productInt :: Bind Lang
productInt = bind "product" (Fix (Apply P.Nothing (Fix $ Apply P.Nothing (Fix $ VecE P.Nothing (VecFold P.Nothing)) f) z))
  where
    f = Fix $ Lam P.Nothing "x" $ Fix $ Lam P.Nothing "y" $ Fix $ BinOpE P.Nothing Times (Fix $ Var P.Nothing "x") (Fix $ Var P.Nothing "y")
    z = Fix $ PrimE P.Nothing $ PrimInt 1

sum :: Bind Lang
sum = bind "sum" (Fix (Apply P.Nothing (Fix $ Apply P.Nothing (Fix $ VecE P.Nothing (VecFold P.Nothing)) f) z))
  where
    f = Fix $ Lam P.Nothing "x" $ Fix $ Lam P.Nothing "y" $ Fix $ BinOpE P.Nothing Plus (Fix $ Var P.Nothing "x") (Fix $ Var P.Nothing "y")
    z = Fix $ PrimE P.Nothing $ PrimInt 0

product :: Bind Lang
product = bind "product" (Fix (Apply P.Nothing (Fix $ Apply P.Nothing (Fix $ VecE P.Nothing (VecFold P.Nothing)) f) z))
  where
    f = Fix $ Lam P.Nothing "x" $ Fix $ Lam P.Nothing "y" $ Fix $ BinOpE P.Nothing Times (Fix $ Var P.Nothing "x") (Fix $ Var P.Nothing "y")
    z = Fix $ PrimE P.Nothing $ PrimInt 1

id :: Bind Lang
id = bind "id" $ Fix $ Lam P.Nothing "x" $ Fix $ Var P.Nothing "x"

const :: Bind Lang
const = bind "const" (Fix $ Lam P.Nothing "x" $ Fix $ Lam P.Nothing "y" $ Fix $ Var P.Nothing "x")

getHeight :: Bind Lang
getHeight = bind "getHeight" (Fix $ GetEnv P.Nothing (Height P.Nothing))

getSelf :: Bind Lang
getSelf = bind "getSelf" (Fix $ GetEnv P.Nothing (Self P.Nothing))

getOutput :: Bind Lang
getOutput = bind "getOutput" (Fix $ Lam P.Nothing "x" $ Fix $ GetEnv P.Nothing $ Output P.Nothing $ Fix $ Var P.Nothing "x")

getInput :: Bind Lang
getInput = bind "getInput" (Fix $ Lam P.Nothing "x" $ Fix $ GetEnv P.Nothing $ Input P.Nothing $ Fix $ Var P.Nothing "x")

getOutputs :: Bind Lang
getOutputs = bind "getOutputs" (Fix $ GetEnv P.Nothing $ Outputs P.Nothing)

getInputs :: Bind Lang
getInputs = bind "getInputs" (Fix $ GetEnv P.Nothing $ Inputs P.Nothing)

getBoxId :: Bind Lang
getBoxId = bind "getBoxId" (Fix $ Lam P.Nothing "x" $ Fix $ BoxE P.Nothing $ BoxAt P.Nothing (Fix $ Var P.Nothing "x") BoxFieldId)

getBoxValue :: Bind Lang
getBoxValue = bind "getBoxValue" (Fix $ Lam P.Nothing "x" $ Fix $ BoxE P.Nothing $ BoxAt P.Nothing (Fix $ Var P.Nothing "x") BoxFieldValue)

getBoxScript :: Bind Lang
getBoxScript = bind "getBoxScript" (Fix $ Lam P.Nothing "x" $ Fix $ BoxE P.Nothing $ BoxAt P.Nothing (Fix $ Var P.Nothing "x") BoxFieldScript)

getBoxArg :: Bind Lang
getBoxArg = bind "getBoxArg" (Fix $ Lam P.Nothing "arg" $ Fix $ Lam P.Nothing "box" $ Fix $ BoxE P.Nothing $ BoxAt P.Nothing (Fix $ Var P.Nothing "box") (BoxFieldArg $ Fix $ Var P.Nothing "arg"))

sha256 :: Bind Lang
sha256 = bind "sha256" (Fix $ Lam P.Nothing "x" $ Fix $ Apply P.Nothing (Fix $ TextE P.Nothing $ TextHash P.Nothing Sha256) (Fix $ Var P.Nothing "x"))

blake2b256 :: Bind Lang
blake2b256 = bind "blake2b256" (Fix $ Lam P.Nothing "x" $ Fix $ Apply P.Nothing (Fix $ TextE P.Nothing $ TextHash P.Nothing Blake2b256) (Fix $ Var P.Nothing "x"))

getVar :: Bind Lang
getVar = bind "getVar" (Fix $ Lam P.Nothing "x" $ Fix $ GetEnv P.Nothing $ GetVar P.Nothing $ Fix $ Var P.Nothing "x")

trace :: Bind Lang
trace = bind "trace" (Fix $ Lam P.Nothing "x" $ Fix $ Lam P.Nothing "y" $ Fix $ Trace P.Nothing (Fix $ Var P.Nothing "x") (Fix $ Var P.Nothing "y"))

showInt :: Bind Lang
showInt = bind "showInt" (Fix $ Lam P.Nothing "x" $ Fix $ Apply P.Nothing (Fix $ TextE P.Nothing (ConvertToText IntToText P.Nothing)) (Fix $ Var P.Nothing "x"))

showBool :: Bind Lang
showBool = bind "showBool" (Fix $ Lam P.Nothing "x" $ Fix $ Apply P.Nothing (Fix $ TextE P.Nothing (ConvertToText BoolToText P.Nothing)) (Fix $ Var P.Nothing "x"))

showScript :: Bind Lang
showScript = bind "showScript" (Fix $ Lam P.Nothing "x" $ Fix $ Apply P.Nothing (Fix $ TextE P.Nothing (ConvertToText ScriptToText P.Nothing)) (Fix $ Var P.Nothing "x"))

lengthVec :: Bind Lang
lengthVec = bind "length" (Fix $ Lam P.Nothing "x" $ Fix $ Apply P.Nothing (Fix $ VecE P.Nothing (VecLength P.Nothing)) (Fix $ Var P.Nothing "x"))

lengthText :: Bind Lang
lengthText = bind "lengthText" (Fix $ Lam P.Nothing "x" $ Fix $ Apply P.Nothing (Fix $ TextE P.Nothing (TextLength P.Nothing)) (Fix $ Var P.Nothing "x"))

biOp name op = bind name (Fix $ LamList P.Nothing ["x", "y"] $ Fix $ BinOpE P.Nothing op x y)
unOp name op = bind name (Fix $ Lam P.Nothing "x" $ Fix $ UnOpE P.Nothing op x)

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
mapVec = bind "map" (Fix $ LamList P.Nothing ["f", "x"] $ app2 (Fix $ VecE P.Nothing (VecMap P.Nothing)) f x)

foldVec :: Bind Lang
foldVec = bind "fold" (Fix $ LamList P.Nothing ["f", "x", "y"] $ app3 (Fix $ VecE P.Nothing (VecFold P.Nothing)) f x y)

appendVec :: Bind Lang
appendVec = bind "++" (Fix $ LamList P.Nothing ["x", "y"] $ Fix $ VecE P.Nothing $ VecAppend P.Nothing x y)

appendText :: Bind Lang
appendText = bind "<>" (Fix $ LamList P.Nothing ["x", "y"] $ Fix $ TextE P.Nothing $ TextAppend P.Nothing x y)

atVec :: Bind Lang
atVec = bind "!!" (Fix $ LamList P.Nothing ["x", "y"] $ Fix $ VecE P.Nothing $ VecAt P.Nothing x y)

pk :: Bind Lang
pk = bind "pk" (Fix $ Lam P.Nothing "x" $ Fix $ Pk P.Nothing x)

fst :: Bind Lang
fst = bind "fst" (lam' "x" $ Fix $ UnOpE P.Nothing (TupleAt 2 0) (var' "x"))

snd :: Bind Lang
snd = bind "snd" (lam' "x" $ Fix $ UnOpE P.Nothing (TupleAt 2 1) (var' "x"))

tupleFuns :: [Bind Lang]
tupleFuns = P.fmap (P.uncurry toFun) tupleIndices
  where
    toFun size idx = bind (toTupleName size idx) $ lam' "x"  $ Fix $ UnOpE P.Nothing (TupleAt size idx) (var' "x")

lam' :: Text -> Lang -> Lang
lam' name expr = Fix $ Lam P.Nothing (VarName P.Nothing name) expr

var' :: Text -> Lang
var' name = Fix $ Var P.Nothing (VarName P.Nothing name)

f, x, y :: Lang

f = Fix $ Var P.Nothing "f"
x = Fix $ Var P.Nothing "x"
y = Fix $ Var P.Nothing "y"

aT, bT, cT :: Type
fT :: Type -> Type

varT :: H.Var -> Type
varT = H.varT P.Nothing

aT = varT "a"
bT = varT "b"
cT = varT "c"
fT ty = H.appT P.Nothing (varT "f") ty

bind :: Text -> Lang -> Bind Lang
bind var body = Bind
  { bind'name = VarName P.Nothing var
  , bind'type = P.Nothing
  , bind'alt  = Alt [] body
  }

letIn :: Text -> Lang -> Lang -> Lang
letIn var body x = singleLet P.Nothing (VarName P.Nothing var) body x


