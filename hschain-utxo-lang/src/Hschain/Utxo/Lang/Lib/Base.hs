module Hschain.Utxo.Lang.Lib.Base(
    importBase
  , baseNames
  , baseLibTypeContext
  , langTypeContext
  , baseModuleCtx
) where

import qualified Prelude as P
import Prelude ((.))
import Prelude (($))

import Data.Either
import Data.Text (Text)

import Data.Fix hiding ((~>))
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types (ArgType(..),argTypes)
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Infer

import Language.HM (monoT, forAllT)
import qualified Language.HM as H

import qualified Data.Map as M

import qualified Hschain.Utxo.Lang.Const as Const

infixr 6 ~>

baseModuleCtx :: ModuleCtx
baseModuleCtx = ModuleCtx
  { moduleCtx'types = InferCtx baseLibTypeContext P.mempty
  , moduleCtx'exprs = baseLibExecContext
  }

langTypeContext :: TypeContext
langTypeContext = baseLibTypeContext

-- | Prelude functions
importBase :: Lang -> Lang
importBase = bindGroupToLet baseFuns

baseLibExecContext :: ExecCtx
baseLibExecContext =
  fromRight err $ runInferM $ fmap ((\vars -> ExecCtx vars) . M.fromList) $ P.mapM fromBindToExec baseFuns
  where
    fromBindToExec Bind{..} = fmap (bind'name, ) $ altGroupToExpr bind'alts
    err = P.error "Failed to load base module"

baseFuns :: [Bind Lang]
baseFuns =
  [ all
  , any
  , andSigma
  , orSigma
  , and
  , or
  , sum
  , product
  , id
  , const
  , compose
  , getHeight
  , getSelf
  , getOutput
  , getInput
  , getOutputs
  , getInputs
  , getDataInputs
  , getBoxId
  , getBoxValue
  , getBoxScript
  , getBoxPostHeight
  , sha256
  , lengthVec
  , lengthText
  , lengthBytes
  , showInt
  , showBool
  , plus
  , times
  , minus
  , division
  , plusDouble
  , timesDouble
  , minusDouble
  , divisionDouble
  , appendText
  , appendBytes
  , appendVec
  , mapVec
  , foldVec
  , pk
  , toSigma
  , sigmaAnd
  , sigmaOr
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
  , otherwise
  , undefined
  , checkSig
  , checkMultiSig
  ] P.++ getBoxArgListFuns P.++ getVars
  where
    getBoxArgListFuns = fmap getBoxArgList argTypes
    getVars = fmap getVarBy argTypes

baseNames :: [Text]
baseNames =
  [ "all"
  , "any"
  , "and"
  , "or"
  , "andSigma"
  , "orSigma"
  , "sum"
  , "product"
  , "id"
  , "const"
  , "."
  , "getHeight"
  , "getSelf"
  , "getOutput"
  , "getInput"
  , "getOutputs"
  , "getInputs"
  , "getDataInputs"
  , "getBoxId"
  , "getBoxValue"
  , "getBoxScript"
  , "getBoxPostHeight"
  , "getBoxArg"
  , "sha256"
  , "blake2b256"
  , "length"
  , "lengthText"
  , "lengthBytes"
  , "showInt"
  , "showBool"
  , "+"
  , "*"
  , "-"
  , "/"
  , "+."
  , "*."
  , "-."
  , "/."
  , "<>"
  , "appendBytes"
  , "++"
  , "map"
  , "length"
  , "pk"
  , "toSigma"
  , "sigmaAnd"
  , "sigmaOr"
  , Const.listAt
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
  , "otherwise"
  , "undefined"
  , Const.checkSig
  , Const.checkMultiSig
  ] P.++ getVarNames

getVarNames :: [Text]
getVarNames = fmap getEnvVarName argTypes

(~>) :: Type -> Type -> Type
(~>) a b = H.arrowT noLoc a b

forAllT' :: Text -> Signature -> Signature
forAllT' = forAllT noLoc

assumpType :: Text -> Signature -> (Text, Signature)
assumpType idx ty = (idx, ty)

baseLibTypeContext :: TypeContext
baseLibTypeContext = H.Context $ M.fromList $
  [ assumpType "and" (monoT $ listT boolT ~> boolT)
  , assumpType "or" (monoT $ listT boolT ~> boolT)
  , assumpType "andSigma" (monoT $ listT sigmaT ~> sigmaT)
  , assumpType "orSigma" (monoT $ listT sigmaT ~> sigmaT)
  , assumpType "all" (forA $ (aT ~> boolT) ~> listT aT ~> boolT)
  , assumpType "any" (forA $ (aT ~> boolT) ~> listT aT ~> boolT)
  , assumpType "sum"      (monoT $ listT intT ~> intT)
  , assumpType "product"  (monoT $ listT intT ~> intT)
  , assumpType  "."  (forABC $ (bT ~> cT) ~> (aT ~> bT) ~> (aT ~> cT))
  , assumpType  "id"  (forA $ aT ~> aT)
  , assumpType "const" (forAB $ aT ~> bT ~> aT)
  , assumpType "getHeight" (monoT intT)
  , assumpType "getSelf" (monoT boxT)
  , assumpType "getOutput" (monoT $ intT ~> boxT)
  , assumpType "getInput"  (monoT $ intT ~> boxT)
  , assumpType "getOutputs" (monoT $ listT boxT)
  , assumpType "getInputs" (monoT $ listT boxT)
  , assumpType "getDataInputs" (monoT $ listT boxT)
  , assumpType "getBoxId" (monoT $ boxT ~> textT)
  , assumpType "getBoxValue" (monoT $ boxT ~> intT)
  , assumpType "getBoxPostHeight" (monoT $ boxT ~> intT)
  , assumpType "getBoxScript" (monoT $ boxT ~> scriptT)
  , assumpType "sha256" (monoT $ bytesT ~> bytesT)
  , assumpType "getVar" (forA $ textT ~> aT)
  , assumpType "length" (forA $ listT aT ~> intT)
  , assumpType "lengthText" (monoT $ textT ~> intT)
  , assumpType "lengthBytes" (monoT $ bytesT ~> intT)
  , assumpType "showInt" (monoT $ intT ~> textT)
  , assumpType "showBool" (monoT $ boolT ~> textT)
  , assumpType "not" (monoT $ boolT ~> boolT)
  , assumpType "&&" (monoT $ boolT ~> boolT ~> boolT)
  , assumpType "||" (monoT $ boolT ~> boolT ~> boolT)
  , assumpType "sigmaAnd" (monoT $ sigmaT ~> sigmaT ~> sigmaT)
  , assumpType "sigmaOr" (monoT $ sigmaT ~> sigmaT ~> sigmaT)
  , assumpType "pk" (monoT $ bytesT ~> sigmaT)
  , assumpType "toSigma" (monoT $ boolT ~> sigmaT)
  , assumpType "+" (monoT $ intT ~> intT ~> intT)
  , assumpType "-" (monoT $ intT ~> intT ~> intT)
  , assumpType "*" (monoT $ intT ~> intT ~> intT)
  , assumpType "/" (monoT $ intT ~> intT ~> intT)
  , assumpType "+." (monoT $ intT ~> intT ~> intT)
  , assumpType "-." (monoT $ intT ~> intT ~> intT)
  , assumpType "*." (monoT $ intT ~> intT ~> intT)
  , assumpType "/." (monoT $ intT ~> intT ~> intT)
  , assumpType "++" (forA $ listT aT ~> listT aT ~> listT aT)
  , assumpType "<>" (monoT $ textT ~> textT ~> textT)
  , assumpType "appendBytes" (monoT $ bytesT ~> bytesT ~> bytesT)
  , assumpType "map" (forAB $ (aT ~> bT) ~> listT aT ~> listT bT)
  , assumpType Const.foldl (forAB $ (aT ~> bT ~> aT) ~> aT ~> listT bT ~> aT)
  , assumpType "length" (forA $ listT aT ~> intT)
  , assumpType Const.listAt (forA $ listT aT ~> intT ~> aT)
  , assumpType "==" (forA $ aT ~> aT ~> boolT)
  , assumpType "/=" (forA $ aT ~> aT ~> boolT)
  , assumpType "<" (forA $ aT ~> aT ~> boolT)
  , assumpType "<=" (forA $ aT ~> aT ~> boolT)
  , assumpType ">=" (forA $ aT ~> aT ~> boolT)
  , assumpType ">" (forA $ aT ~> aT ~> boolT)
  , assumpType "fst" (forAB $ tupleT [aT, bT] ~> aT)
  , assumpType "snd" (forAB $ tupleT [aT, bT] ~> bT)
  , assumpType "otherwise" (monoT boolT)
  , assumpType "undefined" $ forA aT
  , assumpType Const.checkSig $ monoT $ bytesT ~> intT ~> boolT
  , assumpType Const.checkMultiSig $ monoT $ intT ~> listT bytesT ~> listT intT ~> boolT
  ] P.++ getBoxArgListTypes P.++ getEnvVarTypes
  where
    forA = forAllT' "a" . monoT
    forAB = forAllT' "a" . forAllT' "b" . monoT
    forABC = forAllT' "a" . forAllT' "b" . forAllT' "c" . monoT

    getBoxArgListTypes =
      fmap (\ty -> assumpType (getBoxArgVar ty) (monoT $ boxT ~> listT (argTagToType ty))) argTypes

    getEnvVarTypes =
      fmap (\ty -> assumpType (getEnvVarName ty) (monoT $ listT (argTagToType ty))) argTypes

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
and = bind "and" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) g) z))
  where
    g = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc And (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool P.True

or :: Bind Lang
or = bind "or" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) g) z))
  where
    g = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Or (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool P.False

andSigma :: Bind Lang
andSigma = bind "andSigma" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ VecE noLoc $ VecAndSigma noLoc) x)

orSigma :: Bind Lang
orSigma = bind "orSigma" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ VecE noLoc $ VecOrSigma noLoc) x)

sum :: Bind Lang
sum = bind "sum" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) g) z))
  where
    g = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Plus (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 0

product :: Bind Lang
product = bind "product" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) g) z))
  where
    g = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Times (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt 1

id :: Bind Lang
id = bind "id" $ Fix $ Lam noLoc "x" $ Fix $ Var noLoc "x"

const :: Bind Lang
const = bind "const" (Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ Var noLoc "x")

compose :: Bind Lang
compose = bind "." (Fix $ LamList noLoc ["f", "g", "x"] (Fix $ Apply noLoc (var' "f") (Fix $ Apply noLoc (var' "g") (var' "x"))))

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

getDataInputs :: Bind Lang
getDataInputs = bind "getDataInputs" (Fix $ GetEnv noLoc $ DataInputs noLoc)

getBoxId :: Bind Lang
getBoxId = bind "getBoxId" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldId)

getBoxValue :: Bind Lang
getBoxValue = bind "getBoxValue" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldValue)

getBoxScript :: Bind Lang
getBoxScript = bind "getBoxScript" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldScript)

getBoxPostHeight :: Bind Lang
getBoxPostHeight = bind "getBoxPostHeight" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldPostHeight)

getBoxArgList :: ArgType -> Bind Lang
getBoxArgList ty = bind (getBoxArgVar ty) (Fix $ Lam noLoc "box" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "box") (BoxFieldArgList ty))

sha256 :: Bind Lang
sha256 = bind "sha256" (Fix $ Lam noLoc "x" $ Fix $ BytesE noLoc $ BytesHash noLoc Sha256 (Fix $ Var noLoc "x"))

getVarBy :: ArgType -> Bind Lang
getVarBy ty = bind (getEnvVarName ty) (Fix $ GetEnv noLoc $ GetVar noLoc ty)

showInt :: Bind Lang
showInt = bind "showInt" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText noLoc IntToText)) (Fix $ Var noLoc "x"))

showBool :: Bind Lang
showBool = bind "showBool" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText noLoc BoolToText)) (Fix $ Var noLoc "x"))

lengthVec :: Bind Lang
lengthVec = bind "length" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ VecE noLoc (VecLength noLoc)) (Fix $ Var noLoc "x"))

lengthText :: Bind Lang
lengthText = bind "lengthText" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (TextLength noLoc)) (Fix $ Var noLoc "x"))

lengthBytes :: Bind Lang
lengthBytes = bind "lengthBytes" (Fix $ Lam noLoc "x" $ Fix $ BytesE noLoc $ BytesLength noLoc x)

biOp :: Text -> BinOp -> Bind Lang
biOp name op = bind name (Fix $ LamList noLoc ["x", "y"] $ Fix $ BinOpE noLoc op x y)

unOp :: Text -> UnOp -> Bind Lang
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

sigmaAnd :: Bind Lang
sigmaAnd = bind "sigmaAnd" (Fix $ LamList noLoc ["x", "y"] $ Fix $ SigmaE noLoc $ SAnd noLoc x y)

sigmaOr :: Bind Lang
sigmaOr = bind "sigmaOr" (Fix $ LamList noLoc ["x", "y"] $ Fix $ SigmaE noLoc $ SOr noLoc x y)

toSigma :: Bind Lang
toSigma = bind "toSigma" (Fix $ Lam noLoc "x" $ Fix $ SigmaE noLoc $ SPrimBool noLoc x)

pk :: Bind Lang
pk = bind "pk" (Fix $ Lam noLoc "x" $ Fix $ SigmaE noLoc $ Pk noLoc x)

mapVec :: Bind Lang
mapVec = bind "map" (Fix $ LamList noLoc ["f", "x"] $ app2 (Fix $ VecE noLoc (VecMap noLoc)) f x)

foldVec :: Bind Lang
foldVec = bind Const.foldl (Fix $ LamList noLoc ["f", "x", "y"] $ app3 (Fix $ VecE noLoc (VecFold noLoc)) f x y)

appendVec :: Bind Lang
appendVec = bind "++" (Fix $ LamList noLoc ["x", "y"] $ Fix $ VecE noLoc $ VecAppend noLoc x y)

checkSig :: Bind Lang
checkSig = bind Const.checkSig (Fix $ LamList noLoc ["pubKey", "sigIndex"] $ Fix $ CheckSig noLoc (var' "pubKey") (var' "sigIndex"))

checkMultiSig :: Bind Lang
checkMultiSig = bind Const.checkMultiSig (Fix $ LamList noLoc ["total", "pubKeys", "sigIndices"] $ Fix $ CheckMultiSig noLoc (var' "total") (var' "pubKeys") (var' "sigIndices"))

appendText :: Bind Lang
appendText = bind "<>" (Fix $ LamList noLoc ["x", "y"] $ Fix $ TextE noLoc $ TextAppend noLoc x y)

appendBytes :: Bind Lang
appendBytes = bind "appendBytes" (Fix $ LamList noLoc ["x", "y"] $ Fix $ BytesE noLoc $ BytesAppend noLoc x y)

atVec :: Bind Lang
atVec = bind Const.listAt (Fix $ LamList noLoc ["x", "y"] $ Fix $ VecE noLoc $ VecAt noLoc x y)

fst :: Bind Lang
fst = bind "fst" (lam' "x" $ Fix $ UnOpE noLoc (TupleAt 2 0) (var' "x"))

snd :: Bind Lang
snd = bind "snd" (lam' "x" $ Fix $ UnOpE noLoc (TupleAt 2 1) (var' "x"))

otherwise :: Bind Lang
otherwise = bind "otherwise" (Fix $ PrimE noLoc $ PrimBool P.True)

undefined :: Bind Lang
undefined = bind "undefined" (Fix $ FailCase noLoc)

lam' :: Text -> Lang -> Lang
lam' name expr = Fix $ Lam noLoc (PVar noLoc $ VarName noLoc name) expr

var' :: Text -> Lang
var' name = Fix $ Var noLoc (VarName noLoc name)

f, x, y :: Lang
f = Fix $ Var noLoc "f"
x = Fix $ Var noLoc "x"
y = Fix $ Var noLoc "y"

aT, bT, cT :: Type
aT = varT "a"
bT = varT "b"
cT = varT "c"

bind :: Text -> Lang -> Bind Lang
bind var body = simpleBind (VarName noLoc var) body

