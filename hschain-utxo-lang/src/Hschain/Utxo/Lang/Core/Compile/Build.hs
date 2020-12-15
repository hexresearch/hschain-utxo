-- | Smartconstructors for core lang
module Hschain.Utxo.Lang.Core.Compile.Build(
    ap
  , ap2
  , primOp
  , int
  , bool
  , text
  , bytes
  , sigmaBool
  , equals
  , listExpr
  , listAt
  , appendList
  , mapList
  , getBoxId
  , getBoxScript
  , getBoxValue
  , getBoxPostHeight
  , getBoxArgs
  , getInputs
  , getOutputs
  , getDataInputs
  , getSelf
  , getHeight
  , getArgs
  , checkSig
  , checkMultiSig
) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Fix
import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Sigma

import qualified Data.List as L


ap :: Core v -> [Core v] -> Core v
ap f args = L.foldl' (\op a -> EAp op a) f args

primOp :: PrimOp TypeCore -> [Core v] -> Core v
primOp op args = ap (EPrimOp op) args

-- | Application of function to two arguments
ap2 :: Core v -> Core v -> Core v -> Core v
ap2 f a b = f `EAp` a `EAp` b

int :: Int64 -> Core v
int n = EPrim $ PrimInt n

bool :: Bool -> Core v
bool b = EPrim $ PrimBool b

text :: Text -> Core v
text txt = EPrim $ PrimText txt

bytes :: ByteString -> Core v
bytes b = EPrim $ PrimBytes b

sigmaBool :: Bool -> Core v
sigmaBool b = EPrim $ PrimSigma $ Fix $ SigmaBool b

equals :: TypeCore -> Core v -> Core v -> Core v
equals t a b = ap (EPrimOp (OpEQ t)) [a, b]

listAt :: TypeCore -> Core v -> Core v -> Core v
listAt ty as n = ap (EPrimOp (OpListAt ty)) [as, n]

appendList :: TypeCore -> Core v -> Core v -> Core v
appendList ty as bs = ap (EPrimOp (OpListAppend ty)) [as, bs]

mapList :: TypeCore -> TypeCore -> Core v -> Core v -> Core v
mapList ta tb f as = ap (EPrimOp (OpListMap ta tb)) [f, as]

listExpr :: TypeCore -> [Core v] -> Core v
listExpr ty = foldr cons nil
  where
    nil      = EConstr (ConNil ty)
    cons a b = ap (EConstr (ConCons ty)) [a, b]

getBoxId :: Core v -> Core v
getBoxId = EAp (EPrimOp OpGetBoxId)

getBoxValue :: Core v -> Core v
getBoxValue = EAp (EPrimOp OpGetBoxValue)

getBoxPostHeight :: Core v -> Core v
getBoxPostHeight = EAp (EPrimOp OpGetBoxPostHeight)

getBoxScript :: Core v -> Core v
getBoxScript = EAp (EPrimOp OpGetBoxScript)

getBoxArgs :: TypeCore -> Core v -> Core v
getBoxArgs ty = EAp (EPrimOp $ OpGetBoxArgs ty)

getInputs,getOutputs,getSelf,getDataInputs,getHeight :: Core v
getInputs     = EPrimOp OpEnvGetInputs
getOutputs    = EPrimOp OpEnvGetOutputs
getSelf       = EPrimOp OpEnvGetSelf
getDataInputs = EPrimOp OpEnvGetDataInputs
getHeight     = EPrimOp OpEnvGetHeight

getArgs :: TypeCore -> Core v
getArgs ty   = EPrimOp $ OpArgs ty

checkSig :: Core v -> Core v -> Core v
checkSig pk ix = ap (EPrimOp OpCheckSig) [pk, ix]

checkMultiSig :: Core v -> Core v -> Core v -> Core v
checkMultiSig total pks indices = ap (EPrimOp OpCheckMultiSig) [total, pks, indices]


