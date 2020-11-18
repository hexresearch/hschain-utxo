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
  , getBoxIntArgs
  , getBoxTextArgs
  , getBoxByteArgs
  , getBoxBoolArgs
  , getInputs
  , getOutputs
  , getDataInputs
  , getSelf
  , getHeight
  , getIntArgs
  , getTextArgs
  , getByteArgs
  , getBoolArgs
  , checkSig
  , checkMultiSig
) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Fix
import Data.Text (Text)

import Hschain.Utxo.Lang.Types (ArgType(..))
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Sigma

import qualified Data.List as L


ap :: Core b v -> [Core b v] -> Core b v
ap f args = L.foldl' (\op a -> EAp op a) f args

primOp :: PrimOp TypeCore -> [Core b v] -> Core b v
primOp op args = ap (EPrimOp op) args

-- | Application of function to two arguments
ap2 :: Core b v -> Core b v -> Core b v -> Core b v
ap2 f a b = f `EAp` a `EAp` b

int :: Int64 -> Core b v
int n = EPrim $ PrimInt n

bool :: Bool -> Core b v
bool b = EPrim $ PrimBool b

text :: Text -> Core b v
text txt = EPrim $ PrimText txt

bytes :: ByteString -> Core b v
bytes b = EPrim $ PrimBytes b

sigmaBool :: Bool -> Core b v
sigmaBool b = EPrim $ PrimSigma $ Fix $ SigmaBool b

equals :: TypeCore -> Core b v -> Core b v -> Core b v
equals t a b = ap (EPrimOp (OpEQ t)) [a, b]

listAt :: TypeCore -> Core b v -> Core b v -> Core b v
listAt ty as n = ap (EPrimOp (OpListAt ty)) [as, n]

appendList :: TypeCore -> Core b v -> Core b v -> Core b v
appendList ty as bs = ap (EPrimOp (OpListAppend ty)) [as, bs]

mapList :: TypeCore -> TypeCore -> Core b v -> Core b v -> Core b v
mapList ta tb f as = ap (EPrimOp (OpListMap ta tb)) [f, as]

listExpr :: TypeCore -> [Core b v] -> Core b v
listExpr ty = foldr cons nil
  where
    nil      = EConstr (ListT ty) 0
    cons a b = ap (EConstr (ListT ty) 1) [a, b]

getBoxId :: Core b v -> Core b v
getBoxId = EAp (EPrimOp OpGetBoxId)

getBoxValue :: Core b v -> Core b v
getBoxValue = EAp (EPrimOp OpGetBoxValue)

getBoxPostHeight :: Core b v -> Core b v
getBoxPostHeight = EAp (EPrimOp OpGetBoxPostHeight)

getBoxScript :: Core b v -> Core b v
getBoxScript = EAp (EPrimOp OpGetBoxScript)

getBoxIntArgs,getBoxTextArgs,getBoxByteArgs,getBoxBoolArgs :: Core b v -> Core b v
getBoxIntArgs  = EAp (EPrimOp $ OpGetBoxArgs IntArg)
getBoxTextArgs = EAp (EPrimOp $ OpGetBoxArgs TextArg)
getBoxByteArgs = EAp (EPrimOp $ OpGetBoxArgs BytesArg)
getBoxBoolArgs = EAp (EPrimOp $ OpGetBoxArgs BoolArg)

getInputs,getOutputs,getSelf,getDataInputs,getHeight,getIntArgs,getTextArgs,getByteArgs,getBoolArgs :: Core b v
getInputs     = EPrimOp OpEnvGetInputs
getOutputs    = EPrimOp OpEnvGetOutputs
getSelf       = EPrimOp OpEnvGetSelf
getDataInputs = EPrimOp OpEnvGetDataInputs
getHeight     = EPrimOp OpEnvGetHeight
getIntArgs    = EPrimOp $ OpArgs IntArg
getTextArgs   = EPrimOp $ OpArgs TextArg
getByteArgs   = EPrimOp $ OpArgs BytesArg
getBoolArgs   = EPrimOp $ OpArgs BoolArg

checkSig :: Core b v -> Core b v -> Core b v
checkSig pk ix = ap (EPrimOp OpCheckSig) [pk, ix]

checkMultiSig :: Core b v -> Core b v -> Core b v -> Core b v
checkMultiSig total pks indices = ap (EPrimOp OpCheckMultiSig) [total, pks, indices]


