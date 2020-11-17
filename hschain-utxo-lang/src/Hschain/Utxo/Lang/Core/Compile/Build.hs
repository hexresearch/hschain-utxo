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


ap :: ExprCore -> [ExprCore] -> ExprCore
ap f args = L.foldl' (\op a -> EAp op a) f args

primOp :: PrimOp TypeCore -> [ExprCore] -> ExprCore
primOp op args = ap (EPrimOp op) args

-- | Application of function to two arguments
ap2 :: ExprCore -> ExprCore -> ExprCore -> ExprCore
ap2 f a b = f `EAp` a `EAp` b

int :: Int64 -> ExprCore
int n = EPrim $ PrimInt n

bool :: Bool -> ExprCore
bool b = EPrim $ PrimBool b

text :: Text -> ExprCore
text txt = EPrim $ PrimText txt

bytes :: ByteString -> ExprCore
bytes b = EPrim $ PrimBytes b

sigmaBool :: Bool -> ExprCore
sigmaBool b = EPrim $ PrimSigma $ Fix $ SigmaBool b

equals :: TypeCore -> ExprCore -> ExprCore -> ExprCore
equals t a b = ap (EPrimOp (OpEQ t)) [a, b]

listAt :: TypeCore -> ExprCore -> ExprCore -> ExprCore
listAt ty as n = ap (EPrimOp (OpListAt ty)) [as, n]

appendList :: TypeCore -> ExprCore -> ExprCore -> ExprCore
appendList ty as bs = ap (EPrimOp (OpListAppend ty)) [as, bs]

mapList :: TypeCore -> TypeCore -> ExprCore -> ExprCore -> ExprCore
mapList ta tb f as = ap (EPrimOp (OpListMap ta tb)) [f, as]

listExpr :: TypeCore -> [ExprCore] -> ExprCore
listExpr ty = foldr cons nil
  where
    nil      = EConstr (ListT ty) 0
    cons a b = ap (EConstr (ListT ty) 1) [a, b]

getBoxId :: ExprCore -> ExprCore
getBoxId = EAp (EPrimOp OpGetBoxId)

getBoxValue :: ExprCore -> ExprCore
getBoxValue = EAp (EPrimOp OpGetBoxValue)

getBoxPostHeight :: ExprCore -> ExprCore
getBoxPostHeight = EAp (EPrimOp OpGetBoxPostHeight)

getBoxScript :: ExprCore -> ExprCore
getBoxScript = EAp (EPrimOp OpGetBoxScript)

getBoxIntArgs,getBoxTextArgs,getBoxByteArgs,getBoxBoolArgs :: ExprCore -> ExprCore
getBoxIntArgs  = EAp (EPrimOp $ OpGetBoxArgs IntArg)
getBoxTextArgs = EAp (EPrimOp $ OpGetBoxArgs TextArg)
getBoxByteArgs = EAp (EPrimOp $ OpGetBoxArgs BytesArg)
getBoxBoolArgs = EAp (EPrimOp $ OpGetBoxArgs BoolArg)

getInputs, getOutputs, getDataInputs, getSelf, getHeight, getIntArgs, getTextArgs, getByteArgs, getBoolArgs :: ExprCore
getInputs      = EPrimOp OpEnvGetInputs
getOutputs     = EPrimOp OpEnvGetOutputs
getSelf        = EPrimOp OpEnvGetSelf
getDataInputs  = EPrimOp OpEnvGetDataInputs
getHeight      = EPrimOp OpEnvGetHeight
getIntArgs     = EPrimOp $ OpArgs IntArg
getTextArgs    = EPrimOp $ OpArgs TextArg
getByteArgs    = EPrimOp $ OpArgs BytesArg
getBoolArgs    = EPrimOp $ OpArgs BoolArg

checkSig :: ExprCore -> ExprCore -> ExprCore
checkSig pk ix = ap (EPrimOp OpCheckSig) [pk, ix]

checkMultiSig :: ExprCore -> ExprCore -> ExprCore -> ExprCore
checkMultiSig total pks indices = ap (EPrimOp OpCheckMultiSig) [total, pks, indices]


