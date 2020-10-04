-- | Smartconstructors for core lang
module Hschain.Utxo.Lang.Core.Compile.Build(
    ap
  , ap2
  , int
  , bool
  , text
  , bytes
  , sigmaBool
  , equals
  , listAt
  , appendList
  , mapList
  , getBoxId
  , getBoxScript
  , getBoxValue
  , getBoxIntArgs
  , getBoxTextArgs
  , getBoxByteArgs
  , getBoxBoolArgs
  , getInputs
  , getOutputs
  , getSelf
  , getHeight
  , getIntArgs
  , getTextArgs
  , getByteArgs
  , getBoolArgs
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

getBoxId :: ExprCore -> ExprCore
getBoxId = EAp (EPrimOp OpGetBoxId)

getBoxValue :: ExprCore -> ExprCore
getBoxValue = EAp (EPrimOp OpGetBoxValue)

getBoxScript :: ExprCore -> ExprCore
getBoxScript = EAp (EPrimOp OpGetBoxScript)

getBoxIntArgs,getBoxTextArgs,getBoxByteArgs,getBoxBoolArgs :: ExprCore -> ExprCore
getBoxIntArgs  = EAp (EPrimOp $ OpGetBoxArgs IntArg)
getBoxTextArgs = EAp (EPrimOp $ OpGetBoxArgs TextArg)
getBoxByteArgs = EAp (EPrimOp $ OpGetBoxArgs BytesArg)
getBoxBoolArgs = EAp (EPrimOp $ OpGetBoxArgs BoolArg)

getInputs, getOutputs, getSelf, getHeight, getIntArgs, getTextArgs, getByteArgs, getBoolArgs :: ExprCore
getInputs   = EPrimOp OpEnvGetInputs
getOutputs  = EPrimOp OpEnvGetOutputs
getSelf     = EPrimOp OpEnvGetSelf
getHeight   = EPrimOp OpEnvGetHeight
getIntArgs  = EPrimOp $ OpArgs IntArg
getTextArgs = EPrimOp $ OpArgs TextArg
getByteArgs = EPrimOp $ OpArgs BytesArg
getBoolArgs = EPrimOp $ OpArgs BoolArg
