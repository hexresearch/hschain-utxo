-- | Smartconstructors for core lang
module Hschain.Utxo.Lang.Core.Compile.Build(
    constant
  , ap
  , ap2
  , intOp2
  , boolOp2
  , sigmaOp2
  , compareOp
  , op1
  , op2
  , int
  , bool
  , sigmaBool
) where

import Data.Int
import Data.Fix

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Sigma

import qualified Data.List as L
import qualified Data.Vector as V

ap :: ExprCore -> [ExprCore] -> ExprCore
ap f args = L.foldl' (\op a -> EAp op a) f args

-- | Application of function to two arguments
ap2 :: ExprCore -> ExprCore -> ExprCore -> ExprCore
ap2 f a b = EAp (EAp f a) b

constant :: Name -> Prim -> Scomb
constant name val = Scomb
  { scomb'name = name
  , scomb'args = V.empty
  , scomb'body = Typed (EPrim val) (primToType val)
  }

op1 :: Name -> TypeCore -> TypeCore -> Scomb
op1 name argT resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList $ [Typed "x" argT]
  , scomb'body = Typed (EAp (EVar $ Typed name (arrowT argT resT)) (EVar $ Typed "x" argT)) resT
  }

intOp2 :: Name -> Scomb
intOp2 name = op2 name (intT, intT) intT

boolOp2 :: Name -> Scomb
boolOp2 name = op2 name (boolT, boolT) boolT

sigmaOp2 :: Name -> Scomb
sigmaOp2 name = op2 name (sigmaT, sigmaT) sigmaT

-- | TODO: do we need polymorphic comparison?
compareOp :: TypeCore -> Name -> Scomb
compareOp ty name = op2 name (ty, ty) boolT

op2 :: Name -> (TypeCore, TypeCore) -> TypeCore -> Scomb
op2 name (xT, yT) resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList [Typed "x" xT, Typed "y" yT]
  , scomb'body = Typed (ap2 (EVar $ Typed name (funT [xT, yT] resT)) (EVar $ Typed "x" xT) (EVar $ Typed "y" yT)) resT
  }

int :: Int64 -> ExprCore
int n = EPrim $ PrimInt n

bool :: Bool -> ExprCore
bool b = EPrim $ PrimBool b

sigmaBool :: Bool -> ExprCore
sigmaBool b = EPrim $ PrimSigma $ Fix $ SigmaBool b

