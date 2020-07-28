-- | Smartconstructors for core lang
module Hschain.Utxo.Lang.Core.Compile.Build(
    constant
  , constantComb
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
  , text
  , bytes
  , sigmaBool
  , equals
  , toCompareName
) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Fix
import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Sigma

import qualified Data.List as L
import qualified Data.Vector as V

import qualified Language.HM as H

ap :: ExprCore -> [ExprCore] -> ExprCore
ap f args = L.foldl' (\op a -> EAp op a) f args

-- | Application of function to two arguments
ap2 :: ExprCore -> ExprCore -> ExprCore -> ExprCore
ap2 f a b = EAp (EAp f a) b

constant :: Name -> Prim -> Scomb
constant name val = constantComb name (primToType val) (EPrim val)

constantComb :: Name -> TypeCore -> ExprCore -> Scomb
constantComb name ty val = Scomb
  { scomb'name = name
  , scomb'args = V.empty
  , scomb'body = Typed val ty
  }

op1 :: Name -> TypeCore -> TypeCore -> Scomb
op1 name argT resT = Scomb
  { scomb'name = name
  , scomb'args = V.fromList $ [Typed "x" argT]
  , scomb'body = Typed (EAp (EVar name) (EVar "x" )) resT
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
  , scomb'body = Typed (ap2 (EVar name) (EVar "x") (EVar "y")) resT
  }

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
equals t a b = ap (EVar (toCompareName t "equals")) [a, b]

toCompareName :: TypeCore -> Name -> Name
toCompareName ty name = mconcat [primName ty, ".", name]
  where
    primName (H.Type (Fix x)) = case x of
      H.ConT _ prim _ -> prim
      _               -> error "Non-primitive type"


