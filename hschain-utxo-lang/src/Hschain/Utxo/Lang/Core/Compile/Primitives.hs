{-# Language OverloadedLists #-}
-- | Built-in language primitives
module Hschain.Utxo.Lang.Core.Compile.Primitives(
    preludeLib
  , primitives
  , preludeTypeContext
) where

import Hschain.Utxo.Lang.Core.Compile.Build
  hiding (getBoxId, getBoxScript, getBoxValue, getSelf, getInputs, getOutputs)
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Const as Const

preludeLib :: CoreProg
preludeLib = CoreProg $ primitives

preludeTypeContext :: TypeContext
preludeTypeContext = primitivesCtx
  where
    primitivesCtx = TypeContext $ M.fromList $
      fmap (\sc -> (scomb'name sc, getScombSignature sc)) primitives

-- | Built-in language primitives
primitives :: [Scomb]
primitives =
  [ boxCons
  , getBoxId
  , getBoxScript
  , getBoxValue
  ]



------------------------------------------------------------
-- boxes

-- | Low level representation of Box is a tuple of four elements:
-- > (name, script, value, args)
boxCons :: Scomb
boxCons = Scomb
  { scomb'name   = "Box"
  , scomb'forall = []
  , scomb'args   = V.fromList boxArgs
  , scomb'body   = Typed (ap (EConstr consTy 0 4) $ fmap (EVar . typed'value) boxArgs) boxT
  }
  where
    consTy = funT (fmap typed'type boxArgs) boxT

getBoxField :: Name -> Name -> TypeCore -> Scomb
getBoxField name field resT = Scomb
  { scomb'name   = name
  , scomb'forall = []
  , scomb'args   = [Typed "box" boxT]
  , scomb'body   =
      Typed
        (ECase "box" [CaseAlt 0 boxArgs (EVar field)])
        resT
  }
  where

boxArgs :: [Typed Name]
boxArgs =
  [ Typed "name"   bytesT
  , Typed "script" bytesT
  , Typed "value"  intT
  , Typed "args"   argsT
  ]

getBoxId :: Scomb
getBoxId = getBoxField Const.getBoxId "name" bytesT

getBoxScript :: Scomb
getBoxScript = getBoxField Const.getBoxScript "script" bytesT

getBoxValue :: Scomb
getBoxValue = getBoxField Const.getBoxValue "value" intT
