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
  [ 
  ]
