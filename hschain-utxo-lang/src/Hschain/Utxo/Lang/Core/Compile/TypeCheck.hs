module Hschain.Utxo.Lang.Core.Compile.TypeCheck(
  typeCheck
) where

-- import Language.HM (Type)

import Hschain.Utxo.Lang.Core.Compile.Expr

data TypeContext = TypeContext

-- | check the types for core programm.
typeCheck :: TypeContext -> CoreProg -> Bool
typeCheck ctx prog = all (typeCheckScomb extCtx) prog
  where
    extCtx = loadContext prog ctx

loadContext :: CoreProg -> TypeContext -> TypeContext
loadContext = undefined

-- | Check types for a supercombinator
typeCheckScomb :: TypeContext -> Scomb -> Bool
typeCheckScomb = undefined


