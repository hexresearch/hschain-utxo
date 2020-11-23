-- | Turns polymorphic programs to monomorphic ones
module Hschain.Utxo.Lang.Compile.Monomorphize(
  specifyCompareOps
) where


import Data.Fix

import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.Expr      (PrimOp(..))
import Hschain.Utxo.Lang.Types (argTypes)
import Hschain.Utxo.Lang.Expr(argTypeName)

import qualified Language.HM as H

-- | Substitutes polymorphic comparison operators to
-- monomorphic ones. After type checking we have precise
-- information to what operator we should specify.
-- If it still remains polymorphic we throw an error
-- that we have failed to specify the types.
specifyCompareOps :: MonadLang m => TypedLamProg -> m TypedLamProg
specifyCompareOps = liftTypedLamProg $ cataM $ \case
  Ann ty expr -> fmap (Fix . Ann ty) $ case expr of
    EVar loc "listAt" -> EPrimOp loc . OpListAt     <$> getLParam1 "listAt" loc ty
    EVar loc "length" -> EPrimOp loc . OpListLength <$> getLParam1 "length" loc ty
    EVar loc name
      | Just op <- fromCompName name -> do
          cmpT <- fromCompType name loc ty
          return $ EPrimOp loc $ op (H.mapLoc (const ()) cmpT)
    other -> pure other
  where
    fromCompName name = case name of
      "==" -> Just OpEQ
      "/=" -> Just OpNE
      "<"  -> Just OpLT
      "<=" -> Just OpLE
      ">"  -> Just OpGT
      ">=" -> Just OpGE
      _    -> Nothing

    getLParam1 name loc (H.Type (Fix ty)) = case ty of
      H.ArrowT _ (Fix (H.ListT _ t)) _ -> return (H.Type t)
      _                                -> failedToFindMonoType loc name

    fromCompType name loc (H.Type (Fix ty)) = case ty of
      H.ArrowT _ a (Fix (H.ArrowT _ b (Fix (H.ConT _ "Bool" []))))
        | isPrimType a && a == b -> pure $ H.Type a
        | H.isMono $ H.Type a     -> compareForNonPrim loc
        | otherwise              -> failedToFindMonoType loc name
      _ -> compareForNonPrim loc

    isPrimType (Fix x) = case x of
      H.ConT _ name [] -> isPrimTypeName name
      _                -> False
      where
        isPrimTypeName name = any ((name ==) . argTypeName) argTypes

