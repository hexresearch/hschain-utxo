-- | compilation of our language to the core-language that is
-- used in the transaction
module Hschain.Utxo.Lang.Compile(
    compile
  , toCoreScript
) where

import Control.Monad

import Data.Fix
import qualified Data.Map.Strict       as Map
import qualified Data.Functor.Foldable as RS

import Hschain.Utxo.Lang.Expr hiding (Type, TypeContext)
import Hschain.Utxo.Lang.Desugar.ExtendedLC
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Types          (Script(..))
import Hschain.Utxo.Lang.Compile.Infer
import Hschain.Utxo.Lang.Compile.Monomorphize
import Hschain.Utxo.Lang.Core.Types        (Typed(..), TypeCore(..), Name)
import Hschain.Utxo.Lang.Core.Compile.Expr (ExprCore, coreProgToScript)
-- import Hschain.Utxo.Lang.Core.Compile.TypeCheck (lookupSignature, TypeContext)
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Infer

import qualified Language.HM       as H
import qualified Language.HM.Subst as H

import qualified Hschain.Utxo.Lang.Core.Compile.Expr as Core

toCoreScript :: Module -> Either Error Script
toCoreScript m = fmap coreProgToScript $ runInferM $ compile m

-- | Compilation to Core-lang program from the script-language.
compile :: MonadLang m => Module -> m ExprCore
compile
  =  return . substPrimOp
 <=< toCoreProg
-- <=< makeMonomorphic
 <=< specifyCompareOps
 <=< annotateTypes
 <=< toExtendedLC

-- | Perform sunbstiturion of primops
substPrimOp :: ExprCore -> ExprCore
substPrimOp
  = undefined -- go
  -- where
  --   go = RS.cata $ \case
  --     Core.EVarF v
  --       | Just op <- Map.lookup v monoPrimopNameMap
  --         -> Core.EPrimOp op
  --     e -> RS.embed e

-- | Transforms type-annotated monomorphic program without lambda-expressions (all lambdas are lifted)
-- to Core program.
toCoreProg :: MonadLang m => TypedLamProg -> m ExprCore
toCoreProg = fromDefs . unAnnLamProg

bind1 :: a -> Core.Core Core.BindName a -> Core.Scope Core.BindName 'Core.One a
bind1 nm = Core.Scope (Core.BindName1 nm)

bindN :: [a] -> Core.Core Core.BindName a -> Core.Scope Core.BindName 'Core.Many a
bindN nm = Core.Scope (Core.BindNameN nm)

fromDefs :: MonadLang m => [AnnComb (H.Type () Name) (Typed (H.Type () Name) Name)] -> m ExprCore
fromDefs [] = throwError $ PatError MissingMain
fromDefs (Def{..}:rest)
  | def'name == "main" = body
  | otherwise          = Core.ELet <$> body <*> (bind1 (varName'name def'name) <$> fromDefs rest)
  where
    body = go def'args
      where
        go []                   = toCoreExpr def'body
        go (Typed nm ty : args) = Core.ELam <$> toCoreType ty <*> (bind1 nm <$> go args)

toCoreExpr :: MonadLang m => TypedExprLam -> m ExprCore
toCoreExpr = cataM convert
  where
    convert (Ann _exprTy val) = case val of
      -- FIXME: specialization of polymorphic variables is completely broken
      EVar _ name          -> pure $ Core.EVar name
      EPrim _ prim         -> pure $ Core.EPrim $ primLoc'value prim
      EPrimOp _ primOp     -> Core.EPrimOp <$> traverse toCoreType primOp
      EAp _  f a           -> pure $ Core.EAp f a
      -- FIXME: We don't take recurion between let bindings into account
      ELet _ binds body    -> pure $
        let addLet (nm, e) = Core.ELet e . bind1 (typed'value nm)
        in foldr addLet body binds
      ELam _ xs body       -> toLambda xs body
      EIf _ c t e          -> pure $ Core.EIf c t e
      ECase _ e alts       -> Core.ECase e <$> traverse convertAlt alts
      EConstr _ consTy m _ -> do ty <- toCoreType consTy
                                 pure $ Core.EConstr (resultType ty) m
      EAssertType _ e _    -> pure e
      EBottom _            -> pure $ Core.EBottom
    
    convertAlt CaseAlt{..} = return Core.CaseAlt
      { caseAlt'rhs = bindN (typed'value <$> caseAlt'args) caseAlt'rhs
      , ..
      }
    toLambda []                  body = pure body
    toLambda (Typed x ty : vars) body = do
      e   <- toLambda vars body
      ty' <- toCoreType ty
      pure $ Core.ELam ty' (bind1 x e)


resultType :: TypeCore -> TypeCore
resultType (_ :-> b) = resultType b
resultType  a        = a

{-
-- FIXME: specialization of polymorphic variables is completely broken

-- | TODO: now we check only prelude functions.
-- But it would be great to be able for user also to write polymorphic functions.
-- We need to think on more generic rule for substitution like this.
specifyPolyFun :: MonadLang m => Loc -> Map.Map Name () -> H.Type () Name -> Name -> m ExprCore
specifyPolyFun loc ctx ty name = do
  case lookupSignature name ctx of
    Just sig -> fromSignature $ H.monoT $ typeCoreToType sig
    Nothing  -> return $ Core.EVar name
  where
    fromSignature sig
      | null args && H.isMono genTy = return $ Core.EVar name
      | otherwise                   = fromPolyType genTy args
      where
        (args, genTy) = H.splitSignature sig

    fromPolyType genTy argOrder =
      case ty `H.subtypeOf` genTy of
        Right subst -> toPolyVar subst argOrder
        Left err    -> throwError $ TypeError $ H.setLoc loc err
    -- FIXME: what to do with polymorphic expressions?
    toPolyVar subst argOrder =
      case mapM (H.applyToVar subst) argOrder of
        Just _  -> failedToFindMonoType loc name
        Nothing -> failedToFindMonoType loc name
-}

toCoreType :: MonadLang m => H.Type loc Name -> m TypeCore
toCoreType (H.Type ty) = cataM go ty
  where
    -- FIXME: add sane error messages
    go = \case
      H.ArrowT _ a b      -> pure $ a :-> b
      H.VarT _ _          -> failedToFindMonoType noLoc "Type variable encountered"
      H.TupleT _ xs       -> pure $ TupleT xs
      H.ListT  _ a        -> pure $ ListT a
      H.ConT _ "Int"   [] -> pure IntT
      H.ConT _ "Bool"  [] -> pure BoolT
      H.ConT _ "Text"  [] -> pure TextT
      H.ConT _ "Bytes" [] -> pure BytesT
      H.ConT _ "Sigma" [] -> pure SigmaT
      H.ConT _ "Box"   [] -> pure BoxT
      H.ConT _ _ _        -> failedToFindMonoType noLoc "Unknown type"
