-- | compilation of our language to the core-language that is
-- used in the transaction
module Hschain.Utxo.Lang.Compile(
    compile
  , toCoreScript
) where

import Control.Monad

import Data.Fix
import Data.Proxy
import Data.Functor.Identity
import Data.Void
import qualified Data.Map.Strict       as Map

import Hschain.Utxo.Lang.Expr hiding (Type)
import Hschain.Utxo.Lang.Desugar.ExtendedLC
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Types          (Script(..))
import Hschain.Utxo.Lang.Compile.Infer
import Hschain.Utxo.Lang.Compile.Monomorphize
import Hschain.Utxo.Lang.Core.Types        (Typed(..), TypeCore(..), Name)
import Hschain.Utxo.Lang.Core.Compile.Expr (coreProgToScript,Core)
import Hschain.Utxo.Lang.Core.Compile.TypeCheck ()
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Lib.Base (baseLibTypeContext)

import qualified Language.HM.Subst as H
import qualified Language.HM       as H

import qualified Hschain.Utxo.Lang.Core.Compile.Expr as Core

toCoreScript :: Module -> Either Error Script
toCoreScript m = fmap coreProgToScript $ runInferM $ compile m

-- | Compilation to Core-lang program from the script-language.
compile :: MonadLang m => Module -> m (Core Proxy Void) 
compile
  =  pure . Core.eraseCoreLabels
 <=< (either (throwError . FreeVariable) pure . Core.isClosed)
 <=< pure . Core.toDeBrujin
 <=< pure . substPrimOp
 <=< toCoreProg
-- <=< makeMonomorphic
 <=< specifyCompareOps
 <=< annotateTypes
 <=< toExtendedLC


-- | Perform sunbstiturion of primops
substPrimOp :: Core f Name -> Core f Name
substPrimOp = Core.substVar $ \v -> Core.EPrimOp <$> Map.lookup v monoPrimopNameMap

-- | Transforms type-annotated monomorphic program without lambda-expressions (all lambdas are lifted)
-- to Core program.
toCoreProg :: MonadLang m => TypedLamProg -> m (Core Identity Name)
toCoreProg = fromDefs . unAnnLamProg

bind1 :: Name -> Core.Core Identity Name -> Core.Scope1 Identity Name
bind1 nm = Core.Scope1 (Identity nm)

bindN :: [Name] -> Core.Core Identity Name -> Core.ScopeN Identity Name
bindN nm = Core.ScopeN (length nm) (Identity nm)

fromDefs :: MonadLang m => [AnnComb (H.Type () Name) (Typed (H.Type () Name) Name)] -> m (Core Identity Name)
fromDefs [] = throwError $ PatError MissingMain
fromDefs (Def{..}:rest)
  | def'name == "main" = body
  | otherwise          = Core.ELet <$> body <*> (bind1 (varName'name def'name) <$> fromDefs rest)
  where
    body = go def'args
      where
        go []                   = toCoreExpr def'body
        go (Typed nm ty : args) = Core.ELam <$> toCoreType ty <*> (bind1 nm <$> go args)

toCoreExpr :: MonadLang m => TypedExprLam -> m (Core Identity Name)
toCoreExpr = cataM convert
  where
    convert (Ann exprTy val) = case val of
      EVar loc name        ->
        case Map.lookup name monoPrimopNameMap of
          Just op  -> pure $ Core.EPrimOp op
          Nothing  -> specifyPolyFun loc typeCtx exprTy name
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

    typeCtx = baseLibTypeContext

resultType :: TypeCore -> TypeCore
resultType (_ :-> b) = resultType b
resultType  a        = a

-- | TODO: now we check only prelude functions.
-- But it would be great to be able for user also to write polymorphic functions.
-- We need to think on more generic rule for substitution like this.
specifyPolyFun :: MonadLang m => Loc -> TypeContext -> H.Type () Name -> Name -> m (Core Identity Name)
specifyPolyFun loc ctx ty name = do
  case H.lookupCtx name ctx of
    Just sig -> fromSignature $ H.mapLoc (const ()) sig
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
