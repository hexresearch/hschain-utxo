-- | compilation of our language to the core-language that is
-- used in the transaction
module Hschain.Utxo.Lang.Compile(
    compile
  , toCoreScript
  , toCoreScriptUnsafe
  -- * Functions for debug
  , inlineModule
  , inferModule
  , inlinePrelude
) where

import Control.Monad

import Data.Fix
import qualified Data.Map.Strict       as Map
import qualified Data.Functor.Foldable as RS

import Hschain.Utxo.Lang.Expr hiding (Type, TypeContext)
import Hschain.Utxo.Lang.Exec.Subst (subst)
import Hschain.Utxo.Lang.Desugar.ExtendedLC
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Types          (Script(..))
import Hschain.Utxo.Lang.Compile.Infer
import Hschain.Utxo.Lang.Compile.Monomorphize
import Hschain.Utxo.Lang.Core.Types        (Typed(..), TypeCore(..), Name)
import Hschain.Utxo.Lang.Core.Compile.Expr (ExprCore, coreProgToScript)
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Lib.Base (baseLibTypeContext, baseLibExecContext)
import Hschain.Utxo.Lang.Exec.Module (trimModuleByMain)

import qualified Language.HM       as H
import qualified Language.HM.Subst as H

import qualified Hschain.Utxo.Lang.Core.Compile.Expr as Core
import qualified Hschain.Utxo.Lang.Expr as E

import qualified Data.Map.Strict as M
import qualified Data.Text as T


toCoreScript :: Module -> Either Error Script
toCoreScript m = fmap coreProgToScript $ runInferM $ compile m

toCoreScriptUnsafe :: Module -> Script
toCoreScriptUnsafe m = either (error . T.unpack . renderText) id $ toCoreScript m

-- | Compilation to Core-lang program from the script-language.
compile :: MonadLang m => Module -> m ExprCore
compile
  =  pure . substPrimOp
 <=< toCoreProg
 <=< specifyOps
 <=< inlinePolys
 <=< annotateTypes
 <=< toExtendedLC
 <=< pure . inlinePrelude
 <=< trimModuleByMain

-- | Inlines all prelude functions
inlinePrelude :: Module -> Module
inlinePrelude = inlineExecCtx baseLibExecContext

-- | Reduces all simple applications:
--
-- > (\x -> e) a ==> e[x/a]
simplifyLamApp :: Lang -> Lang
simplifyLamApp = cata $ \case
  Apply _ (Fix (Lam _ (PVar _ v) body)) expr -> subst body v expr
  Apply _ (Fix (LamList loc ((PVar _ v) : rest) body)) expr -> case rest of
    [] -> subst body v expr
    vs -> Fix $ LamList loc vs (subst body v expr)
  other -> Fix other

inlineExecCtx :: ExecCtx -> Module -> Module
inlineExecCtx (ExecCtx funs) = mapBinds (simplifyLamApp . inlineLang)
  where
    inlineLang = cata $ \case
      Var _ v              | Just expr <- M.lookup v funs -> inlineLang expr
      InfixApply loc a v b | Just expr <- M.lookup v funs -> Fix $ Apply loc (Fix $ Apply loc (inlineLang expr) a) b

      other                                  -> Fix other

-- | Function for debug. It compiles module up to inlining of polymorphic functions.
inlineModule :: Module -> TypedLamProg
inlineModule m = either (error . T.unpack . renderText) id $ runInferM $ (inlinePolys <=< annotateTypes <=< toExtendedLC <=< pure . inlinePrelude) m

-- | Function for debug. It compiles module up to type inference of all subterms.
inferModule :: Module -> TypedLamProg
inferModule m = either (error . T.unpack . renderText) id $ runInferM $ (annotateTypes <=< toExtendedLC <=< pure . inlinePrelude) m

-- | Perform sunbstiturion of primops
substPrimOp :: ExprCore -> ExprCore
substPrimOp
  = go
  where
    go = RS.cata $ \case
      Core.EVarF v
        | Just op <- Map.lookup v monoPrimopNameMap
          -> Core.EPrimOp op
      e -> RS.embed e

-- | Transforms type-annotated monomorphic program without lambda-expressions (all lambdas are lifted)
-- to Core program.
toCoreProg :: MonadLang m => TypedLamProg -> m ExprCore
toCoreProg = fromDefs . unAnnLamProg

fromDefs :: MonadLang m => [AnnComb (H.Type () Name) (Typed (H.Type () Name) Name)] -> m ExprCore
fromDefs [] = throwError $ PatError MissingMain
fromDefs (Def{..}:rest)
  | def'name == "main" = body
  | otherwise          = Core.ELet (varName'name def'name) <$> body <*> fromDefs rest
  where
    body = go def'args
      where
        go []                   = toCoreExpr def'body
        go (Typed nm ty : args) = Core.ELam nm <$> toCoreType ty <*> go args

toCoreExpr :: MonadLang m => TypedExprLam -> m ExprCore
toCoreExpr tyExpr = cataM convert tyExpr
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
        let addLet (nm, e) = Core.ELet (typed'value nm) e
        in foldr addLet body binds
      ELam _ xs body       -> toLambda xs body
      EIf _ c t e          -> pure $ Core.EIf c t e
      ECase _ e alts       -> Core.ECase e <$> traverse convertAlt alts
      EConstr _ consTy m _ -> do ty <- toCoreType consTy
                                 pure $ Core.EConstr (resultType ty) m
      EAssertType _ e _    -> pure e
      EBottom _            -> pure $ Core.EBottom

    convertAlt CaseAlt{..} = return Core.CaseAlt
      { caseAlt'args = typed'value <$> caseAlt'args
      , ..
      }
    toLambda []                  body = pure body
    toLambda (Typed x ty : vars) body = do
      e   <- toLambda vars body
      ty' <- toCoreType ty
      pure $ Core.ELam x ty' e

    typeCtx = baseLibTypeContext

resultType :: TypeCore -> TypeCore
resultType (_ :-> b) = resultType b
resultType  a        = a

-- | TODO: now we check only prelude functions.
-- But it would be great to be able for user also to write polymorphic functions.
-- We need to think on more generic rule for substitution like this.
specifyPolyFun :: MonadLang m => Loc -> E.TypeContext -> H.Type () Name -> Name -> m ExprCore
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
        Right sub  -> toPolyVar sub argOrder
        Left err   -> throwError $ TypeError $ H.setLoc loc err
    -- FIXME: what to do with polymorphic expressions?
    toPolyVar sub argOrder =
      case mapM (H.applyToVar sub) argOrder of
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
