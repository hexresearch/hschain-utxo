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
import Data.List (elemIndex)
import Data.Void
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

import Hschain.Utxo.Lang.Exec.Subst (subst)
import Hschain.Utxo.Lang.Expr hiding (Type)
import Hschain.Utxo.Lang.Desugar.ExtendedLC
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Types          (Script(..))
import Hschain.Utxo.Lang.Compile.Infer
import Hschain.Utxo.Lang.Compile.Monomorphize
import Hschain.Utxo.Lang.Core.Types        (Typed(..), TypeCore(..), Name)
import Hschain.Utxo.Lang.Core.Compile.Expr (Core,coreProgToScript)
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Lib.Base (baseLibTypeContext, baseLibExecCtx)
import Hschain.Utxo.Lang.Exec.Module (trimModuleByMain)

import qualified Type.Check.HM       as H
import qualified Type.Check.HM.Subst as H

import qualified Hschain.Utxo.Lang.Core.Compile.Expr as Core

toCoreScript :: Module -> Either Error Script
toCoreScript m = fmap coreProgToScript $ runInferM $ compile m

toCoreScriptUnsafe :: Module -> Script
toCoreScriptUnsafe m = either (error . T.unpack . renderText) id $ toCoreScript m

-- | Compilation to Core-lang program from the script-language.
compile :: MonadLang m => Module -> m (Core Void)
compile
  =  toCoreProg
 <=< specifyOps
 <=< inlinePolys
 <=< annotateTypes
 <=< toExtendedLC
 <=< pure . inlinePrelude
 <=< trimModuleByMain

-- | Inlines all prelude functions
inlinePrelude :: Module -> Module
inlinePrelude = inlineExecCtx baseLibExecCtx

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
      Var _ v              | Just expr <- Map.lookup v funs -> inlineLang expr
      InfixApply loc a v b | Just expr <- Map.lookup v funs -> Fix $ Apply loc (Fix $ Apply loc (inlineLang expr) a) b
      other                                  -> Fix other

-- | Function for debug. It compiles module up to inlining of polymorphic functions.
inlineModule :: Module -> TypedLamProg
inlineModule m = either (error . T.unpack . renderText) id $ runInferM $ (inlinePolys <=< annotateTypes <=< toExtendedLC <=< pure . inlinePrelude) m

-- | Function for debug. It compiles module up to type inference of all subterms.
inferModule :: Module -> TypedLamProg
inferModule m = either (error . T.unpack . renderText) id $ runInferM $ (annotateTypes <=< toExtendedLC <=< pure . inlinePrelude) m

-- | Transforms type-annotated monomorphic program without lambda-expressions (all lambdas are lifted)
-- to Core program.
toCoreProg :: MonadLang m => TypedLamProg -> m (Core Void)
toCoreProg = fromDefs . unAnnLamProg

-- bind1 :: Name -> Core.Core Identity Name -> Core.Scope1 Identity Name
-- bind1 nm = Core.Scope1 (Identity nm)

-- bindN :: [Name] -> Core.Core Identity Name -> Core.ScopeN Identity Name
-- bindN nm = Core.ScopeN (length nm) (Identity nm)

fromDefs :: MonadLang m => [AnnComb (H.Type () Name) (Typed (H.Type () Name) Name)] -> m (Core Void)
fromDefs = recur []
  where
    recur _   [] = throwError $ PatError MissingMain
    recur ctx (defn : rest)
      | def'name defn == "main" = body ctx defn
      | otherwise               = Core.ELet <$> body ctx defn
                                            <*> recur (varName'name (def'name defn) : ctx) rest
    --
    body ctx0 Def{..} = go ctx0 def'args
      where
        go ctx []                   = toCoreExpr ctx def'body
        go ctx (Typed nm ty : args) = Core.ELam <$> toCoreType ty <*> go (nm : ctx) args

toCoreExpr :: MonadLang m => [Name] -> TypedExprLam -> m (Core Void)
toCoreExpr = go
  where
    go ctx (Fix (Ann exprTy val)) = case val of
      EVar loc name
        | Just i <- elemIndex name ctx
          -> pure $ Core.BVar i
        | Just op <- Map.lookup name monoPrimopNameMap
          -> pure $ Core.EPrimOp op
        | otherwise
          -> specifyPolyFun loc typeCtx exprTy name
      EPrim _ prim         -> pure $ Core.EPrim $ primLoc'value prim
      EPrimOp _ primOp     -> Core.EPrimOp <$> traverse toCoreType primOp
      EAp _  f a           -> Core.EAp <$> go ctx f <*> go ctx a
      ELet _ exprs body    -> toLet    ctx exprs body
      ELam _ xs body       -> toLambda ctx xs body
      EIf _ c t e          -> Core.EIf <$> go ctx c <*> go ctx t <*> go ctx e
      ECase _ e alts       -> Core.ECase <$> go ctx e <*> traverse (convertAlt ctx) alts
      EConstr _ m          -> do m' <- mapM toCoreType m
                                 pure $ Core.EConstr m'
      EAssertType _ e _    -> go ctx e
      EBottom _            -> pure $ Core.EBottom

    convertAlt ctx CaseAlt{..} = do
      e <- go (caseAlt'args <> ctx) caseAlt'rhs
      tag <- mapM toCoreType caseAlt'tag
      pure Core.CaseAlt
        { caseAlt'nVars = length caseAlt'args
        , caseAlt'rhs   = e
        , caseAlt'tag   = tag
        , ..
        }
    toLet ctx [] body = go ctx body
    toLet ctx ((Typed x _, expr) : rest) body = do
      expr' <- go ctx expr
      body' <- toLet (x : ctx) rest body
      pure $ Core.ELet expr' body'
    --
    toLambda ctx []                  body = go ctx body
    toLambda ctx (Typed x ty : vars) body = do
      e   <- toLambda (x : ctx) vars body
      ty' <- toCoreType ty
      pure $ Core.ELam ty' e

    typeCtx = baseLibTypeContext

-- | TODO: now we check only prelude functions.
-- But it would be great to be able for user also to write polymorphic functions.
-- We need to think on more generic rule for substitution like this.
specifyPolyFun :: MonadLang m => Loc -> TypeContext -> H.Type () Name -> Name -> m (Core Void)
specifyPolyFun loc ctx ty name = do
  case H.lookupCtx name ctx of
    Just sig -> fromSignature $ H.mapLoc (const ()) sig
    Nothing  -> throwError $ FreeVariable name
  where
    fromSignature sig
      | null args && H.isMono genTy = throwError $ FreeVariable name
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


toCoreType :: MonadLang m => H.Type () Name -> m TypeCore
toCoreType (H.Type ty) = cataM go ty
  where
    -- FIXME: add sane error messages
    go = \case
      H.ArrowT _ a b       -> pure $ a :-> b
      H.VarT _ _           -> failedToFindMonoType noLoc "Type variable encountered"
      H.TupleT _ xs        -> pure $ TupleT xs
      H.ListT  _ a         -> pure $ ListT a
      H.ConT _ "Int"   []  -> pure IntT
      H.ConT _ "Bool"  []  -> pure BoolT
      H.ConT _ "Text"  []  -> pure TextT
      H.ConT _ "Bytes" []  -> pure BytesT
      H.ConT _ "Sigma" []  -> pure SigmaT
      H.ConT _ "Box"   []  -> pure BoxT
      H.ConT _ "Unit"  []  -> pure UnitT
      H.ConT _ "Maybe" [a] -> pure $ MaybeT a
      H.ConT _  con ts | "Sum" `T.isPrefixOf` con -> pure $ SumT ts
      H.ConT _ _ _         -> failedToFindMonoType noLoc "Unknown type"
