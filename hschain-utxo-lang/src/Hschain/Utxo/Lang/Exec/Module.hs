-- | Functions that operate on module level.
module Hschain.Utxo.Lang.Exec.Module(
    evalModule
  , checkMainModule
  , moduleToMainExpr
) where

import Hex.Common.Text

import Control.Monad.State.Strict

import Data.Either
import Data.Fix
import Data.Maybe

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Monad

import qualified Language.HM as H
import qualified Data.List.Extra as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

-- | Convert raw module data to context information that can be used
-- to evaluate expressions that depend on this module.
evalModule :: TypeContext -> Module -> Either Error ModuleCtx
evalModule typeCtx m = evalModule' typeCtx (appendRecordFuns m)

evalModule' :: TypeContext -> Module -> Either Error ModuleCtx
evalModule' typeCtx Module{..} = runInferM $ do
  binds <- InferM $ lift $ evalStateT (mapM checkBind module'binds) (typeCtx <> userTypeCtx)
  toModuleCtx binds
  where
    userTypeCtx = userTypesToTypeContext module'userTypes

    toModuleCtx :: BindGroup Lang -> InferM ModuleCtx
    toModuleCtx bs = fmap (\es -> ModuleCtx
      { moduleCtx'types = InferCtx ((H.Context $ M.fromList $ catMaybes types) <> userTypeCtx) module'userTypes
      , moduleCtx'exprs = ExecCtx (M.fromList es)
      }) exprs
      where
        types = fmap (\Bind{..} -> fmap (\ty -> (varName'name bind'name, ty)) bind'type) bs

        exprs = mapM (\Bind{..} -> fmap (bind'name, ) $ desugar module'userTypes =<< altGroupToExpr bind'alts) bs

    checkBind :: Bind Lang -> StateT TypeContext (Either Error) (Bind Lang)
    checkBind bind@Bind{..} = do
      ctx <- get
      ty <- lift $ runInferM $ inferExpr (InferCtx ctx module'userTypes) =<< altGroupToExpr bind'alts
      let typeIsOk =
            case bind'type of
              Just userTy -> if (isRight $ H.subtypeOf (H.stripSignature userTy) ty) then Nothing else (Just userTy)
              Nothing     -> Nothing
      case typeIsOk of
        Just userTy -> do
          lift $ Left $ TypeError $ H.UnifyErr (H.getLoc userTy) (H.stripSignature userTy) ty
        Nothing     -> do
          let resTy = fromMaybe (H.typeToSignature ty) bind'type
          put $ ctx <> H.Context (M.singleton (varName'name bind'name) resTy)
          return $ bind { bind'type = Just resTy }

data SelectIndex = SelectIndex
  { selectIndex'size  :: !Int
  , selectIndex'id    :: !Int
  } deriving (Show, Eq)

appendRecordFuns :: Module -> Module
appendRecordFuns m =
  m { module'binds = recordFuns ++ module'binds m }
  where
    recordFuns = selectors ++ updaters

    selectors = extractSelectors =<< types

    updaters = extractUpdaters =<< types

    types = M.elems $ userTypeCtx'types $ module'userTypes m

    onRecTypes extract UserType{..} = onField extract =<< (M.toList $ userType'cases)

    onField fromField (cons, def) = case def of
      ConsDef _ -> []
      RecordCons fields ->
        let size = V.length fields
            sel  = SelectIndex size
        in  zipWith (\n x -> fromField cons (sel n) x) [0..] $ V.toList fields

    extractSelectors = onRecTypes $ \cons index RecordField{..} ->
      simpleBind recordField'name (selectorFun cons index)

    selectorFun cons SelectIndex{..} =
      Fix $ Lam noLoc (PCons noLoc cons args) $ Fix $ Var noLoc vx
      where
        args = V.toList $ (V.replicate selectIndex'size $ PWildCard noLoc) V.// [(selectIndex'id, pvx)]
        vx   = VarName noLoc "x"
        pvx  = PVar noLoc vx

    extractUpdaters = onRecTypes $ \cons index RecordField{..} ->
      simpleBind (recordFieldUpdateFunName $ recordField'name) (updaterFun cons index)

    updaterFun cons SelectIndex{..} =
      Fix $ LamList noLoc [pvx, PCons noLoc cons inArgs] $ Fix $ Cons noLoc cons outArgs
      where
        inArgs  = V.toList $ (V.fromList $ fmap (PVar noLoc) args) V.// [(selectIndex'id, wild)]
        outArgs = (V.fromList $ fmap (Fix . Var noLoc) args) V.// [(selectIndex'id, Fix $ Var noLoc vx)]
        args    = fmap (VarName noLoc . mappend "z" . showt) [0 .. selectIndex'size - 1]

        vx   = VarName noLoc "x"
        pvx  = PVar noLoc vx
        wild = PWildCard noLoc

------------------------------------------------------------
-- compile main module

moduleToMainExpr :: MonadLang m => TypeContext -> Module -> m Lang
moduleToMainExpr typeCtx prog = do
  modCtx <- liftEither $ evalModule typeCtx prog
  return $ appendExecCtx (moduleCtx'exprs modCtx) (Fix $ Var noLoc "main")

data SimplifyCtx = SimplifyCtx
  { simplifyCtx'types :: UserTypeCtx
  }

simplifyExpr :: MonadLang m => SimplifyCtx -> Lang -> m Lang
simplifyExpr SimplifyCtx{..} expr = desugar simplifyCtx'types expr

-- | type-checks all bindings in the module.
--
-- It takes type-context for all free-variables in the module and module itself
-- and returns Nothing if module is correct or Just error in case that some types are ill defined.
checkMainModule :: TypeContext -> Module -> Maybe Error
checkMainModule types m =
  either Just (const Nothing) $
    runInferM $ do
      modCtx <- liftEither $ evalModule types m
      let ctx  = moduleCtx'types modCtx
          ctx' = ctx { inferCtx'binds = types <> inferCtx'binds ctx }
      inferExpr ctx' =<< moduleToMainExpr types m


appendExecCtx :: ExecCtx -> Lang -> Lang
appendExecCtx ctx expr =
  Fix $ Let (H.getLoc expr) binds expr
  where
    binds = fmap (\(var, rhs) -> simpleBind var rhs) $ M.toList $
      execCtx'vars $ pruneExecCtx expr ctx

-- | It removes all bindings from context that not used in the given expresiion
pruneExecCtx :: Lang -> ExecCtx -> ExecCtx
pruneExecCtx expr (ExecCtx ctx) =
  ExecCtx $ M.intersection ctx $ varsToMap vars
  where
    varsToMap vs = M.fromList $ fmap (, ()) $ S.toList vs

    vars = getUsedVars ctx (freeVars expr) S.empty

    getUsedVars m cur res
      | S.null cur = res
      | otherwise = getUsedVars (M.difference m curMap) next (cur <> res)
      where
        next = foldMap (\var -> maybe S.empty freeVars $ M.lookup var m) cur
        curMap = varsToMap cur

