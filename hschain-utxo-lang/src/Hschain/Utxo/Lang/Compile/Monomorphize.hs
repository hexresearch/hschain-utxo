-- | Turns polymorphic programs to monomorphic ones
module Hschain.Utxo.Lang.Compile.Monomorphize(
    specifyCompareOps
  , inlinePolys
) where

import Control.Monad
import Control.Monad.State.Strict

import Data.Bifunctor
import Data.Coerce
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe

import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.Expr      (PrimOp(..))
import Hschain.Utxo.Lang.Core.Types (Name, Typed(..))
import Hschain.Utxo.Lang.Expr(VarName(..))

import qualified Data.Map.Strict as M

import qualified Language.HM as H
import qualified Language.HM.Subst as H

-- | Substitutes polymorphic comparison operators to
-- monomorphic ones. After type checking we have precise
-- information to what operator we should specify.
-- If it still remains polymorphic we throw an error
-- that we have failed to specify the types.
specifyCompareOps :: MonadLang m => TypedLamProg -> m TypedLamProg
specifyCompareOps = liftTypedLamProg $ cataM $ \case
  Ann ty expr -> fmap (Fix . Ann ty) $ case expr of
    EVar loc name | Just op <- specPolyOp name ty -> pure $ EPrimOp loc op
    other -> pure other
  where
    specPolyOp name ty = case name of
      "listAt" -> OpListAt            <$> getLParam1 ty
      "length" -> OpListLength        <$> getLParam1 ty
      "++"     -> OpListAppend        <$> getLParam1 ty
      "any"    -> OpListAny           <$> (getArrowParam1 =<< getArrowParam1 ty)
      "all"    -> OpListAll           <$> (getArrowParam1 =<< getArrowParam1 ty)
      "map"    -> uncurry OpListMap   <$> (getArrowParam =<< getArrowParam1 ty)
      "filter" -> OpListFilter        <$> (getArrowParam1 =<< getArrowParam1 ty)
      "foldl"  -> uncurry OpListFoldl <$> foldlParam ty
      "foldr"  -> uncurry OpListFoldr <$> foldrParam ty
      "show"   -> OpShow              <$> getArrowParam1 ty
      "=="     -> OpEQ                <$> cmpParam ty
      "/="     -> OpNE                <$> cmpParam ty
      "<"      -> OpLT                <$> cmpParam ty
      "<="     -> OpLE                <$> cmpParam ty
      ">"      -> OpGT                <$> cmpParam ty
      ">="     -> OpGE                <$> cmpParam ty
      _        -> Nothing

    cmpParam = getArrowParam1

    foldlParam t = do
      (b, t2) <- getArrowParam =<< getArrowParam1 t
      a <- getArrowParam1 t2
      return (a, b)

    foldrParam t = do
      (a, t2) <- getArrowParam =<< getArrowParam1 t
      b <- getArrowParam1 t2
      return (a, b)

    getLParam1 t = getListParam =<< (fmap fst $ getArrowParam t)

    getArrowParam (H.Type (Fix ty)) = case ty of
      H.ArrowT _ a b -> Just (H.Type a, H.Type b)
      _              -> Nothing

    getListParam (H.Type (Fix ty)) = case ty of
      H.ListT _ t -> Just $ H.Type t
      _           -> Nothing

    getArrowParam1 = fmap fst . getArrowParam

--------------------------------------------------------------------
-- inline all polymorphic functions

newtype InlineCtx = InlineCtx (Map Name TypedExprLam)

type Inline a = StateT InlineCtx (Either Error) a

-- | Inlines all polymorphic functions
inlinePolys :: MonadLang m => TypedLamProg -> m TypedLamProg
inlinePolys (AnnLamProg ps) = liftEither $
  evalStateT (fmap AnnLamProg $ fmap reverse $ foldM inlineDef [] ps) (InlineCtx M.empty)

inlineDef :: [TypedDef] -> TypedDef -> Inline [TypedDef]
inlineDef res def = do
  inlDef <- inlineDefBody def
  if H.isMono $ getTypedDefType def
    then pure $ inlDef : res
    else do
      insertCtx inlDef
      return res

insertCtx :: TypedDef -> Inline ()
insertCtx def =
  modify' $ coerce $ M.insert (varName'name $ def'name def) (defBodyToLam def)

insertLetCtx :: TypedName -> TypedExprLam -> Inline ()
insertLetCtx tName rhs =
  modify' $ coerce $ M.insert (typed'value tName)  rhs

inlineDefBody :: TypedDef -> Inline TypedDef
inlineDefBody =
  local $ \defn -> fmap (\body -> defn { def'body = body }) $ inlineExpr $ def'body defn

inlineExpr :: TypedExprLam -> Inline TypedExprLam
inlineExpr = cataM $ \topVal@(Ann ty val) -> case val of
  EVar _ name -> do
    mSubst <- getSubst ty name
    return $ fromMaybe (Fix topVal) mSubst
  ELet loc binds body -> local (\b -> do
      bindsInl <- fmap reverse $ foldM inlineLetBind [] binds
      bodyInl <- inlineExpr b
      pure $ if null bindsInl
        then bodyInl
        else Fix $ Ann ty $ ELet loc bindsInl bodyInl
    ) body
  _           -> pure $ Fix topVal
  where
    inlineLetBind res (tName, rhs) = do
      rhsInl <- inlineExpr rhs
      if H.isMono (typed'type tName)
        then return $ (tName, rhsInl) : res
        else do
          insertLetCtx tName rhsInl
          return res

getSubst :: H.Type () Name -> Name -> Inline (Maybe TypedExprLam)
getSubst ty name = do
  mExpr <- fmap (\(InlineCtx m) -> M.lookup name m) get
  mapM specType mExpr
  where
    specType expr = case ty `H.subtypeOf` (ann'note $ unFix expr) of
      Right subst -> pure $ mapType (H.apply subst) expr
      Left _      -> inlineError H.defLoc $ renderText expr

local :: (a -> Inline b) -> a -> Inline b
local f a = do
  st <- get
  res <- f a
  put st
  return res

mapType :: (H.Type () Name -> H.Type () Name) -> TypedExprLam -> TypedExprLam
mapType f = cata $ \(Ann ty val) -> Fix $ Ann (f ty) $ case val of
  ELet loc binds body -> ELet loc (fmap (first mapBind) binds) body
  EPrimOp loc p -> EPrimOp loc $ fmap f p
  ELam loc bs body -> ELam loc (fmap mapBind bs) body
  EConstr loc t m n -> EConstr loc (f t) m n
  EAssertType loc a t -> EAssertType loc a (f t)
  ECase loc e alts -> ECase loc e (fmap mapAlt alts)
  other               -> other
  where
    mapBind t = t { typed'type = f (typed'type t) }

    mapAlt alt = alt
      { caseAlt'constrType = f $ caseAlt'constrType alt
      , caseAlt'args       = fmap mapBind $ caseAlt'args alt
      }

