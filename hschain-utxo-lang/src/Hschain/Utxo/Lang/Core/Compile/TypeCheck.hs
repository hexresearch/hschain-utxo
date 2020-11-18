{-# LANGUAGE OverloadedStrings #-}
-- | Type checker for core language.
--
-- Now it works only for monomorphic types.
-- Since we test for equality but it's easy to extend for
-- polymorphic types with unification algorithm but do we really need to do it?
module Hschain.Utxo.Lang.Core.Compile.TypeCheck(
    typeCheck
  , runCheck
  -- * primitive types
  , primToType
  , primopToType
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Types (ArgType(..))


data MonoType
  = MonoType TypeCore  -- ^ Simple case when we know the type
  | AnyType            -- ^ Type that can be anything we use it for bottoms

unifyMonoType :: MonoType -> MonoType -> Check b v MonoType
unifyMonoType a b = case (a, b) of
  (MonoType ta, MonoType tb)
    | ta == tb  -> return a
    | otherwise -> throwError (TypeCoreMismatch ta tb)
  (AnyType, tb) -> return tb
  (ta, AnyType) -> return ta

-- | Check the types for core program.
typeCheck :: Context b v => Core b v -> Either TypeCoreError TypeCore
typeCheck prog
  = runCheck
  $ inferExpr prog >>= \case
      AnyType    -> throwError ExpressionIsBottom
      MonoType t -> return t


-- | Monad for the type inference.
newtype Check b v a = Check (ReaderT (Ctx b v TypeCore) (Either TypeCoreError) a)
  deriving newtype ( Functor, Applicative, Monad
                   , MonadReader (Ctx b v TypeCore)
                   , MonadError TypeCoreError
                   )

runCheck :: Context b v => Check b v x -> Either TypeCoreError x
runCheck (Check m) = runReaderT m emptyContext

getSignature :: Context b v => v -> Check b v TypeCore
getSignature v = maybe err pure =<< asks (flip lookupVar v)
  where
    err = throwError $ VarIsNotDefined "NAME"

inferExpr :: Context b v => Core b v -> Check b v MonoType
inferExpr = \case
    EVar  var      -> MonoType <$> getSignature var
    EPrim prim     -> pure $ MonoType $ primToType prim
    EPrimOp op     -> MonoType <$> primopToType op
    EAp  f a       -> inferAp f a
    ELam ty f      -> inferScope1 ty f >>= \case
      MonoType tyR -> pure $ MonoType $ ty :-> tyR
      AnyType      -> pure $ AnyType
    ELet  e body   -> inferLet  e body
    ECase e alts   -> inferCase e alts
    -- Constructors
    EConstr (TupleT ts) 0 -> pure $ MonoType $ foldr (:->) (TupleT ts) ts
    EConstr (ListT  t)  0 -> pure $ MonoType $ ListT t
    EConstr (ListT  t)  1 -> pure $ MonoType $ t :-> ListT t :-> ListT t
    EConstr{}             -> throwError BadConstructor
    EIf c t e      -> inferIf c t e
    EBottom        -> pure AnyType

inferScope1 :: Context b v => TypeCore -> Scope b 'One v -> Check b v MonoType
inferScope1 ty (Scope b expr)
  = local (bindOne b ty) $ inferExpr expr

inferScopeN :: Context b v => [TypeCore] -> Scope b 'Many v -> Check b v MonoType
inferScopeN types (Scope b expr) = do
  ctx' <- bindMany b types =<< ask
  local (const ctx') $ inferExpr expr


inferAp :: Context b v => Core b v -> Core b v -> Check b v MonoType
inferAp f a = do
  funTy <- inferExpr f
  aTy   <- inferExpr a
  case (funTy, aTy) of
    (MonoType (aT' :-> bT), MonoType aT)
      | aT' == aT -> pure (MonoType bT)
      | otherwise -> throwError (TypeCoreMismatch aT aT')
    (MonoType (_ :-> bT), AnyType)
      -> pure (MonoType bT)
    (MonoType t, _         ) -> throwError $ ArrowTypeExpected t
    (AnyType   , MonoType _) -> pure AnyType
    (AnyType   , AnyType   ) -> pure AnyType

inferLet :: Context b v => Core b v -> Scope b 'One v -> Check b v MonoType
inferLet expr body = do
  ty <- inferExpr expr >>= \case
    MonoType ty -> pure ty
    AnyType     -> throwError PolymorphicLet
  inferScope1 ty body

inferCase :: Context b v => Core b v -> [CaseAlt b v] -> Check b v MonoType
inferCase expr alts = inferExpr expr >>= \case
  -- Tuple
  MonoType (TupleT ts) -> case alts of
    [ CaseAlt 0 e] -> inferScopeN ts e
    _              -> throwError BadCase
  -- List
  MonoType (ListT  t) -> getResultType =<< traverse (inferListAlt t) alts
  -- Box
  MonoType  BoxT -> case alts of
    [ CaseAlt 0 e] -> inferScopeN [BytesT, BytesT, IntT, argsTuple] e
    _              -> throwError BadCase
  _ -> throwError BadCase
  where
    inferListAlt _  (CaseAlt 0 e) = inferScopeN []             e
    inferListAlt ty (CaseAlt 1 e) = inferScopeN [ty, ListT ty] e
    inferListAlt _   _            = throwError BadCase
    --
    getResultType = \case
      []   -> throwError EmptyCaseExpression
      t:ts -> foldM unifyMonoType t ts


inferIf :: Context b v => Core b v -> Core b v -> Core b v -> Check b v MonoType
inferIf c t e = do
  inferExpr c >>= \case
    AnyType        -> pure ()
    MonoType BoolT -> pure ()
    MonoType ty    -> throwError (TypeCoreMismatch ty BoolT)
  tT <- inferExpr t
  eT <- inferExpr e
  unifyMonoType tT eT


-------------------------------------------------------
-- constants

primToType :: Prim -> TypeCore
primToType = \case
  PrimInt   _ -> IntT
  PrimText  _ -> TextT
  PrimBool  _ -> BoolT
  PrimSigma _ -> SigmaT
  PrimBytes _ -> BytesT

primopToType :: PrimOp TypeCore -> Check b v TypeCore
primopToType = \case
  OpAdd -> pure $ IntT :-> IntT :-> IntT
  OpSub -> pure $ IntT :-> IntT :-> IntT
  OpMul -> pure $ IntT :-> IntT :-> IntT
  OpDiv -> pure $ IntT :-> IntT :-> IntT
  OpNeg -> pure $ IntT :-> IntT
  --
  OpBoolAnd -> pure $ BoolT :-> BoolT :-> BoolT
  OpBoolOr  -> pure $ BoolT :-> BoolT :-> BoolT
  OpBoolXor -> pure $ BoolT :-> BoolT :-> BoolT
  OpBoolNot -> pure $ BoolT :-> BoolT
  --
  OpSigPK        -> pure $ BytesT :-> SigmaT
  OpSigBool      -> pure $ BoolT  :-> SigmaT
  OpSigAnd       -> pure $ SigmaT :-> SigmaT :-> SigmaT
  OpSigOr        -> pure $ SigmaT :-> SigmaT :-> SigmaT
  OpSigListAnd   -> pure $ ListT SigmaT :-> SigmaT
  OpSigListOr    -> pure $ ListT SigmaT :-> SigmaT
  OpSigListAll a -> pure $ (a :-> SigmaT) :-> ListT a :-> SigmaT
  OpSigListAny a -> pure $ (a :-> SigmaT) :-> ListT a :-> SigmaT
  --
  OpCheckSig      -> pure $ BytesT :-> IntT :-> BoolT
  OpCheckMultiSig -> pure $ IntT :-> ListT BytesT :-> ListT IntT :-> BoolT
  --
  OpSHA256      -> pure $ BytesT :-> BytesT
  OpTextLength  -> pure $ TextT  :-> IntT
  OpTextAppend  -> pure $ TextT  :-> TextT :-> TextT
  OpBytesLength -> pure $ BytesT :-> IntT
  OpBytesAppend -> pure $ BytesT :-> BytesT :-> BytesT
  --
  OpEQ ty -> compareType ty
  OpNE ty -> compareType ty
  OpGT ty -> compareType ty
  OpGE ty -> compareType ty
  OpLT ty -> compareType ty
  OpLE ty -> compareType ty
  --
  OpArgs tag     -> pure $ ListT (tagToType tag)
  OpGetBoxId     -> pure $ BoxT :-> BytesT
  OpGetBoxScript -> pure $ BoxT :-> BytesT
  OpGetBoxValue  -> pure $ BoxT :-> IntT
  OpGetBoxArgs t -> pure $ BoxT :-> ListT (tagToType t)
  OpGetBoxPostHeight -> pure $ BoxT :-> IntT
  --
  OpShow      ty  -> showType ty
  OpToBytes   tag -> pure $ tagToType tag :-> BytesT
  -- FIXME: Function is in fact partial
  OpFromBytes tag -> pure $ BytesT :-> (tagToType tag)
  --
  OpEnvGetHeight     -> pure IntT
  OpEnvGetSelf       -> pure BoxT
  OpEnvGetInputs     -> pure $ ListT BoxT
  OpEnvGetOutputs    -> pure $ ListT BoxT
  OpEnvGetDataInputs -> pure $ ListT BoxT

  --
  OpListMap    a b -> pure $ (a :-> b) :-> ListT a :-> ListT b
  OpListAt     a   -> pure $ ListT a :-> IntT    :-> a
  OpListAppend a   -> pure $ ListT a :-> ListT a :-> ListT a
  OpListLength a   -> pure $ ListT a :-> IntT
  OpListFoldr  a b -> pure $ (a :-> b :-> b) :-> b :-> ListT a :-> b
  OpListFoldl  a b -> pure $ (b :-> a :-> b) :-> b :-> ListT a :-> b
  OpListFilter a   -> pure $ (a :-> BoolT) :-> ListT a :-> ListT a
  OpListSum        -> pure $ ListT IntT  :-> IntT
  OpListAnd        -> pure $ ListT BoolT :-> BoolT
  OpListOr         -> pure $ ListT BoolT :-> BoolT
  OpListAll    a   -> pure $ (a :-> BoolT) :-> ListT a :-> BoolT
  OpListAny    a   -> pure $ (a :-> BoolT) :-> ListT a :-> BoolT
  where
    tagToType = \case
      IntArg   -> IntT
      BoolArg  -> BoolT
      TextArg  -> TextT
      BytesArg -> BytesT

compareType :: TypeCore -> Check b v TypeCore
compareType ty = case ty of
  IntT      -> pure r
  TextT     -> pure r
  BytesT    -> pure r
  BoolT     -> pure r
  _         -> throwError $ BadEquality ty
  where
    r = ty :-> ty :-> BoolT

showType :: TypeCore -> Check b v TypeCore
showType ty = case ty of
  IntT      -> pure r
  BoolT     -> pure r
  _         -> throwError $ BadEquality ty
  where
    r = ty :-> TextT
