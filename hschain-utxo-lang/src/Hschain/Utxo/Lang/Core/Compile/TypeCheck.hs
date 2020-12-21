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

import qualified Data.Vector as V


data MonoType
  = MonoType TypeCore  -- ^ Simple case when we know the type
  | AnyType            -- ^ Type that can be anything we use it for bottoms

unifyMonoType :: MonoType -> MonoType -> Check v MonoType
unifyMonoType a b = case (a, b) of
  (MonoType ta, MonoType tb)
    | ta == tb  -> return a
    | otherwise -> throwError (TypeCoreMismatch ta tb)
  (AnyType, tb) -> return tb
  (ta, AnyType) -> return ta

-- | Check the types for core program.
typeCheck :: Core v -> Either TypeCoreError TypeCore
typeCheck prog
  = runCheck
  $ inferExpr prog >>= \case
      AnyType    -> throwError ExpressionIsBottom
      MonoType t -> return t


-- | Monad for the type inference.
newtype Check v a = Check (ReaderT [TypeCore] (Either TypeCoreError) a)
  deriving newtype ( Functor, Applicative, Monad
                   , MonadReader [TypeCore]
                   , MonadError TypeCoreError
                   )

runCheck :: Check v x -> Either TypeCoreError x
runCheck (Check m) = runReaderT m []

getSignature :: Int -> Check v TypeCore
getSignature v = maybe err pure =<< asks (flip lookupVar v)
  where
    err = throwError $ VarIsNotDefined "NAME"

inferExpr :: Core v -> Check v MonoType
inferExpr = \case
    EVar  _        -> throwError $ VarIsNotDefined "Free variable without type"
    BVar  i        -> MonoType <$> getSignature i
    EPrim prim     -> pure $ MonoType $ primToType prim
    EPrimOp op     -> MonoType <$> primopToType op
    EAp  f a       -> inferAp f a
    ELam ty f      -> inferScope1 ty f >>= \case
      MonoType tyR -> pure $ MonoType $ ty :-> tyR
      AnyType      -> pure $ AnyType
    ELet  e body   -> inferLet  e body
    ECase e alts   -> inferCase e alts
    EConstr con    -> fmap MonoType $ maybe (throwError BadConstructor) pure $ conCoreType con
    EIf c t e      -> inferIf c t e
    EBottom        -> pure AnyType

inferScope1 :: TypeCore -> Core v -> Check v MonoType
inferScope1 ty expr
  = local (ty:) $ inferExpr expr

inferScopeN :: Int -> [TypeCore] -> Core v -> Check v MonoType
inferScopeN n types expr = do
  when (n /= length types) $ do
    throwError BadCase
  local (types <>) $ inferExpr expr


inferAp :: Core v -> Core v -> Check v MonoType
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

inferLet :: Core v -> Core v -> Check v MonoType
inferLet expr body = do
  ty <- inferExpr expr >>= \case
    MonoType ty -> pure ty
    AnyType     -> throwError PolymorphicLet
  inferScope1 ty body

inferCase :: Core v -> [CaseAlt v] -> Check v MonoType
inferCase expr alts = inferExpr expr >>= \case
  -- Tuple
  MonoType (TupleT tsA) -> case alts of
    [ CaseAlt (ConTuple tsB) n e] | tsA == V.toList tsB -> inferScopeN n tsA e
    _                                                -> throwError BadCase
  -- List
  MonoType (ListT  t) -> getResultType =<< traverse (inferListAlt t) alts
  -- Box
  MonoType  BoxT -> case alts of
    [ CaseAlt con n e] | con == boxPrimCon -> inferScopeN n [BytesT, BytesT, IntT, argsTuple] e
    _                                      -> throwError BadCase
  -- Maybe
  MonoType (MaybeT t) -> getResultType =<< traverse (inferMaybeAlt t) alts
  -- Unit
  MonoType UnitT -> getResultType =<< traverse inferUnitAlt alts
  -- Sum
  MonoType (SumT tsA) -> case alts of
    [ CaseAlt (ConSum idx tsB) n e] | tsA == V.toList tsB -> case tsB V.!? idx of
        Just ty -> inferScopeN n [ty] e
        Nothing -> throwError BadCase
    _ -> throwError BadCase
  _ -> throwError BadCase
  where
    inferListAlt ty (CaseAlt (ConNil tB) n e)  | ty == tB = inferScopeN n [] e
    inferListAlt ty (CaseAlt (ConCons tB) n e) | ty == tB = inferScopeN n [ty, ListT ty] e
    inferListAlt _   _                                    = throwError BadCase
    --
    getResultType = \case
      []   -> throwError EmptyCaseExpression
      t:ts -> foldM unifyMonoType t ts

    inferMaybeAlt ty = \case
      CaseAlt (ConNothing tB) n e | ty == tB -> inferScopeN n [] e
      CaseAlt (ConJust tB) n e    | ty == tB -> inferScopeN n [ty] e
      _                                      -> throwError BadCase

    inferUnitAlt = \case
      CaseAlt ConUnit n e -> inferScopeN n [] e
      _                   -> throwError BadCase






inferIf :: Core v -> Core v -> Core v -> Check v MonoType
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

primopToType :: PrimOp TypeCore -> Check v TypeCore
primopToType = \case
  OpAdd -> pure $ IntT :-> IntT :-> IntT
  OpSub -> pure $ IntT :-> IntT :-> IntT
  OpMul -> pure $ IntT :-> IntT :-> IntT
  OpDiv -> pure $ IntT :-> IntT :-> IntT
  OpNeg -> pure $ IntT :-> IntT
  --
  OpBoolAnd -> pure $ BoolT :-> BoolT :-> BoolT
  OpBoolOr  -> pure $ BoolT :-> BoolT :-> BoolT
  OpBoolNot -> pure $ BoolT :-> BoolT
  --
  OpSigPK        -> pure $ BytesT :-> SigmaT
  OpSigDTuple    -> pure $ BytesT :-> BytesT :-> BytesT :-> SigmaT
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
  OpArgs t       -> pure $ t
  OpGetBoxId     -> pure $ BoxT :-> TupleT [BytesT,IntT]
  OpGetBoxScript -> pure $ BoxT :-> BytesT
  OpGetBoxValue  -> pure $ BoxT :-> IntT
  OpGetBoxArgs t -> pure $ BoxT :-> t
  OpGetBoxPostHeight -> pure $ BoxT :-> IntT
  --
  OpShow      ty  -> showType ty
  OpToBytes   ty  -> pure $ ty :-> BytesT
  -- FIXME: Function is in fact partial
  OpFromBytes ty  -> pure $ BytesT :-> ty
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
  OpListProduct    -> pure $ ListT IntT  :-> IntT
  OpListAnd        -> pure $ ListT BoolT :-> BoolT
  OpListOr         -> pure $ ListT BoolT :-> BoolT
  OpListAll    a   -> pure $ (a :-> BoolT) :-> ListT a :-> BoolT
  OpListAny    a   -> pure $ (a :-> BoolT) :-> ListT a :-> BoolT

compareType :: TypeCore -> Check v TypeCore
compareType ty = case ty of
  IntT      -> pure r
  TextT     -> pure r
  BytesT    -> pure r
  BoolT     -> pure r
  _         -> throwError $ BadEquality ty
  where
    r = ty :-> ty :-> BoolT

showType :: TypeCore -> Check v TypeCore
showType ty = case ty of
  IntT      -> pure r
  BoolT     -> pure r
  _         -> throwError $ BadEquality ty
  where
    r = ty :-> TextT
