-- | Type checker for core language.
--
-- Now it works only for monomorphic types.
-- Since we test for equality but it's easy to extend for
-- polymorphic types with unification algorithm but do we really need to do it?
module Hschain.Utxo.Lang.Core.Compile.TypeCheck(
    typeCheck
  , TypeContext(..)
  , lookupSignature
  , runCheck
  -- * primitive types
  , primToType
  , primopToType
  ) where


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import Data.Foldable
import Data.Map.Strict (Map)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr (ArgType(..))

import qualified Data.Map.Strict as M
import qualified Data.Vector as V


data MonoType
  = MonoType TypeCore  -- ^ simple case when we know the type
  | AnyType            -- ^ type that can be anything
                       --    we use it for bottoms

unifyMonoType :: MonoType -> MonoType -> Check MonoType
unifyMonoType a b = case (a, b) of
  (MonoType ta, MonoType tb) -> if (ta == tb) then return a else throwError (TypeCoreMismatch ta tb)
  (AnyType, tb) -> return tb
  (ta, AnyType) -> return ta

-- | Check the types for core programm.
typeCheck :: CoreProg -> Maybe TypeCoreError
typeCheck prog = either Just (const Nothing) $
  runCheck (loadContext prog) (typeCheckM prog)


-- | Monad for the type inference.
newtype Check a = Check (ReaderT TypeContext (Either TypeCoreError) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader TypeContext, MonadError TypeCoreError)

runCheck :: TypeContext -> Check a -> Either TypeCoreError a
runCheck ctx (Check m) = runReaderT m ctx

typeCheckM :: CoreProg -> Check ()
typeCheckM (CoreProg prog) = mapM_ typeCheckScomb  prog

getSignature :: Name -> Check TypeCore
getSignature name = maybe err pure =<< asks (lookupSignature name)
  where
    err = throwError $ VarIsNotDefined name

-- | Reads  type signature of supercombinator
getScombType :: Scomb -> TypeCore
getScombType Scomb{..} = foldr (:->) res args
  where
    args = fmap typed'type $ V.toList scomb'args
    res  = typed'type scomb'body

-- | Check types for a supercombinator
typeCheckScomb :: Scomb -> Check ()
typeCheckScomb Scomb{..} =
  local (loadArgs (V.toList scomb'args)) $
    typeCheckExpr scomb'body

typeCheckExpr :: Typed TypeCore ExprCore -> Check ()
typeCheckExpr Typed{..} =
  hasType (MonoType typed'type) =<< inferExpr typed'value

hasType :: MonoType -> MonoType -> Check ()
hasType a b = do
  isOk <- fmap isMonoType $ unifyMonoType a b
  sequence_ $ liftA2 (\ta tb -> when (not isOk) $ typeCoreMismatch ta tb) (fromMonoType a) (fromMonoType b)

fromMonoType :: MonoType -> Maybe TypeCore
fromMonoType = \case
  MonoType a -> Just a
  AnyType    -> Nothing

isMonoType :: MonoType -> Bool
isMonoType x = case x of
  AnyType    -> False
  MonoType _ -> True

inferExpr :: ExprCore -> Check MonoType
inferExpr = \case
    EVar var       -> inferVar var
    EPrim prim     -> inferPrim prim
    EPrimOp op     -> MonoType <$> primopToType op
    EAp  f a       -> inferAp f a
    ELet nm e body -> inferLet nm e body
    ECase e alts   -> inferCase e alts
    -- Constructors
    EConstr (TupleT ts) 0 -> pure $ MonoType $ foldr (:->) (TupleT ts) ts
    EConstr (ListT  t)  0 -> pure $ MonoType $ ListT t
    EConstr (ListT  t)  1 -> pure $ MonoType $ t :-> ListT t :-> ListT t
    EConstr{}             -> throwError BadConstructor
    EIf c t e      -> inferIf c t e
    EBottom        -> pure AnyType

inferVar :: Name -> Check MonoType
inferVar name = getMonoType name

getMonoType :: Name -> Check MonoType
getMonoType name = fmap MonoType $ getSignature name

inferPrim :: Prim -> Check MonoType
inferPrim p = return $ MonoType $ primToType p

inferAp :: ExprCore -> ExprCore -> Check MonoType
inferAp f a = do
  fT <- inferExpr f
  aT <- inferExpr a
  getApTy fT aT
  where
    getApTy :: MonoType -> MonoType -> Check MonoType
    getApTy fT aT = do
      (farg, fres) <- getArrowTypes fT
      hasType farg aT
      return fres

    getArrowTypes :: MonoType -> Check (MonoType, MonoType)
    getArrowTypes ty = case ty of
      AnyType            -> return (AnyType, AnyType)
      MonoType (x :-> y) -> return (MonoType x, MonoType y)
      MonoType t         -> throwError $ ArrowTypeExpected t

inferLet :: Name -> ExprCore -> ExprCore -> Check MonoType
inferLet nm expr body = do
  ty <- inferExpr expr >>= \case
    MonoType ty -> pure ty
    AnyType     -> throwError PolymorphicLet
  local (loadName (Typed nm ty)) $ inferExpr body

inferCase :: ExprCore -> [CaseAlt] -> Check MonoType
inferCase expr alts = inferExpr expr >>= \case
  -- Tuple
  MonoType (TupleT ts) -> case alts of
    [ CaseAlt 0 vars e] -> do
      env <- matchTypes vars ts
      local (loadArgs env) $ inferExpr e
    _ -> throwError BadCase
  -- List
  MonoType (ListT  t) -> getResultType =<< traverse (inferListAlt t) alts
   -- Box
  MonoType  BoxT -> case alts of
    [ CaseAlt 0 [a,b,c,d] e] -> local (loadArgs [ Typed a BytesT
                                                , Typed b BytesT
                                                , Typed c IntT
                                                , Typed d argsTuple
                                                ])
                              $ inferExpr e
    _ -> throwError BadCase
  _ -> throwError BadCase
  where
    inferListAlt _  (CaseAlt 0 []     e) = inferExpr e
    inferListAlt ty (CaseAlt 1 [x,xs] e)
      = local (loadArgs [ Typed x  ty
                        , Typed xs (ListT ty)
                        ])
      $ inferExpr e
    inferListAlt _ _ = throwError BadCase
    --
    getResultType = \case
      []   -> throwError EmptyCaseExpression
      t:ts -> foldM unifyMonoType t ts

matchTypes :: [Name] -> [TypeCore] -> Check [Typed TypeCore Name]
matchTypes []     []     =  return []
matchTypes (n:ns) (t:ts) = (Typed n t:) <$> matchTypes ns ts
matchTypes  _      _     = throwError BadCase

inferIf :: ExprCore -> ExprCore -> ExprCore -> Check MonoType
inferIf c t e = do
  cT <- inferExpr c
  tT <- inferExpr t
  eT <- inferExpr e
  hasType cT (MonoType BoolT)
  unifyMonoType tT eT

-------------------------------------------------------
-- type inference context

-- | Type context of the known signatures
newtype TypeContext = TypeContext (Map Name TypeCore)
  deriving newtype (Semigroup, Monoid)

-- | Loads all user defined signatures to context
loadContext :: CoreProg -> TypeContext
loadContext (CoreProg defs) =
  foldl' (\res sc -> insertSignature (scomb'name sc) (getScombType sc) res) mempty defs

insertSignature :: Name -> TypeCore -> TypeContext -> TypeContext
insertSignature name sig (TypeContext m) =
  TypeContext $ M.insert name sig m

loadArgs :: [Typed TypeCore Name] -> TypeContext -> TypeContext
loadArgs args ctx =
  foldl' (\res arg -> loadName arg res) ctx args

loadName :: Typed TypeCore Name -> TypeContext -> TypeContext
loadName Typed{..} = insertSignature typed'value typed'type

lookupSignature :: Name -> TypeContext -> Maybe TypeCore
lookupSignature name (TypeContext m) = M.lookup name m

-------------------------------------------------------
-- constants

primToType :: Prim -> TypeCore
primToType = \case
  PrimInt   _ -> IntT
  PrimText  _ -> TextT
  PrimBool  _ -> BoolT
  PrimSigma _ -> SigmaT
  PrimBytes _ -> BytesT

primopToType :: PrimOp TypeCore -> Check TypeCore
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
  OpSigPK        -> pure $ TextT  :-> SigmaT
  OpSigBool      -> pure $ BoolT  :-> SigmaT
  OpSigAnd       -> pure $ SigmaT :-> SigmaT :-> SigmaT
  OpSigOr        -> pure $ SigmaT :-> SigmaT :-> SigmaT
  OpSigListAnd   -> pure $ ListT SigmaT :-> SigmaT
  OpSigListOr    -> pure $ ListT SigmaT :-> SigmaT
  OpSigListAll a -> pure $ (a :-> SigmaT) :-> ListT a :-> SigmaT
  OpSigListAny a -> pure $ (a :-> SigmaT) :-> ListT a :-> SigmaT
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
  OpMakeBox      -> pure $ BytesT :-> BytesT :-> IntT :-> argsTuple :-> BoxT
  --
  OpShow      ty  -> showType ty
  OpToBytes   tag -> pure $ tagToType tag :-> BytesT
  -- FIXME: Function is in fact partial
  OpFromBytes tag -> pure $ BytesT :-> (tagToType tag)
  --
  OpEnvGetHeight  -> pure IntT
  OpEnvGetSelf    -> pure BoxT
  OpEnvGetInputs  -> pure $ ListT BoxT
  OpEnvGetOutputs -> pure $ ListT BoxT
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
      TextArg  ->TextT
      BytesArg -> BytesT

compareType :: TypeCore -> Check TypeCore
compareType ty = case ty of
  IntT      -> pure r
  TextT     -> pure r
  BytesT    -> pure r
  BoolT     -> pure r
  _         -> throwError $ BadEquality ty
  where
    r = ty :-> ty :-> BoolT

showType :: TypeCore -> Check TypeCore
showType ty = case ty of
  IntT      -> pure r
  BoolT     -> pure r
  _         -> throwError $ BadEquality ty
  where
    r = ty :-> TextT
