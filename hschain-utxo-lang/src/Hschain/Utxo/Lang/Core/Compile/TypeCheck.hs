-- | Type checker for core language.
--
-- Now it works only for monomorphic types.
-- Since we test for equality but it's easy to extend for
-- polymorphic types with unification algorithm but do we really need to do it?
module Hschain.Utxo.Lang.Core.Compile.TypeCheck(
    typeCheck
  , TypeContext(..)
  , lookupSignature
  , getScombSignature
  , runCheck
  -- * primitive types
  , primToType
  , primopToType
) where


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import Data.Fix
import Data.Foldable
import Data.Map.Strict (Map)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr (argTagToType,argsT,intT,textT,bytesT,boolT,sigmaT,boxT,listT,funT)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Language.HM as H

{- for debug
import Debug.Trace
import Text.Show.Pretty hiding (Name)

trace' :: Show a => a -> a
trace' a = trace (ppShow a) a

traceT :: Type -> Type
traceT ty = trace (show $ pretty ty) ty
-}

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

getSignature :: Name -> Check SignatureCore
getSignature name = maybe err pure =<< fmap (lookupSignature name) ask
  where
    err = throwError $ VarIsNotDefined name

-- | Reads  type signature of supercombinator
getScombType :: Scomb -> TypeCore
getScombType Scomb{..} = foldr (H.arrowT ()) res args
  where
    args = fmap typed'type $ V.toList scomb'args
    res  = typed'type scomb'body

getScombSignature :: Scomb -> SignatureCore
getScombSignature = H.monoT . getScombType

-- | Check types for a supercombinator
typeCheckScomb :: Scomb -> Check ()
typeCheckScomb Scomb{..} =
  local (loadArgs (V.toList scomb'args)) $
    typeCheckExpr scomb'body

typeCheckExpr :: Typed ExprCore -> Check ()
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
  AnyType -> False
  MonoType t -> H.isMono t

inferExpr :: ExprCore -> Check MonoType
inferExpr = \case
    EVar var       -> inferVar var
    EPrim prim     -> inferPrim prim
    EPrimOp op     -> MonoType <$> primopToType op
    EAp  f a       -> inferAp f a
    ELet nm e body -> inferLet nm e body
    ECase e alts   -> inferCase e alts
    EConstr ty _ _ -> pure $ MonoType ty
    EIf c t e      -> inferIf c t e
    EBottom        -> pure AnyType

inferVar :: Name -> Check MonoType
inferVar name = getMonoType name

getMonoType :: Name -> Check MonoType
getMonoType name =
  fmap MonoType $ (\sig -> maybe (noMonoSignature name sig) pure $ extractMonoType sig) =<< getSignature name

noMonoSignature :: Name -> SignatureCore -> Check a
noMonoSignature name x = notMonomorphicType name $ H.stripSignature x

extractMonoType :: SignatureCore -> Maybe TypeCore
extractMonoType x = flip cataM (H.unSignature x) $ \case
  H.MonoT ty      -> Just ty
  H.ForAllT _ _ _ -> Nothing

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
      AnyType -> return (AnyType, AnyType)
      MonoType (H.Type (Fix t)) ->
        case t of
          H.ArrowT () arg res -> return (MonoType $ H.Type arg, MonoType $ H.Type res)
          _                   -> throwError $ ArrowTypeExpected $ H.Type $ Fix t

inferLet :: Name -> ExprCore -> ExprCore -> Check MonoType
inferLet nm expr body = do
  ty <- inferExpr expr >>= \case
    MonoType ty -> pure ty
    AnyType     -> throwError PolymorphicLet
  local (loadName (Typed nm ty)) $ inferExpr body

inferCase :: ExprCore -> [CaseAlt] -> Check MonoType
inferCase e alts = do
  _ty <- inferExpr e
  -- FIXME: We don't use type informatio to check that patterns are
  --        correct
  getResultType =<< mapM inferAlt alts
  where
    getResultType :: [MonoType] -> Check MonoType
    getResultType = \case
      []   -> throwError EmptyCaseExpression
      t:ts -> foldM unifyMonoType t ts

inferAlt :: CaseAlt -> Check MonoType
inferAlt CaseAlt{..} =
  local (loadArgs caseAlt'args) $
    inferExpr caseAlt'rhs

inferIf :: ExprCore -> ExprCore -> ExprCore -> Check MonoType
inferIf c t e = do
  cT <- inferExpr c
  tT <- inferExpr t
  eT <- inferExpr e
  hasType cT (MonoType boolT)
  unifyMonoType tT eT

-------------------------------------------------------
-- type inference context

-- | Type context of the known signatures
newtype TypeContext = TypeContext (Map Name SignatureCore)
  deriving newtype (Semigroup, Monoid)

-- | Loads all user defined signatures to context
loadContext :: CoreProg -> TypeContext
loadContext (CoreProg defs) =
  foldl' (\res sc -> insertSignature (scomb'name sc) (getScombSignature sc) res) mempty defs

insertSignature :: Name -> SignatureCore -> TypeContext -> TypeContext
insertSignature name sig (TypeContext m) =
  TypeContext $ M.insert name sig m

loadArgs :: [Typed Name] -> TypeContext -> TypeContext
loadArgs args ctx =
  foldl' (\res arg -> loadName arg res) ctx args

loadName :: Typed Name -> TypeContext -> TypeContext
loadName Typed{..} = insertSignature typed'value (H.monoT typed'type)

lookupSignature :: Name -> TypeContext -> Maybe SignatureCore
lookupSignature name (TypeContext m) = M.lookup name m

-------------------------------------------------------
-- constants

primToType :: Prim -> TypeCore
primToType = \case
  PrimInt   _ -> intT
  PrimText  _ -> textT
  PrimBool  _ -> boolT
  PrimSigma _ -> sigmaT
  PrimBytes _ -> bytesT

primopToType :: PrimOp -> Check TypeCore
primopToType = \case
  OpAdd -> pure $ funT [intT,intT] intT
  OpSub -> pure $ funT [intT,intT] intT
  OpMul -> pure $ funT [intT,intT] intT
  OpDiv -> pure $ funT [intT,intT] intT
  OpNeg -> pure $ funT [intT]      intT
  --
  OpBoolAnd -> pure $ funT [boolT, boolT] boolT
  OpBoolOr  -> pure $ funT [boolT, boolT] boolT
  OpBoolXor -> pure $ funT [boolT, boolT] boolT
  OpBoolNot -> pure $ funT [boolT]        boolT
  --
  OpSigPK        -> pure $ funT [textT] sigmaT
  OpSigBool      -> pure $ funT [boolT] sigmaT
  OpSigAnd       -> pure $ funT [sigmaT,sigmaT] sigmaT
  OpSigOr        -> pure $ funT [sigmaT,sigmaT] sigmaT
  OpSigListAnd   -> pure $ funT [listT sigmaT] sigmaT
  OpSigListOr    -> pure $ funT [listT sigmaT] sigmaT
  OpSigListAll a -> pure $ funT [funT [a] sigmaT, listT a] sigmaT
  OpSigListAny a -> pure $ funT [funT [a] sigmaT, listT a] sigmaT
  --
  OpSHA256      -> pure $ funT [bytesT]         bytesT
  OpTextLength  -> pure $ funT [textT]          intT
  OpTextAppend  -> pure $ funT [textT,  textT]  textT
  OpBytesLength -> pure $ funT [bytesT]         intT
  OpBytesAppend -> pure $ funT [bytesT, bytesT] bytesT
  --
  OpEQ ty -> compareType ty
  OpNE ty -> compareType ty
  OpGT ty -> compareType ty
  OpGE ty -> compareType ty
  OpLT ty -> compareType ty
  OpLE ty -> compareType ty
  --
  OpArgs tag     -> pure $ listT (tagToType tag)
  OpGetBoxId     -> pure $ funT [boxT] bytesT
  OpGetBoxScript -> pure $ funT [boxT] bytesT
  OpGetBoxValue  -> pure $ funT [boxT] intT
  OpGetBoxArgs t -> pure $ funT [ boxT ] (listT $ tagToType t)
  OpMakeBox      -> pure $ funT [bytesT, bytesT, intT, argsT] boxT
  --
  OpShow      ty  -> showType ty
  OpToBytes   tag -> pure $ funT [tagToType tag] bytesT
  -- FIXME: Function is in fact partial
  OpFromBytes tag -> pure $ funT [bytesT] (tagToType tag)
  --
  OpEnvGetHeight  -> pure intT
  OpEnvGetSelf    -> pure boxT
  OpEnvGetInputs  -> pure $ listT boxT
  OpEnvGetOutputs -> pure $ listT boxT
  --
  OpListMap    a b -> pure $ funT [ funT [a] b , listT a ] (listT b)
  OpListAt     a   -> pure $ funT [ listT a, intT ] a
  OpListAppend a   -> pure $ funT [ listT a, listT a ] (listT a)
  OpListLength a   -> pure $ funT [ listT a ] intT
  OpListFoldr  a b -> pure $ funT [ funT [a, b] b
                                  , b
                                  , listT a
                                  ] b
  OpListFoldl  a b -> pure $ funT [ funT [b, a] b
                                  , b
                                  , listT a
                                  ] b
  OpListFilter a   -> pure $ funT [ funT [a] boolT, listT a] (listT a)
  OpListSum        -> pure $ funT [ listT intT ] intT
  OpListAnd        -> pure $ funT [ listT boolT ] boolT
  OpListOr         -> pure $ funT [ listT boolT ] boolT
  OpListAndSigma   -> pure $ funT [ listT sigmaT ] sigmaT
  OpListOrSigma    -> pure $ funT [ listT sigmaT ] sigmaT
  OpListAll    a   -> pure $ funT [ funT [a] boolT, listT a ] boolT
  OpListAny    a   -> pure $ funT [ funT [a] boolT, listT a ] boolT
  OpListNil    a   -> pure $ listT a
  OpListCons   a   -> pure $ funT [ a , listT a ] (listT a)
  where
    tagToType = H.mapLoc (const ()) . argTagToType

compareType :: TypeCore -> Check TypeCore
compareType ty
  | ty == intT   = pure r
  | ty == textT  = pure r
  | ty == bytesT = pure r
  | ty == boolT  = pure r
  | otherwise    = throwError $ BadEquality ty
  where
    r = funT [ty,ty] boolT

showType :: TypeCore -> Check TypeCore
showType ty
  | ty == intT  = pure r
  | ty == boolT = pure r
  | otherwise   = throwError $ BadEquality ty
  where
    r = funT [ty] textT
