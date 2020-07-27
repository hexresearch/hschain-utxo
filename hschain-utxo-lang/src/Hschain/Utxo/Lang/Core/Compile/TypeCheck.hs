-- | Type checker for core language.
--
-- Now it works only for monomorphic types.
-- Since we test for equality but it's easy to extend for
-- polymorphic types with unification algorithm but do we really need to do it?
module Hschain.Utxo.Lang.Core.Compile.TypeCheck(
    typeCheck
  , TypeContext(..)
  , getScombType
  -- * primitive types
  , intT
  , boolT
  , textT
  , bytesT
  , sigmaT
  , primT
  , boxT
  , envT
  , primToType
  , varT
  , listT
  , argsT
  , argsTypes
  , tupleT
  , arrowT
  , funT
) where


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import Data.Fix
import Data.Either
import Data.Map.Strict (Map)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Error

import qualified Data.Map.Strict as M
import qualified Data.List as L
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
typeCheck :: TypeContext -> CoreProg -> Maybe TypeCoreError
typeCheck ctx prog = either Just (const Nothing) $
  runCheck (loadContext prog ctx) (typeCheckM prog)


-- | Monad for the type inference.
newtype Check a = Check (ReaderT TypeContext (Either TypeCoreError) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader TypeContext, MonadError TypeCoreError)

runCheck :: TypeContext -> Check a -> Either TypeCoreError a
runCheck ctx (Check m) = runReaderT m ctx

typeCheckM :: CoreProg -> Check ()
typeCheckM (CoreProg prog) = mapM_ typeCheckScomb  prog

getType :: Name -> Check TypeCore
getType name = do
  TypeContext ctx <- ask
  maybe err pure $ M.lookup name ctx
  where
    err = throwError $ VarIsNotDefined name

-- | Reads  type signature of supercombinator
getScombType :: Scomb -> TypeCore
getScombType Scomb{..} = foldr (H.arrowT ()) res args
  where
    args = fmap typed'type $ V.toList scomb'args
    res  = typed'type scomb'body

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
  MonoType t -> isMono t

isMono :: TypeCore -> Bool
isMono (H.Type t) = flip cata t $ \case
  H.VarT _ _      -> False
  H.ConT _ _ as   -> and as
  H.ArrowT _ a b  -> a && b
  H.TupleT _ as   -> and as
  H.ListT _ a     -> a

inferExpr :: ExprCore -> Check MonoType
inferExpr = \case
    EVar var       -> inferVar var
    EPrim prim     -> inferPrim prim
    EAp  f a       -> inferAp f a
    ELet es e      -> inferLet es e
    ECase e alts   -> inferCase e alts
    EConstr ty _ _ -> pure $ MonoType ty
    EIf c t e      -> inferIf c t e
    EBottom        -> pure AnyType

inferVar :: Typed Name -> Check MonoType
inferVar (Typed name ty) =
  if isMonoType (MonoType ty)
    then do
      globalTy <- getType name
      if isMonoType (MonoType globalTy)
        then when (ty /= globalTy) $ typeCoreMismatch ty globalTy
        else when (isLeft $ ty `H.subtypeOf` globalTy) $ subtypeError ty globalTy
      return $ MonoType ty
    else notMonomorphicType name ty

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

inferLet :: [(Typed Name, ExprCore)] -> ExprCore -> Check MonoType
inferLet binds body = local (loadArgs (fmap fst binds)) $ do
  mapM_ (uncurry checkBind) binds
  inferExpr body
  where
    checkBind :: Typed Name -> ExprCore -> Check ()
    checkBind Typed{..} expr = do
      ty <- inferExpr expr
      hasType ty (MonoType typed'type)

inferCase :: Typed ExprCore -> [CaseAlt] -> Check MonoType
inferCase e alts = do
  checkTop e
  getResultType =<< mapM inferAlt alts
  where
    checkTop :: Typed ExprCore -> Check ()
    checkTop Typed{..} = do
      ty <- inferExpr typed'value
      hasType ty (MonoType typed'type)

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
newtype TypeContext = TypeContext (Map Name TypeCore)
  deriving newtype (Semigroup, Monoid)


-- | Loads all user defined signatures to context
loadContext :: CoreProg -> TypeContext -> TypeContext
loadContext (CoreProg defs) ctx =
  L.foldl' (\res sc -> insertSignature (scomb'name sc) (getScombType sc) res) ctx defs

insertSignature :: Name -> TypeCore -> TypeContext -> TypeContext
insertSignature name ty (TypeContext m) =
  TypeContext $ M.insert name ty m

loadArgs :: [Typed Name] -> TypeContext -> TypeContext
loadArgs args ctx =
  L.foldl' (\res arg -> loadName arg res) ctx args

loadName :: Typed Name -> TypeContext -> TypeContext
loadName Typed{..} = insertSignature typed'value typed'type

-------------------------------------------------------
-- constants

primToType :: Prim -> TypeCore
primToType = \case
  PrimInt   _ -> intT
  PrimText  _ -> textT
  PrimBool  _ -> boolT
  PrimSigma _ -> sigmaT
  PrimBytes _ -> bytesT

intT :: TypeCore
intT = primT "Int"

textT :: TypeCore
textT = primT "Text"

bytesT :: TypeCore
bytesT = primT "Bytes"

boolT :: TypeCore
boolT = primT "Bool"

sigmaT :: TypeCore
sigmaT = primT "Sigma"

boxT :: TypeCore
boxT = primT "Box"

envT :: TypeCore
envT = primT "Environment"

primT :: Name -> TypeCore
primT name = H.conT () name []

varT :: Name -> TypeCore
varT name = H.varT () name

listT :: TypeCore -> TypeCore
listT ty = H.listT () ty

tupleT :: [TypeCore] -> TypeCore
tupleT ts = H.tupleT () ts

arrowT :: TypeCore -> TypeCore -> TypeCore
arrowT a b = H.arrowT () a b

funT :: [TypeCore] -> TypeCore -> TypeCore
funT args resT = foldr arrowT resT args

argsT :: TypeCore
argsT = tupleT argsTypes

argsTypes :: [TypeCore]
argsTypes = [listT intT, listT textT, listT boolT]
