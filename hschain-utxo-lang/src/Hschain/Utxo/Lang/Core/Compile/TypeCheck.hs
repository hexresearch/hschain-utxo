{-# OPTIONS_GHC -Wno-orphans #-}
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
  , sigmaT
  , primT
  , boxT
  , envT
  , primToType
  , varT
  , listT
  , tupleT
  , arrowT
  , funT
) where

import Control.Monad.Reader

import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import Language.HM (IsVar, stringIntToVar, stringPrettyLetters)
import Language.HM.Pretty (PrintCons(..), HasPrefix(..))

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Vector as V

import qualified Language.HM as H

import Data.Text.Prettyprint.Doc

{- for debug
import Debug.Trace
import Text.Show.Pretty hiding (Name)

trace' :: Show a => a -> a
trace' a = trace (ppShow a) a

traceT :: Type -> Type
traceT ty = trace (show $ pretty ty) ty
-}

data MonoType
  = MonoType Type  -- ^ simple case when we know the type
  | AnyType        -- ^ type that can be anything
                   --    we use it for bottoms

unifyMonoType :: MonoType -> MonoType -> Maybe MonoType
unifyMonoType a b = case (a, b) of
  (MonoType ta, MonoType tb) -> if (ta == tb) then Just a else Nothing
  (AnyType, tb) -> Just tb
  (ta, AnyType) -> Just ta

instance IsVar Name where
  intToVar = stringIntToVar
  prettyLetters = stringPrettyLetters

instance HasPrefix Name where
  getFixity = const Nothing

instance PrintCons Name where
  printCons name args = hsep $ pretty name : args

-- | Check the types for core programm.
typeCheck :: TypeContext -> CoreProg -> Bool
typeCheck ctx prog = fromMaybe False $
  runCheck (loadContext prog ctx) (typeCheckM prog)

-- | Monad for the type inference.
type Check a = ReaderT TypeContext Maybe a

runCheck :: TypeContext -> Check a -> Maybe a
runCheck ctx m = runReaderT m ctx

typeCheckM :: CoreProg -> Check Bool
typeCheckM (CoreProg prog) = fmap and $ mapM typeCheckScomb  prog

getType :: Name -> Check Type
getType name = do
  TypeContext ctx <- ask
  lift $ M.lookup name ctx

-- | Reads  type signature of supercombinator
getScombType :: Scomb -> Type
getScombType Scomb{..} = foldr (H.arrowT ()) res args
  where
    args = fmap typed'type $ V.toList scomb'args
    res  = typed'type scomb'body

-- | Check types for a supercombinator
typeCheckScomb :: Scomb -> Check Bool
typeCheckScomb Scomb{..} =
  local (loadArgs (V.toList scomb'args)) $
    typeCheckExpr scomb'body

typeCheckExpr :: Typed Expr -> Check Bool
typeCheckExpr Typed{..} =
  fmap (hasType (MonoType typed'type)) $ inferExpr typed'value

hasType :: MonoType -> MonoType -> Bool
hasType a b = maybe False isMonoType $ unifyMonoType a b

fromMonoType :: MonoType -> Maybe Type
fromMonoType = \case
  MonoType a -> Just a
  AnyType    -> Nothing

isMonoType :: MonoType -> Bool
isMonoType x = isJust $ fromMonoType x

inferExpr :: Expr -> Check MonoType
inferExpr = \case
    EVar var       -> inferVar var
    EPrim prim     -> inferPrim prim
    EAp  f a       -> inferAp f a
    ELet es e      -> inferLet es e
    ECase e alts   -> inferCase e alts
    EConstr ty _ _ -> pure $ MonoType ty
    EIf c t e      -> inferIf c t e
    EBottom        -> pure AnyType

inferVar :: Name -> Check MonoType
inferVar name = fmap MonoType $ getType name

inferPrim :: Prim -> Check MonoType
inferPrim p = return $ MonoType $ primToType p

inferAp :: Expr -> Expr -> Check MonoType
inferAp f a = do
  fT <- inferExpr f
  aT <- inferExpr a
  lift $ getApTy fT aT
  where
    getApTy :: MonoType -> MonoType -> Maybe MonoType
    getApTy fT aT = do
      (farg, fres) <- getArrowTypes fT
      guard $ hasType farg aT
      return fres

    getArrowTypes :: MonoType -> Maybe (MonoType, MonoType)
    getArrowTypes ty = case ty of
      AnyType -> Just (AnyType, AnyType)
      MonoType (H.Type (Fix t)) ->
        case t of
          H.ArrowT () arg res -> Just (MonoType $ H.Type arg, MonoType $ H.Type res)
          _                   -> Nothing

inferLet :: [(Typed Name, Expr)] -> Expr -> Check MonoType
inferLet binds body = local (loadArgs (fmap fst binds)) $ do
  mapM_ (uncurry checkBind) binds
  inferExpr body
  where
    checkBind :: Typed Name -> Expr -> Check ()
    checkBind Typed{..} expr = do
      ty <- inferExpr expr
      guard $ hasType ty (MonoType typed'type)

inferCase :: Typed Expr -> [CaseAlt] -> Check MonoType
inferCase e alts = do
  checkTop e
  getResultType =<< mapM inferAlt alts
  where
    checkTop :: Typed Expr -> Check ()
    checkTop Typed{..} = do
      ty <- inferExpr typed'value
      guard $ hasType ty (MonoType typed'type)

    getResultType :: [MonoType] -> Check MonoType
    getResultType = \case
      []   -> lift Nothing
      t:ts -> do
        lift $ L.foldl' (\a b -> unifyMonoType b =<< a) (Just t) ts

inferAlt :: CaseAlt -> Check MonoType
inferAlt CaseAlt{..} =
  local (loadArgs caseAlt'args) $
    inferExpr caseAlt'rhs

inferIf :: Expr -> Expr -> Expr -> Check MonoType
inferIf c t e = do
  cT <- inferExpr c
  tT <- inferExpr t
  eT <- inferExpr e
  guard $ hasType cT (MonoType boolT)
  lift $ unifyMonoType tT eT

-------------------------------------------------------
-- type inference context

-- | Type context of the known signatures
newtype TypeContext = TypeContext (Map Name Type)

instance Semigroup TypeContext where
  (TypeContext a) <> (TypeContext b) = TypeContext (a <> b)

instance Monoid TypeContext where
  mempty = TypeContext mempty

-- | Loads all user defined signatures to context
loadContext :: CoreProg -> TypeContext -> TypeContext
loadContext (CoreProg defs) ctx =
  L.foldl' (\res sc -> insertSignature (scomb'name sc) (getScombType sc) res) ctx defs

insertSignature :: Name -> Type -> TypeContext -> TypeContext
insertSignature name ty (TypeContext m) =
  TypeContext $ M.insert name ty m

loadArgs :: [Typed Name] -> TypeContext -> TypeContext
loadArgs args ctx =
  L.foldl' (\res arg -> loadName arg res) ctx args

loadName :: Typed Name -> TypeContext -> TypeContext
loadName Typed{..} = insertSignature typed'value typed'type

-------------------------------------------------------
-- constants

primToType :: Prim -> Type
primToType = \case
  PrimInt   _ -> intT
  PrimText  _ -> textT
  PrimBool  _ -> boolT
  PrimSigma _ -> sigmaT

intT :: Type
intT = primT "Int"

textT :: Type
textT = primT "Text"

boolT :: Type
boolT = primT "Bool"

sigmaT :: Type
sigmaT = primT "Sigma"

boxT :: Type
boxT = primT "Box"

envT :: Type
envT = primT "Environment"

primT :: Name -> Type
primT name = H.conT () name []

varT :: Name -> Type
varT name = H.varT () name

listT :: Type -> Type
listT ty = H.listT () ty

tupleT :: [Type] -> Type
tupleT ts = H.tupleT () ts

arrowT :: Type -> Type -> Type
arrowT a b = H.arrowT () a b

funT :: [Type] -> Type -> Type
funT args resT = foldr arrowT resT args
