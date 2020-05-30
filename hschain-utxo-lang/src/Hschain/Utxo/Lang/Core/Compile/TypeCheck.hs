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
) where

import Control.Monad.Reader

import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Vector as V

import qualified Language.HM as H

-- | Check the types for core programm.
typeCheck :: TypeContext -> CoreProg -> Bool
typeCheck ctx prog = fromMaybe False $
  runCheck (loadContext prog ctx) (typeCheckM prog)

-- | Monad for the type inference.
type Check a = ReaderT TypeContext Maybe a

runCheck :: TypeContext -> Check a -> Maybe a
runCheck ctx m = runReaderT m ctx

typeCheckM :: CoreProg -> Check Bool
typeCheckM prog = fmap and $ mapM typeCheckScomb  prog

getType :: Name -> Check Type
getType name = do
  TypeContext ctx <- ask
  lift $ M.lookup name ctx

getScombType :: Scomb -> Type
getScombType Scomb{..} = L.foldl' (H.arrowT ()) res args
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
  fmap (== typed'type) $ inferExpr typed'value

inferExpr :: Expr -> Check Type
inferExpr = \case
    EVar var       -> inferVar var
    EPrim prim     -> inferPrim prim
    EAp  f a       -> inferAp f a
    ELet es e      -> inferLet es e
    ECase e alts   -> inferCase e alts
    EConstr ty _ _ -> pure ty

inferVar :: Name -> Check Type
inferVar = getType

inferPrim :: Prim -> Check Type
inferPrim p = return $ primToType p

inferAp :: Expr -> Expr -> Check Type
inferAp f a = do
  fT <- inferExpr f
  aT <- inferExpr a
  lift $ getApTy fT aT
  where
    getApTy :: Type -> Type -> Maybe Type
    getApTy fT aT = do
      (farg, fres) <- getArrowTypes fT
      guard $ farg == aT
      return fres

    getArrowTypes :: Type -> Maybe (Type, Type)
    getArrowTypes (H.Type (Fix t)) =
      case t of
        H.ArrowT () arg res -> Just (H.Type arg, H.Type res)
        _                   -> Nothing

inferLet :: [(Typed Name, Expr)] -> Expr -> Check Type
inferLet binds body = local (loadArgs (fmap fst binds)) $ do
  mapM_ (uncurry checkBind) binds
  inferExpr body
  where
    checkBind :: Typed Name -> Expr -> Check ()
    checkBind Typed{..} expr = do
      ty <- inferExpr expr
      lift $ guard $ ty == typed'type

inferCase :: Typed Expr -> [CaseAlt] -> Check Type
inferCase e alts = do
  checkTop e
  getResultType =<< mapM inferAlt alts
  where
    checkTop :: Typed Expr -> Check ()
    checkTop Typed{..} = do
      ty <- inferExpr typed'value
      guard $ ty == typed'type

    getResultType :: [Type] -> Check Type
    getResultType = \case
      []   -> lift Nothing
      t:ts -> do
        guard $ all (== t) ts
        return t

inferAlt :: CaseAlt -> Check Type
inferAlt CaseAlt{..} =
  local (loadArgs caseAlt'args) $
    inferExpr caseAlt'rhs

-------------------------------------------------------
-- type inference context

-- | Type context of the known signatures
newtype TypeContext = TypeContext (Map Name Type)

-- | Loads all user defined signatures to context
loadContext :: CoreProg -> TypeContext -> TypeContext
loadContext defs ctx =
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

