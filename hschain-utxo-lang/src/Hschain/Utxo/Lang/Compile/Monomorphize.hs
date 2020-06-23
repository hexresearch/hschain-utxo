-- | Turns polymorphic programs to monomorphic ones
module Hschain.Utxo.Lang.Compile.Monomorphize(
  makeMonomorphic
) where

import Hex.Common.Text

import Control.Monad.State.Strict

import Data.Fix
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Sequence (Seq)

import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Infer (TypedProg)
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (primToType, funT)
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Typed(..), Type)
import Hschain.Utxo.Lang.Expr (Loc, noLoc, boolT, VarName(..))

import qualified Language.HM as H
import qualified Language.HM.Subst as H
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import qualified Data.Set as S

-- | Makes types monomorphic.
makeMonomorphic :: MonadLang m => TypedProg -> m TypedProg
makeMonomorphic prog = runMono (makeMono progMap context)
  where
    progMap = M.fromList $ fmap (\x -> (def'name x, x)) prog
    context = fmap getDefType progMap

type Mono a = StateT MonoSt (Either Error) a
type ProgMap = Map Name TypedDef
type TypedDef = AnnComb Type (Typed Name)
type Context = Map Name Type

runMono :: MonadLang m => Mono a -> m TypedProg
runMono m = liftEither $ fmap (M.elems . monoSt'prog) $ execStateT m initSt
  where
    initSt = MonoSt initSeed M.empty
    initSeed = Seq.singleton $ Typed "main" $ fromType boolT

fromType :: H.Type Loc Name -> Type
fromType = H.mapLoc (const ())

data MonoSt = MonoSt
  { monoSt'seeds :: Seq (Typed Name)  -- ^ Free variables found in the definition,
                                      --   with monomorphic types
  , monoSt'prog  :: ProgMap           -- ^ Result of the algorithm
  }

makeMono :: ProgMap -> Context -> Mono ()
makeMono progMap ctx = do
  st <- get
  mapM_ (procSeed progMap ctx $ monoSt'prog st) $ monoSt'seeds st

procSeed :: ProgMap -> Context -> ProgMap -> Typed Name -> Mono ()
procSeed progMap ctx resultProgMap seed
  | isMonoT $ typed'type seed =
      case M.lookup name progMap of
        Just defn -> withCheckCache name (procDef progMap ctx defn)
        Nothing   -> lift $ Left $ ExecError $ UnboundVariables [VarName noLoc name]
  | otherwise  = lift $ Left $ MonoError $ FailedToFindMonoType name
  where
    name = typed'value seed

    withCheckCache key act
      | M.member key resultProgMap = return ()
      | otherwise                  = act

procDef :: ProgMap -> Context -> TypedDef -> Mono ()
procDef progMap ctx defn = do
  (newSeeds, defn') <- substDef progMap ctx defn
  addSeeds newSeeds
  insertDef defn'

substDef :: ProgMap -> Context -> TypedDef -> Mono ([Typed Name], TypedDef)
substDef progMap ctx defn@Def{..} = do
  (frees, body) <- substExpr progMap locals ctx' def'body
  return $ (frees, defn { def'body = body } )
  where
    ctx' = ctx <> argsT
    argsT = M.fromList $ fmap (\Typed{..} -> (typed'value, typed'type)) def'args
    locals = S.fromList $ fmap typed'value def'args

substExpr :: ProgMap -> Set Name -> Context -> AnnExpr Type (Typed Name) -> Mono ([Typed Name], AnnExpr Type (Typed Name))
substExpr progMap locals ctx (Fix (Ann ty expr)) =
  case expr of
    EVar loc name               -> onVar loc name
    EPrim loc prim              -> onPrim loc prim
    EAp loc f a                 -> onAp loc f a
    ELet loc bs e               -> onLet loc bs e
    ELam loc args e             -> onLam loc args e
    EIf loc c t e               -> onIf loc c t e
    ECase loc e alts            -> onCase loc e alts
    EConstr loc conTy m n       -> onConstr loc conTy m n
    EBottom loc                 -> onBottom loc
  where
    rec = substExpr progMap locals ctx

    -- | TODO: consider rewrite for poly to mono-types
    onVar loc name =
      case M.lookup name ctx of
        Just varTy -> do
          ty' <- unify ty varTy
          let resE = Fix $ Ann ty' $ EVar loc name
              resF = if S.member name locals
                       then []
                       else [Typed name ty']
          return (resF, resE)
        Nothing -> throwError $ ExecError $ UnboundVariables [VarName loc name]

    onPrim loc prim = return ([], Fix $ Ann (primToType prim) $ EPrim loc prim)

    onAp loc f a
      | haveMonoTs [f, a] = do
        (fF, fE) <- rec f
        (aF, aE) <- rec a
        return (fF <> aF, Fix $ Ann ty $ EAp loc fE aE)
      | otherwise = do
        ty' <- unify (H.arrowT () (H.varT () "$1" ) ty) (getAnnType f )
        (fF, fE) <- rec $ setAnn ty' f
        case H.extractArrow ty' of
          Just (lhs, _) -> do
            aT' <- unify lhs (getAnnType a)
            (aF, aE) <- rec $ setAnn aT' a
            return $ (fF <> aF, Fix $ Ann ty' $ EAp loc fE aE)
          Nothing -> throwError $ MonoError $ FailedToFindMonoType "ap-nothing"


    onLet loc binds body = undefined {-do
      ty' <- unify ty (getAnnType body)
      (bodyF, bodyE) <- rec $ setAnn ty' body
-}

    onLam loc args body
      | all (isMonoT . typed'type) args && (isMonoT $ getAnnType body) = do
          let ctx' = (M.fromList $ zip argNames argTypes) <> ctx
              locals' = S.fromList argNames <> locals
          (bodyF, bodyE) <- substExpr progMap locals' ctx' body
          return (bodyF, Fix $ Ann ty $ ELam loc args bodyE)
      | otherwise = do
          ty' <- unify ty lamTy
          let (lamArgsT, lamRhsT) = H.extractFunType ty'
              ctx' = (M.fromList $ zip argNames lamArgsT) <> ctx
              locals' = S.fromList argNames <> locals
          (bodyF, bodyE) <- substExpr progMap locals' ctx' $ setAnn lamRhsT body
          let argsE = zipWith Typed argNames lamArgsT
          return (bodyF, Fix $ Ann ty' $ ELam loc argsE bodyE)
          where
            lamTy = funT (fmap typed'type args) (getAnnType body)
            argNames = fmap typed'value args
            argTypes = fmap typed'type args

    onIf loc c t e = do
      (cF, cE) <- rec $ setAnn (fromType boolT) c
      (tF, tE) <- rec $ setAnn ty t
      (eF, eE) <- rec $ setAnn ty e
      return (mconcat [cF, tF, eF], Fix $ Ann ty $ EIf loc cE tE eE)

    onCase = undefined

    onConstr loc conTy m n
      | isMonoT conTy = return ([], Fix $ Ann conTy $ EConstr loc conTy m n)
      | otherwise = do
          ty' <- unify ty conTy
          if isMonoT ty'
            then return ([], Fix $ Ann ty' $ EConstr loc ty' m n)
            else throwError $ MonoError $ FailedToFindMonoType $ mconcat ["Constr-", showt m, "-", showt n]

    onBottom loc = return ([], Fix $ Ann ty $ EBottom loc)

    setAnn t (Fix (Ann _ e)) = Fix $ Ann t e

    haveMonoTs = all (isMonoT . getAnnType)

isMonoT :: Type -> Bool
isMonoT (H.Type x) = flip cata x $ \case
  H.VarT _ _     -> False
  H.ConT _ _ as  -> and as
  H.ArrowT _ a b -> a && b
  H.ListT _ a    -> a
  H.TupleT _ as  -> and as

addSeeds :: [Typed Name] -> Mono ()
addSeeds seeds =
  modify' $ \st -> st { monoSt'seeds = monoSt'seeds st <> Seq.fromList seeds }

insertDef :: TypedDef -> Mono ()
insertDef defn =
  modify' $ \st -> st { monoSt'prog = M.insert (def'name defn) defn $ monoSt'prog st }

getDefType :: TypedDef -> Type
getDefType Def{..} = foldr (\a b -> H.arrowT () a b) rhs args
  where
    args = fmap typed'type def'args
    rhs  = getAnnType def'body

getAnnType :: AnnExpr Type (Typed Name) -> Type
getAnnType (Fix (Ann ty _)) = ty

unify :: Type -> Type -> Mono Type
unify tA tB = case H.unifyTypes tA tB of
  Right subst -> return $ H.apply subst tB
  Left err    -> throwError $ TypeError $ H.mapLoc (const noLoc) err

