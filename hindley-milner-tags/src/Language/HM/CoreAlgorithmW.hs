module Language.HM.AlgorithmW(
    Context
  , inferType
) where

import Debug.Trace
import Text.Show.Pretty

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer

import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe

import Language.HM.Term
import Language.HM.Theta
import Language.HM.Type
import Language.HM.TypeError


import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Context = Map Var Sigma

instance CanApply Context where
    apply subst = fmap (apply subst)

type InferM a = StateT Int (Except TypeError) a

runInferM :: InferM a -> Either TypeError a
runInferM m = runExcept $ evalStateT m 0

inferType :: Context -> Term -> Either TypeError Tau
inferType ctx term = fmap snd $ runInferM $ infer ctx term

infer :: Context -> Term -> InferM (Theta, Tau)
infer ctx (Fix x) = case x of
  Var v       -> inferVar ctx v
  App a b     -> inferApp ctx a b
  Abs v r     -> inferAbs ctx v r
  Let v a b   -> inferLet ctx v a b

inferVar :: Context -> Var -> InferM (Theta, Tau)
inferVar ctx v = fmap (mempty, ) $ maybe err newInstance $ M.lookup v ctx
  where
    err = throwError $ NotInScopeErr v

inferApp :: Context -> Term -> Term -> InferM (Theta, Tau)
inferApp ctx f a = do
  tvn <- fmap varT freshVar
  res <- inferTerms ctx [f, a]
  case res of
    (phi, [tf, ta]) -> liftEither $ fmap (\subst -> (subst, apply subst tvn)) $ unify phi tf (arrowT ta tvn)

inferAbs :: Context -> Var -> Term -> InferM (Theta, Tau)
inferAbs ctx x body = do
  tvn <- freshVar
  (phi, tbody) <- infer (ctx1 tvn) body
  return (phi, arrowT (apply phi (varT tvn)) tbody)
  where
    ctx1 tvn = M.insert x (newVar tvn) ctx

    newVar tvn = monoT $ varT tvn

inferLet :: Context -> Var -> Term -> Term -> InferM (Theta, Tau)
inferLet ctx v term body = do
  (phi, tTerm) <- infer ctx term
  ctx1 <- addDecl v tTerm $ apply phi ctx
  (subst, tbody) <- infer ctx1 body
  return (subst <@> phi, tbody)

addDecl :: Var -> Tau -> Context -> InferM Context
addDecl v t ctx = do
  scheme <- toScheme unknowns t
  return $ M.insert v scheme ctx
  where
    toScheme uVars ty = do
      (subst, newVars) <- fmap (\xs -> (toSubst xs, fmap snd xs)) $
          mapM (\sv -> fmap ((sv, )) freshVar) $ S.toList schematicVars
      return $ foldr forAllT (monoT (apply subst ty)) newVars
      where
        schematicVars = tyVars ty `S.difference` uVars

    unknowns = foldMap tyVars ctx
    toSubst = M.fromList . fmap (second varT)

inferLetRec = undefined

inferTerms :: Context -> [Term] -> InferM (Theta, [Tau])
inferTerms ctx ts = case ts of
  []   -> return $ (mempty, [])
  a:as -> do
    (phi, ta)  <- infer ctx a
    (psi, tas) <- inferTerms (apply phi ctx) as
    return (psi <@> phi, apply psi ta : tas)

unify :: Theta -> Tau -> Tau -> Either TypeError Theta
unify phi (Fix x) (Fix y) = case (x, y) of
  (VarT tvn, t) ->
      let phiTvn = applyVar phi tvn
          phiT   = apply phi (Fix t)
      in  if phiTvn == varT tvn
            then extend phi tvn phiT
            else unify phi phiTvn phiT
  (ConT name ts, VarT v) -> unify phi (varT v) (conT name ts)
  (ConT n xs, ConT m ys) ->
    if n == m
      then unifyl phi xs ys
      else Left $ UnifyErr (Fix x) (Fix y)
  where
    unifyl subst as bs = foldr go (Right subst) $ zip as bs
      where
        go (a, b) eSubst = (\t -> unify t a b) =<< eSubst

applyVar :: Theta -> Var -> Tau
applyVar subst v = fromMaybe (varT v) $ M.lookup v subst

extend :: Theta -> Var -> Tau -> Either TypeError Theta
extend phi tvn ty
  | varT tvn == ty           = Right phi
  | S.member tvn (tyVars ty) = Left $ OccursErr tvn ty
  | otherwise                = Right $ delta tvn ty <@> phi

delta :: Var -> Tau -> Theta
delta v ty = M.fromList [(v, ty)]

--------------------------------------------------------------

newInstance :: Sigma -> InferM Tau
newInstance = fmap (uncurry apply) . cataM go
  where
    go = \case
      MonoT ty -> return (mempty, ty)
      ForAllT v (m, ty) -> fmap (\nv -> (M.insert v (varT nv) m, ty)) freshVar

freshVar :: InferM Var
freshVar = do
  n <- get
  put $ n + 1
  return $ mappend "$$" (show n)

