module Type.Infer where

import Control.Monad.Except
import Control.Monad.RWS

import Data.Text (Text)

import Type.Loc
import Type.Type
import Type.ClassEnv (ClassEnv)
import Type.Subst



newtype Infer a = Infer (RWST ClassEnv () InferSt (Except TypeError) a )
  deriving (Functor, Applicative, Monad,
            MonadReader ClassEnv, MonadState InferSt, MonadError TypeError)

data InferSt = InferSt
  { inferSt'subst  :: !Subst
  , inferSt'count  :: !Int
  }

getClassEnv :: Infer ClassEnv
getClassEnv = ask

runInfer :: Infer a -> ClassEnv -> Either TypeError a
runInfer (Infer st) ce = fmap fst $ runExcept $ evalRWST st ce (InferSt nullSubst 0)

getSubst :: Infer Subst
getSubst = fmap inferSt'subst get

extSubst :: Subst -> Infer ()
extSubst s = modify $ \st -> st { inferSt'subst = s `compose` inferSt'subst st }

unify :: Type -> Type -> Infer ()
unify t1 t2 = do
  s <- getSubst
  u <- mostGeneralUnifier (apply s t1) (apply s t2)
  extSubst u

newTVar :: Kind -> Infer Type
newTVar k = do
  n <- fmap inferSt'count get
  bumpCount
  return $ TVar (getLoc k) $ Tyvar (getLoc k) (enumId (getLoc k) n) k
  where
    bumpCount = modify $ \st -> st { inferSt'count = inferSt'count st + 1 }

freshInst :: Scheme -> Infer (Qual Type)
freshInst (Forall _ ks qt) = fmap (\ts -> inst ts qt) $ mapM newTVar ks

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts = \case
    TAp loc a b -> TAp loc (rec a) (rec b)
    TFun loc a b -> TFun loc (rec a) (rec b)
    TTuple loc as -> TTuple loc (fmap rec as)
    TGen _ n  -> ts !! n
    t       -> t
    where
      rec = inst ts

instance Instantiate a => Instantiate [a] where
  inst ts  = fmap (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (Qual loc ps t) = Qual loc (inst ts ps) (inst ts t)

instance Instantiate Pred where
  inst ts (IsIn loc idx t) = IsIn loc idx (inst ts t)







