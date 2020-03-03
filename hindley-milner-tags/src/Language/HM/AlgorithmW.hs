--------------------------------------------------------------------------------

module Language.HM.AlgorithmW (
    -- * Typing contexts
    Context(..),
    empty,

    -- * External interface
    runW,
    inferW,

    -- * Misc
    genInOrder
) where

--------------------------------------------------------------------------------

import Data.Fix
import Data.Function (on)
import Data.String
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Map as M

import Language.HM.Term
import Language.HM.Theta
import Language.HM.Type
import Language.HM.TypeError

--------------------------------------------------------------------------------

-- | Typing contexts.
newtype Context src = Context { unContext :: M.Map Var (Sigma src) }

-- | 'empty' is the empty typing context.
empty :: Context src
empty = Context M.empty

-- instance CanApply Context where
applyContext :: Theta src -> Context src -> Context src
applyContext s = Context . M.map (applySigma s) . unContext

--------------------------------------------------------------------------------

-- | The type of Algorithm W computations.
newtype W src a = W { unW :: Context src -> Int -> Either (TypeError src) (a, Int) }

instance Functor (W src) where
    f `fmap` (W m) = W $ \ctx n -> do
        (r,n') <- m ctx n
        return (f r, n')

instance Applicative (W src) where
    pure x = W $ \_ n -> return (x, n)

    (W m) <*> (W m') = W $ \ctx n -> do
        (f, n')  <- m ctx n
        (x, n'') <- m' ctx n'
        return (f x, n'')

instance Monad (W src) where
    W m >>= f = W $ \ctx n -> do
        (r, n') <- m ctx n
        let W m' = f r in m' ctx n'

--------------------------------------------------------------------------------

-- | 'fresh' returns a fresh type variable.
fresh :: src -> W src (Tau src)
fresh src = W $ \_ n -> return (varT src (mconcat ["$", fromString $ show n]), n + 1)

typeError :: TypeError src -> W src a
typeError err = W $ \_ _ -> Left err

--------------------------------------------------------------------------------

-- | 'gen' @t@ generalises a monomorphic type @t@ to a polymorphic type.
gen :: Tau src -> W src (Sigma src)
gen t = W $ \ctx n -> return (gen' ctx t, n)

gen' :: Context src -> Tau src -> Sigma src
gen' ctx t = L.foldr (\(ty, src) -> forAllT src ty) (monoT t) $ M.toList $ unVarSet vs
    where
        cs = mconcat $ map tyVars $ M.elems $ unContext ctx
        vs = tyVars t `differenceVarSet` cs

-- | 'genInOrder' @ctx t@ generalises a monomorphic type @t@ to a polymorphic
-- type in a context @ctx@. This variant of 'gen' ensures that the order of
-- quantifiers matches that in which type variables occur in @t@.
genInOrder :: Context src -> Tau src -> Sigma src
genInOrder ctx t = foldr (\(var, src) -> forAllT src var) (monoT t) vs
    where
        cs = mconcat $ map tyVars $ M.elems $ unContext ctx
        vs = L.deleteFirstsBy ((==) `on` fst) (tyVarsInOrder t) (M.toList $ unVarSet cs)

-- | 'inst' @t@ instantiates a polymorphic type @t@ with fresh type variables.
inst :: Sigma src -> W src (Tau src)
inst = cataM go . unSigma
    where
        go (MonoT t) = return t
        go (ForAllT src x t) = do
            i <- fresh src

            let s = Theta $ M.singleton x i

            return (applyTau s t)

-- | 'withContext' @f m@ runs applies @f@ to the typing context in which @m@ is
-- run. The context of the overall computation is not affected.
withContext :: (Context src -> Context src) -> W src a -> W src a
withContext f (W m) = W $ \ctx n -> m (f ctx) n

-- | 'lookupType' @x@ looks up the type of @x@ in the context.
lookupType :: Var -> W src (Maybe (Sigma src))
lookupType x = W $ \ctx n -> return (M.lookup x $ unContext ctx, n)

-- | 'requireType' @x@ looks up the type of @x@ in the context. A type error
-- is raised if @x@ is not typed in the context.
requireType :: src -> Var -> W src (Sigma src)
requireType src x = lookupType x >>= \r -> case r of
    Nothing -> typeError $ NotInScopeErr src x
    Just t  -> return t

--------------------------------------------------------------------------------

-- | 'bind' @x t@ binds a type @t@ to a type variable named @x@.
bind :: Text -> TauF src (Fix (TauF src)) -> W src (Theta src)
bind x (VarT _ y) | x == y = return $ Theta M.empty
bind x t = case getVar (tyVars (Tau $ Fix t)) x of
  Just src -> typeError $ OccursErr src x (Tau $ Fix t)
  Nothing  -> return $ Theta $ M.singleton x (Tau $ Fix t)

-- | 'unify' @t0 t1@ unifies two types @t0@ and @t1@. If the two types can be
-- unified, a substitution is returned which unifies them.
unify :: Tau src -> Tau src -> W src (Theta src)
unify (Tau tau0) (Tau tau1) = go (unFix tau0) (unFix tau1)
    where
        go (VarT _ x) t = bind x t
        go t (VarT _ x) = bind x t
        go (ConT _ a) (ConT _ b) | a == b = return emptyTheta
        go (AppT _ f x) (AppT _ g y) = unifyPair (f, x) (g, y)
        go (ArrowT _ f x) (ArrowT _ g y) = unifyPair (f, x) (g, y)
        -- without base types etc., all types are unifiable
        go t0 t1 = typeError $ UnifyErr (getSrc $ Tau $ Fix t0) (Tau $ Fix t0) (Tau $ Fix t1)

        unifyPair (f, x) (g, y) = do
            s0 <- go (unFix f) (unFix g)
            s1 <- go (unFix $ unTau $ applyTau s0 $ Tau x) (unFix $ unTau $ applyTau s0 $ Tau y)
            return (s1 <@> s0)

--------------------------------------------------------------------------------

-- | 'infer' @term@ reconstructs types in @term@.
infer :: forall src . Term src -> W src (TyTerm src)
infer term = (\(s,_,e) -> applyTyTerm s e) `fmap` cata go term
    where
        go :: TermF src Var (W src (Theta src, Tau src, TyTerm src)) -> W src (Theta src, Tau src, TyTerm src)
        go (Var src x) = do
            -- @x@ must be typed in the context
            pt <- requireType src x

            -- instantiate the type of @x@
            mt <- inst pt

            -- return an annotated variable along with an empty substituion
            return (emptyTheta, mt, tyVarE src (Typed x (monoT mt)) mt)
        go (App src f x) = do
            (s0, t0, tf) <- f
            (s1, t1, tx) <- withContext (applyContext s0) x

            -- generate a fresh type variable to represent the return type
            -- of the function
            mt <- fresh src

            -- unify the types
            s2 <- unify t0 (arrowT src t1 mt)

            -- return the annotated application
            return (s2 <@> s1 <@> s0, applyTau s2 mt, tyAppE src tf tx mt)
        go (Abs src x e) = do
            -- we need a fresh type variable for @x@
            mt <- fresh src

            --
            (s0, t0, te) <- withContext (Context . M.insert x (monoT mt) . unContext) e

            let rt = arrowT src mt t0

            -- return the annotated abstraction
            return (s0, applyTau s0 rt, tyAbsE src (Typed x (monoT mt)) te rt)
        go (Let src x e0 e1) = do
            -- infer the type of the expression that is being bound
            (s0, t0, te0) <- e0

            -- generalise the type of the expression that is being bound
            pt <- withContext (applyContext s0) (gen t0)

            -- infer the type of the other expression
            (s1, t1, te1) <- withContext (applyContext s0 . Context . M.insert x pt . unContext) e1

            -- return the annotated let binding
            return (s1 <@> s0, t1, tyLetE src (Typed x pt) te0 te1 t1)

--------------------------------------------------------------------------------

-- | 'runW' @gamma term@ runs Algorithm W on @term@ with an initial context
-- @gamma@ and returns an updated @term@ with explicit type annotations.
runW :: Context src -> Term src -> Either (TypeError src) (TyTerm src)
runW ctx term = fst `fmap` unW (infer term) ctx 0

-- | 'inferW' @gamma term@ runs Algorithm W on @term@ with an initial context
-- @gamma@ and returns the polymorphic type of the whole term.
inferW :: Context src -> Term src -> Either (TypeError src) (Tau src)
inferW ctx term = (tyAnn . unTypedF . unFix) `fmap` runW ctx term

--------------------------------------------------------------------------------
