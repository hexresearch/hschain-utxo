module Language.HM.AlgorithmW (
    -- * Typing contexts
    Context(..),
    emptyContext,

    -- * External interface
    runW,
    inferW,
    subtypeOf,

    -- * Misc
    genInOrder,
    normaliseType,
    normaliseSignature,
    typeToSignature
) where

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad

import Data.Fix
import Data.Function (on)
import Data.String
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Map.Strict as M

import Language.HM.Term
import Language.HM.Subst
import Language.HM.Type
import Language.HM.TypeError

{-
import Debug.Trace
import Text.Show.Pretty (ppShow)
-}

-- | Tag to distinguish already proven code from user code.
-- We need it to show proper source code location on type mismatch.
data Origin a
  = Proven a
  | UserCode a
  deriving (Show, Eq, Functor)

instance HasLoc a => HasLoc (Origin a) where
  type Loc (Origin a) = Loc a
  getLoc = \case
    Proven a   -> getLoc a
    UserCode a -> getLoc a

getUserCodeSrc :: Origin src -> Origin src -> src
getUserCodeSrc a b = case (a, b) of
  (UserCode res, _) -> res
  (_, UserCode res) -> res
  (Proven res, _)   -> res

markProven :: Context src -> Context (Origin src)
markProven (Context m) = Context $ fmap (fmap Proven) m

markUserCode :: Functor f  => f src -> f (Origin src)
markUserCode = fmap UserCode

unOrigin :: Origin a -> a
unOrigin = \case
  Proven a   -> a
  UserCode a -> a

--------------------------------------------------------------------------------

-- | Typing contexts.
newtype Context src = Context { unContext :: M.Map Var (Signature src)
  } deriving (Show, Eq, Semigroup, Monoid)

-- | 'empty' is the empty typing context.
emptyContext :: Context src
emptyContext = Context M.empty

-- instance CanApply Context where
applyContext :: Subst src -> Context src -> Context src
applyContext s = Context . M.map (applySignature s) . unContext

--------------------------------------------------------------------------------

-- | The type of Algorithm W computations.
newtype W src a = W { unW :: Context (Origin src) -> Int -> Either (TypeError src) (a, Int) }

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
fresh :: Origin src -> W src (Type (Origin src))
fresh src = W $ \_ n -> return (varT src (mconcat ["$", fromString $ show n]), n + 1)

typeError :: TypeError src -> W src a
typeError err = W $ \_ _ -> Left err

--------------------------------------------------------------------------------

-- | 'gen' @t@ generalises a monomorphic type @t@ to a polymorphic type.
gen :: Type (Origin src) -> W src (Signature (Origin src))
gen t = W $ \ctx n -> return (gen' ctx t, n)

gen' :: Context src -> Type src -> Signature src
gen' ctx t = L.foldr (\(ty, src) -> forAllT src ty) (monoT t) $ M.toList $ unVarSet vs
    where
        cs = mconcat $ map tyVars $ M.elems $ unContext ctx
        vs = tyVars t `differenceVarSet` cs

-- | 'genInOrder' @ctx t@ generalises a monomorphic type @t@ to a polymorphic
-- type in a context @ctx@. This variant of 'gen' ensures that the order of
-- quantifiers matches that in which type variables occur in @t@.
genInOrder :: Context src -> Type src -> Signature src
genInOrder ctx t = foldr (\(var, src) -> forAllT src var) (monoT t) vs
    where
        cs = mconcat $ map tyVars $ M.elems $ unContext ctx
        vs = L.deleteFirstsBy ((==) `on` fst) (tyVarsInOrder t) (M.toList $ unVarSet cs)

-- | 'inst' @t@ instantiates a polymorphic type @t@ with fresh type variables.
inst :: Signature (Origin src) -> W src (Type (Origin src))
inst = cataM go . unSignature
    where
        go (MonoT t) = return t
        go (ForAllT src x t) = do
            i <- fresh src

            let s = Subst $ M.singleton x i

            return (applyType s t)

-- | 'withContext' @f m@ runs applies @f@ to the typing context in which @m@ is
-- run. The context of the overall computation is not affected.
withContext :: (Context (Origin src )-> Context (Origin src)) -> W src a -> W src a
withContext f (W m) = W $ \ctx n -> m (f ctx) n

-- | 'lookupType' @x@ looks up the type of @x@ in the context.
lookupType :: Var -> W src (Maybe (Signature (Origin src)))
lookupType x = W $ \ctx n -> return (M.lookup x $ unContext ctx, n)

-- | 'requireType' @x@ looks up the type of @x@ in the context. A type error
-- is raised if @x@ is not typed in the context.
requireType :: Origin src -> Var -> W src (Signature (Origin src))
requireType src x = lookupType x >>= \r -> case r of
    Nothing -> typeError $ NotInScopeErr (unOrigin src) x
    Just t  -> return $ fmap (const src) t

--------------------------------------------------------------------------------

-- | 'bind' @x t@ binds a type @t@ to a type variable named @x@.
bind :: Text -> TypeF (Origin src) (Fix (TypeF (Origin src))) -> W src (Subst (Origin src))
bind x (VarT _ y) | x == y = return $ Subst M.empty
bind x t = case getVar (tyVars (Type $ Fix t)) x of
  Just src -> typeError $ OccursErr (getUserCodeSrc src src2) x (fmap unOrigin $ Type $ Fix t)
  Nothing  -> return $ Subst $ M.singleton x (Type $ Fix t)
  where
    src2 = getLoc (Type $ Fix t)

-- | 'unify' @t0 t1@ unifies two types @t0@ and @t1@. If the two types can be
-- unified, a substitution is returned which unifies them.
unify :: Show src => Type (Origin src) -> Type (Origin src) -> W src (Subst (Origin src))
unify (Type tau0) (Type tau1) = go (unFix tau0) (unFix tau1)
    where
        go (VarT _ x) t = bind x t
        go t (VarT _ x) = bind x t
        go (ConT _ a) (ConT _ b) | a == b = return emptySubst
        go (AppT _ f x) (AppT _ g y) = unifyPair (f, x) (g, y)
        go (ArrowT _ f x) (ArrowT _ g y) = unifyPair (f, x) (g, y)
        -- without base types etc., all types are unifiable
        go t0 t1 =
          let toLoc = getLoc . Type . Fix
              loc = getUserCodeSrc (toLoc t0) (toLoc t1)
          in  typeError $ UnifyErr loc (fmap unOrigin $ Type $ Fix t0) (fmap unOrigin $ Type $ Fix t1)


        unifyPair (f, x) (g, y) = do
            s0 <- go (unFix f) (unFix g)
            s1 <- go (unFix $ unType $ applyType s0 $ Type x) (unFix $ unType $ applyType s0 $ Type y)
            return (s1 <@> s0)

subtypeOf' :: Show src => Type (Origin src) -> Type (Origin src) -> W src (Subst (Origin src))
subtypeOf' (Type tau0) (Type tau1) = go (unFix tau0) (unFix tau1)
  where
    go (VarT _ x) t@(VarT _ _) = bind x t
    go t (VarT _ x) = bind x t
    go (ConT _ a) (ConT _ b) | a == b = return emptySubst
    go (AppT _ f x) (AppT _ g y) = subPair (f, x) (g, y)
    go (ArrowT _ f x) (ArrowT _ g y) = subPair (f, x) (g, y)
    -- without base types etc., all types are unifiable
    go t0 t1 =
      let toLoc = getLoc . Type . Fix
          loc = getUserCodeSrc (toLoc t1) (toLoc t0)
      in  typeError $ UnifyErr loc (fmap unOrigin $ Type $ Fix t0) (fmap unOrigin $ Type $ Fix t1)

    subPair (f, x) (g, y) = do
        s0 <- go (unFix f) (unFix g)
        s1 <- go (unFix $ unType $ applyType s0 $ Type x) (unFix $ unType $ applyType s0 $ Type y)
        return (s1 <@> s0)



--------------------------------------------------------------------------------

-- | 'infer' @term@ reconstructs types in @term@.
infer :: forall src . Show src => Term (Origin src) -> W src (TyTerm (Origin src))
infer term = (\(s,_,e) -> applyTyTerm s e) `fmap` cata go (unTerm term)
    where
        go :: TermF (Origin src) Var (W src (Subst (Origin src), Type (Origin src), TyTerm (Origin src))) -> W src (Subst (Origin src), Type (Origin src), TyTerm (Origin src))
        go (Var src x) = do
            -- @x@ must be typed in the context
            pt <- requireType src x

            -- instantiate the type of @x@
            mt <- inst pt

            -- return an annotated variable along with an empty substituion
            return (emptySubst, mt, tyVarE src (TyVar $ Typed x (monoT mt)) mt)
        go (App src f x) = do
            (s0, t0, tf) <- f
            (s1, t1, tx) <- withContext (applyContext s0) x

            -- generate a fresh type variable to represent the return type
            -- of the function
            mt <- fresh src

            -- unify the types
            s2 <- unify t0 (arrowT src t1 mt)

            -- return the annotated application
            return (s2 <@> s1 <@> s0, applyType s2 mt, tyAppE src tf tx mt)
        go (Abs src x e) = do
            -- we need a fresh type variable for @x@
            mt <- fresh src

            --
            (s0, t0, te) <- withContext (Context . M.insert x (monoT mt) . unContext) e

            let rt = arrowT src (fmap (const src) mt) t0

            -- return the annotated abstraction
            return (s0, applyType s0 rt, tyAbsE src (TyVar $ Typed x (monoT mt)) te rt)
        go (Let src x e0 e1) = do
            -- infer the type of the expression that is being bound
            (s0, t0, te0) <- e0

            -- generalise the type of the expression that is being bound
            pt <- withContext (applyContext s0) (gen t0)

            -- infer the type of the other expression
            (s1, t1, te1) <- withContext (applyContext s0 . Context . M.insert x pt . unContext) e1

            -- return the annotated let binding
            return (s1 <@> s0, t1, tyLetE src (TyVar $ Typed x pt) te0 te1 t1)
        go (AssertType _ x sign) = do
            (s0, t0, tx) <- x
            ty <- inst sign
            s1 <- subtypeOf' ty t0
            return (s1 <@> s0, ty, tx)
{-
trace' x = trace (ppShow x) x
trace1 msg x = trace (mconcat [msg, ": " , ppShow x]) x
-}
--------------------------------------------------------------------------------

-- | 'runW' @gamma term@ runs Algorithm W on @term@ with an initial context
-- @gamma@ and returns an updated @term@ with explicit type annotations.
runW :: Show src => Context src -> Term src -> Either (TypeError src) (TyTerm src)
runW ctx term = fmap (fmap unOrigin) $ fst `fmap` unW (infer $ markUserCode term) (markProven ctx) 0

-- | 'inferW' @gamma term@ runs Algorithm W on @term@ with an initial context
-- @gamma@ and returns the polymorphic type of the whole term.
inferW :: Show src => Context src -> Term src -> Either (TypeError src) (Type src)
inferW ctx term = either (Left . normaliseTypeError) (Right . normaliseType) $
  (tyAnn . unTypedF . unFix . unTyTerm) `fmap` runW ctx term

subtypeOf :: Show src => Context src -> Signature src -> Signature src -> Either (TypeError src) (Subst src)
subtypeOf ctx ta tb =
  fmap (fmap unOrigin) $ fst `fmap` unW (join $ liftA2 subtypeOf' (inst $ markUserCode ta) (inst $ markUserCode tb)) (markProven ctx) 0

--------------------------------------------------------------------------------
-- normalise result type

normaliseSignature :: Signature src -> Signature src
normaliseSignature x = applySignature (normaliseSubst x) x

normaliseType :: Type src -> Type src
normaliseType x = applyType (normaliseSubst x) x

normaliseTypeError :: TypeError src -> TypeError src
normaliseTypeError = \case
  OccursErr src text ty  -> OccursErr src text (normaliseType ty)
  UnifyErr src tyA tyB   -> UnifyErr src (normaliseType tyA) (normaliseType tyB)
  NotInScopeErr src name -> NotInScopeErr src name

{- todo: implement and normalise runW
normaliseTyTerm :: TyTerm src -> TyTerm src
normaliseTyTerm x = applyTyTerm (normaliseSubst x) x
-}
normaliseSubst :: HasTypeVars m => m src -> Subst src
normaliseSubst x =
  Subst $ M.fromList $
      zipWith (\(nameA, src) nameB -> (nameA, varT src nameB)) (tyVarsInOrder x) letters

letters :: [Text]
letters = fmap fromString $ [1..] >>= flip replicateM ['a'..'z']

typeToSignature :: Show src => Type src -> Signature src
typeToSignature ty = (foldr (.) id $ fmap (uncurry $ flip forAllT) vs) (monoT ty)
  where
    vs = tyVarsInOrder ty

