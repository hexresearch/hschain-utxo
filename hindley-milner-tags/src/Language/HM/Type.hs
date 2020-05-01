-- | This module contains the abstract syntax of Hindley-Milner types.
module Language.HM.Type (
    IsVar(..),
    HasLoc(..),
    -- * Monomorphic types.
    TypeF(..),
    Type(..),
    varT,
    conT,
    arrowT,
    tupleT,
    listT,

    -- * Polymorphic types.
    SignatureF(..),
    Signature(..),
    forAllT,
    monoT,
    stripSignature,
    typeToSignature,

    VarSet(..),
    differenceVarSet,
    getVar,
    varSetToList,
    memberVarSet,

    HasTypeVars(..),
    LocFunctor(..)
) where

--------------------------------------------------------------------------------

import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.Fix
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Tuple (swap)

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Text.Show.Deriving

--------------------------------------------------------------------------------

class HasLoc f where
  type Loc f :: *
  getLoc :: f -> Loc f

class Ord v => IsVar v where
  -- | Way to allocate fresh variables from integer count
  intToVar      :: Int -> v

  -- | Canonical leters for pretty output
  prettyLetters :: [v]

instance HasLoc (Type loc v) where
  type Loc (Type loc v) = loc

  getLoc (Type (Fix x)) = case x of
    VarT   loc _   -> loc
    ConT   loc _ _ -> loc
    ArrowT loc _ _ -> loc
    TupleT loc _   -> loc
    ListT  loc _   -> loc

instance HasLoc (Signature loc var) where
  type Loc (Signature loc var) = loc

  getLoc (Signature x) = cata go x
    where
      go = \case
        MonoT ty        -> getLoc ty
        ForAllT loc _ _ -> loc

data TypeF loc var r
    = VarT loc var
    | ConT loc var [r]
    | ArrowT loc r r    -- special case of ConT that is rendered as ->
    | TupleT loc [r]    -- special case of ConT that is rendered as (,,,)
    | ListT loc r       -- special case of ConT that is rendered as [a]
    deriving (Eq, Ord, Show, Functor)

-- | Monomorphic types.
newtype Type loc var = Type { unType :: Fix (TypeF loc var) }
  deriving (Show, Eq, Ord)

-- | 'varT' @x@ constructs a type variable named @x@.
varT :: loc -> var -> Type loc var
varT loc var = Type $ Fix $ VarT loc var

-- | 'varT' @x@ constructs a type variable named @x@.
conT :: loc -> var -> [Type loc var] -> Type loc var
conT loc name args = Type $ Fix $ ConT loc name $ fmap unType $ args

-- | 'arrowT' @t0 t1@ constructs an arrow type from @t0@ to @t1@.
arrowT :: loc -> Type loc v -> Type loc v -> Type loc v
arrowT loc (Type t0) (Type t1) = Type $ Fix $ ArrowT loc t0 t1

tupleT :: loc -> [Type loc var] -> Type loc var
tupleT loc ts = Type $ Fix $ TupleT loc $ fmap unType ts

listT :: loc -> Type loc var -> Type loc var
listT loc (Type t) = Type $ Fix $ ListT loc t

--------------------------------------------------------------------------------

data SignatureF loc var r
    = ForAllT loc var r
    | MonoT (Type loc var)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Polymorphic types.
newtype Signature loc var = Signature { unSignature :: Fix (SignatureF loc var)
  } deriving (Show, Eq, Ord)

instance Functor (Signature loc) where
  fmap f (Signature x) = Signature $ cata go x
    where
      go = \case
        ForAllT loc var a -> Fix $ ForAllT loc (f var) a
        MonoT ty          -> Fix $ MonoT $ fmap f ty

instance Functor (Type a) where
  fmap f (Type x) = Type $ cata go x
    where
      go = \case
        VarT loc name      -> Fix $ VarT loc $ f name
        ConT loc name args -> Fix $ ConT loc (f name) args
        ArrowT loc a b     -> Fix $ ArrowT loc a b
        TupleT loc as      -> Fix $ TupleT loc as
        ListT loc a        -> Fix $ ListT loc a

class LocFunctor f where
  mapLoc :: (locA -> locB) -> f locA var -> f locB var

instance LocFunctor Type where
  mapLoc f (Type x) = Type $ cata go x
    where
      go = \case
        VarT loc name      -> Fix $ VarT (f loc) name
        ConT loc name args -> Fix $ ConT (f loc) name args
        ArrowT loc a b     -> Fix $ ArrowT (f loc) a b
        TupleT loc as      -> Fix $ TupleT (f loc) as
        ListT loc a        -> Fix $ ListT (f loc) a

instance LocFunctor Signature where
  mapLoc f (Signature x) = Signature $ cata go x
    where
      go = \case
        ForAllT loc var a -> Fix $ ForAllT (f loc) var a
        MonoT ty          -> Fix $ MonoT $ mapLoc f ty

-- | 'forAllT' @x t@ universally quantifies @x@ in @t@.
forAllT :: loc -> v -> Signature loc v -> Signature loc v
forAllT loc x (Signature t) = Signature $ Fix $ ForAllT loc x t

-- | 'monoT' @t@ lifts a monomorophic type @t@ to a polymorphic one.
monoT :: Type loc src -> Signature loc src
monoT = Signature . Fix . MonoT

{-
instance AlphaEq (Signature src) where
    alphaEq (Signature sig0) (Signature sig1) = sigmaEq M.empty (unFix sig0) (unFix sig1)
        where
            tauEq env (VarT _ x) (VarT _ y) = case M.lookup x env of
                -- the variable is bound in the left expression: check that
                -- it matches the name of the variable in the right expression
                -- that was bound at the same point
                Just y' -> y == y'
                -- the variable is free in the left expression: it should have
                -- the same name as the variable in the right expression
                Nothing -> x == y
            tauEq _ (ConT _ x) (ConT _ y) = x == y
            tauEq env (AppT _ x0 x1) (AppT _ y0 y1) =
                tauEq env (unFix x0) (unFix y0) &&
                tauEq env (unFix x1) (unFix y1)
            tauEq env (ArrowT _ x0 x1) (ArrowT _ x0' x1') =
                tauEq env (unFix x0) (unFix x0') &&
                tauEq env (unFix x1) (unFix x1')
            tauEq _ _ _ = False

            sigmaEq env (MonoT (Type t0)) (MonoT (Type t1)) =
                tauEq env (unFix t0) (unFix t1)
            sigmaEq env (ForAllT _ x t0) (ForAllT _ y t1) =
                sigmaEq (M.insert x y env) (unFix t0) (unFix t1)
            sigmaEq _ _ _ = False
-}


typeToSignature :: (Eq loc, Ord v) => Type loc v -> Signature loc v
typeToSignature ty = foldr (\(v, src) a -> forAllT src v a) (monoT ty) vs
  where
    vs = tyVarsInOrder ty

--------------------------------------------------------------------------------

-- | The class of types which have free type variables.
class HasTypeVars f where
    -- | 'tyVars' @t@ calculates the set of free type variables in @t@.
    tyVars :: Ord var => f src var -> VarSet src var

    -- | 'tyVarsInOrder' @t@ is like 'tyVars' @t@, except that the type
    -- variables are returned in the order in which they are encountered.
    tyVarsInOrder :: (Eq src, Ord var) => f src var -> [(var, src)]

instance HasTypeVars Type where
    tyVars = cata go . unType
      where
        go = \case
          VarT loc v    -> VarSet $ M.singleton v loc
          ConT _ _ args -> mconcat args
          ArrowT _ a b  -> mappend a b
          TupleT _ as   -> mconcat as
          ListT _ a     -> a

    tyVarsInOrder = L.nub . cata go . unType
      where
        go = \case
          VarT loc var -> [(var, loc)]
          ConT _ _ as  -> mconcat as
          ArrowT _ a b -> mappend a b
          TupleT _ as  -> mconcat as
          ListT _ a    -> a


instance HasTypeVars Signature where
    tyVars = cata go . unSignature
      where
        go = \case
          MonoT t       -> tyVars t
          ForAllT _ x t -> VarSet $ M.delete x $ unVarSet t

    tyVarsInOrder = L.nub . cata go . unSignature
      where
        go = \case
          MonoT t         -> tyVarsInOrder t
          ForAllT src x t -> L.deleteBy ((==) `on` fst) (x, src) t

--------------------------------------------------------------------------------

newtype VarSet src var = VarSet { unVarSet :: Map var src }

instance Ord var => Semigroup (VarSet src var) where
  (VarSet a) <> (VarSet b) = VarSet $ M.union a b

instance Ord var => Monoid (VarSet src var) where
  mempty = VarSet M.empty

differenceVarSet :: Ord var => VarSet src var -> VarSet src var -> VarSet src var
differenceVarSet (VarSet a) (VarSet b) = VarSet $ a `M.difference` b

getVar :: Ord var => VarSet src var -> var -> Maybe src
getVar (VarSet m) var = M.lookup var m

varSetToList :: VarSet src var -> [(src, var)]
varSetToList (VarSet m) = fmap swap $ M.toList m

memberVarSet :: Ord var => var -> VarSet src var -> Bool
memberVarSet k (VarSet m) = M.member k m

--------------------------------------------------------------------------------

stripSignature :: Signature src var -> Type src var
stripSignature = cata go . unSignature
  where
    go = \case
      ForAllT _ _ r -> r
      MonoT ty -> ty

$(deriveShow1 ''TypeF)
$(deriveShow1 ''SignatureF)
$(deriveEq1 ''TypeF)
$(deriveEq1 ''SignatureF)
$(deriveOrd1 ''TypeF)
$(deriveOrd1 ''SignatureF)
