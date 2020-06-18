-- | This module contains type annotations for terms of the language.
module Language.HM.TyTerm(
    Ann(..)
  , TyTerm(..)
  , tyVarE
  , tyAppE
  , tyLamE
  , tyLetE
  , tyLetRecE
  , tyAssertTypeE
  , tyCaseE
  , tyConstrE
  , tyBottomE
  , mapType
) where

import Data.Fix

import Language.HM.Subst
import Language.HM.Type
import Language.HM.Term

-- | Type to annotate nodes of AST.
-- We use it for type annotations.
data Ann note f a = Ann
  { ann'note  :: note
  , ann'value :: f a
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Terms with type annotations for all subexpressions.
newtype TyTerm loc v = TyTerm { unTyTerm :: Fix (Ann (Type loc v) (TermF loc v)) }
  deriving (Show, Eq)

-- tyTerm :: Type loc v -> TermF loc var (Ann () ) -> TyTerm loc var
tyTerm :: Type loc v -> TermF loc v (Fix (Ann (Type loc v) (TermF loc v))) -> TyTerm loc v
tyTerm ty x = TyTerm $ Fix $ Ann ty x

-- | 'varE' @loc x@ constructs a variable whose name is @x@ with source code at @loc@.
tyVarE :: Type loc var -> loc -> var -> TyTerm loc var
tyVarE ty loc var =  tyTerm ty $ Var loc var

-- | 'appE' @loc a b@ constructs an application of @a@ to @b@ with source code at @loc@.
tyAppE :: Type loc v -> loc -> TyTerm loc v -> TyTerm loc v -> TyTerm loc v
tyAppE ty loc (TyTerm l) (TyTerm r) = tyTerm ty $ App loc l r

-- | 'lamE' @loc x e@ constructs an abstraction of @x@ over @e@ with source code at @loc@.
tyLamE :: Type loc v -> loc -> v -> TyTerm loc v -> TyTerm loc v
tyLamE ty loc x (TyTerm e) = tyTerm ty $ Lam loc x e

-- | 'letE' @loc binds e@ constructs a binding of @binds@ in @e@ with source code at @loc@.
-- No recursive bindings.
tyLetE :: Type loc v -> loc -> [Bind loc v (TyTerm loc v)] -> TyTerm loc v -> TyTerm loc v
tyLetE ty loc binds (TyTerm e) = tyTerm ty $ Let loc (fmap (fmap unTyTerm) binds) e

-- | 'letRecE' @loc binds e@ constructs a recursive binding of @binds@ in @e@ with source code at @loc@.
tyLetRecE :: Type loc v -> loc -> [Bind loc v (TyTerm loc v)] -> TyTerm loc v -> TyTerm loc v
tyLetRecE ty loc binds (TyTerm e) = tyTerm ty $ LetRec loc (fmap (fmap unTyTerm) binds) e

-- | 'assertTypeE' @loc term ty@ constructs assertion of the type @ty@ to @term@.
tyAssertTypeE :: loc -> TyTerm loc v -> Type loc v -> TyTerm loc v
tyAssertTypeE loc (TyTerm a) ty = tyTerm ty $ AssertType loc a ty

-- | 'caseE' @loc expr alts@ constructs case alternatives expression.
tyCaseE :: Type loc v -> loc -> TyTerm loc v -> [CaseAlt loc v (TyTerm loc v)] -> TyTerm loc v
tyCaseE ty loc (TyTerm e) alts = tyTerm ty $ Case loc e $ fmap (fmap unTyTerm) alts

-- | 'constrE' @loc ty tag arity@ constructs constructor tag expression.
tyConstrE :: loc -> Type loc v -> v -> Int -> TyTerm loc v
tyConstrE loc ty tag arity = tyTerm ty $ Constr loc ty tag arity

-- | 'bottomE' @loc@ constructs bottom value.
tyBottomE :: Type loc v -> loc -> TyTerm loc v
tyBottomE ty loc = tyTerm ty $ Bottom loc

instance LocFunctor TyTerm where
  mapLoc f (TyTerm x) = TyTerm $ cata go x
    where
      go (Ann annTy term) = Fix $ Ann (mapLoc f annTy) $ case term of
        Var loc v    -> Var (f loc) v
        App loc a b  -> App (f loc) a b
        Lam loc v a  -> Lam (f loc) v a
        Let loc vs a -> Let (f loc) (fmap (\b ->  b { bind'loc = f $ bind'loc b }) vs) a
        LetRec loc vs a -> LetRec (f loc) (fmap (\b ->  b { bind'loc = f $ bind'loc b }) vs) a
        AssertType loc r sig -> AssertType (f loc) r (mapLoc f sig)
        Constr loc ty v arity -> Constr (f loc) (mapLoc f ty) v arity
        Case loc e alts -> Case (f loc) e (fmap (mapAlt f) alts)
        Bottom loc -> Bottom (f loc)

      mapAlt g alt@CaseAlt{..} = alt
        { caseAlt'loc  = g caseAlt'loc
        , caseAlt'args = fmap (mapTyped g) caseAlt'args
        , caseAlt'constrType = mapLoc g caseAlt'constrType
        }

      mapTyped g (Typed ty val) = Typed (mapLoc g ty) val



instance CanApply TyTerm where
  apply subst term = mapType (apply subst) term

mapType :: (Type loc var -> Type loc var) -> TyTerm loc var -> TyTerm loc var
mapType f (TyTerm x) = TyTerm $ cata go x
    where
      go (Ann ty term) = Fix $ Ann (f ty) term

