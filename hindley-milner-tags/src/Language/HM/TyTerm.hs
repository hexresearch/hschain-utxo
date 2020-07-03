-- | This module contains type annotations for terms of the language.
module Language.HM.TyTerm(
    Ann(..)
  , TyTerm(..)
  , tyVarE
  , tyPrimE
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

import Control.Arrow

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
newtype TyTerm prim loc v = TyTerm { unTyTerm :: Fix (Ann (Type loc v) (TermF prim loc v)) }
  deriving (Show, Eq)

-- tyTerm :: Type loc v -> TermF loc var (Ann () ) -> TyTerm loc var
tyTerm :: Type loc v -> TermF prim loc v (Fix (Ann (Type loc v) (TermF prim loc v))) -> TyTerm prim loc v
tyTerm ty x = TyTerm $ Fix $ Ann ty x

-- | 'varE' @loc x@ constructs a variable whose name is @x@ with source code at @loc@.
tyVarE :: Type loc var -> loc -> var -> TyTerm prim loc var
tyVarE ty loc var =  tyTerm ty $ Var loc var

-- | 'varE' @loc x@ constructs a variable whose name is @x@ with source code at @loc@.
tyPrimE :: Type loc var -> loc -> prim -> TyTerm prim loc var
tyPrimE ty loc prim =  tyTerm ty $ Prim loc prim

-- | 'appE' @loc a b@ constructs an application of @a@ to @b@ with source code at @loc@.
tyAppE :: Type loc v -> loc -> TyTerm prim loc v -> TyTerm prim loc v -> TyTerm prim loc v
tyAppE ty loc (TyTerm l) (TyTerm r) = tyTerm ty $ App loc l r

-- | 'lamE' @loc x e@ constructs an abstraction of @x@ over @e@ with source code at @loc@.
tyLamE :: Type loc v -> loc -> v -> TyTerm prim loc v -> TyTerm prim loc v
tyLamE ty loc x (TyTerm e) = tyTerm ty $ Lam loc x e

-- | 'letE' @loc binds e@ constructs a binding of @binds@ in @e@ with source code at @loc@.
-- No recursive bindings.
tyLetE :: Type loc v -> loc -> [Bind loc v (TyTerm prim loc v)] -> TyTerm prim loc v -> TyTerm prim loc v
tyLetE ty loc binds (TyTerm e) = tyTerm ty $ Let loc (fmap (fmap unTyTerm) binds) e

-- | 'letRecE' @loc binds e@ constructs a recursive binding of @binds@ in @e@ with source code at @loc@.
tyLetRecE :: Type loc v -> loc -> [Bind loc v (TyTerm prim loc v)] -> TyTerm prim loc v -> TyTerm prim loc v
tyLetRecE ty loc binds (TyTerm e) = tyTerm ty $ LetRec loc (fmap (fmap unTyTerm) binds) e

-- | 'assertTypeE' @loc term ty@ constructs assertion of the type @ty@ to @term@.
tyAssertTypeE :: loc -> TyTerm prim loc v -> Type loc v -> TyTerm prim loc v
tyAssertTypeE loc (TyTerm a) ty = tyTerm ty $ AssertType loc a ty

-- | 'caseE' @loc expr alts@ constructs case alternatives expression.
tyCaseE :: Type loc v -> loc -> TyTerm prim loc v -> [CaseAlt loc v (TyTerm prim loc v)] -> TyTerm prim loc v
tyCaseE ty loc (TyTerm e) alts = tyTerm ty $ Case loc e $ fmap (fmap unTyTerm) alts

-- | 'constrE' @loc ty tag arity@ constructs constructor tag expression.
tyConstrE :: loc -> Type loc v -> v -> Int -> TyTerm prim loc v
tyConstrE loc ty tag arity = tyTerm ty $ Constr loc ty tag arity

-- | 'bottomE' @loc@ constructs bottom value.
tyBottomE :: Type loc v -> loc -> TyTerm prim loc v
tyBottomE ty loc = tyTerm ty $ Bottom loc

instance LocFunctor (TyTerm prim) where
  mapLoc f (TyTerm x) = TyTerm $ cata go x
    where
      go (Ann annTy term) = Fix $ Ann (mapLoc f annTy) $ case term of
        Var loc v    -> Var (f loc) v
        Prim loc p   -> Prim (f loc) p
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

      mapTyped g (Typed ty val) = Typed (mapLoc g ty) (first g val)

instance TypeFunctor (TyTerm prim) where
  mapType f (TyTerm x) = TyTerm $ cata go x
    where
      go (Ann ty term) = Fix $ Ann (f ty) $
        case term of
          Constr loc cty cons arity -> Constr loc (f cty) cons arity
          Case loc e alts          -> Case loc e $ fmap applyAlt alts
          other                    -> other

      applyAlt alt@CaseAlt{..} = alt
        { caseAlt'args       = fmap applyTyped caseAlt'args
        , caseAlt'constrType = f caseAlt'constrType
        }

      applyTyped ty@Typed{..} = ty { typed'type = f typed'type }

instance CanApply (TyTerm prim) where
  apply subst term = mapType (apply subst) term


