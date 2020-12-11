-- | This module contains the abstract syntax tree of the term language.
module Type.Check.HM.Term(
    Term(..)
  , TermF(..)
  , CaseAlt(..)
  , Bind(..)
  , varE
  , primE
  , appE
  , lamE
  , letE
  , letRecE
  , assertTypeE
  , caseE
  , constrE
  , bottomE
  , freeVars
) where

import Control.Arrow

import Data.Data
import Data.Fix
import Data.Set (Set)
import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving

import Type.Check.HM.Subst
import Type.Check.HM.Type

import qualified Data.Set as S

-- | Term functor. The arguments are
-- @loc@ for source code locations, @v@ for variables, @r@ for recurion.
data TermF prim loc v r
    = Var loc v                       -- ^ Variables.
    | Prim loc prim                   -- ^ Primitives.
    | App loc r r                     -- ^ Applications.
    | Lam loc v r                     -- ^ Abstractions.
    | Let loc (Bind loc v r) r        -- ^ Let bindings.
    | LetRec loc [Bind loc v r] r     -- ^ Recursive  let bindings
    | AssertType loc r (Type loc v)   -- ^ Assert type.
    | Case loc r [CaseAlt loc v r]    -- ^ case alternatives
    | Constr loc (Type loc v) v       -- ^ constructor with tag and arity, also we should provide the type
                                      --   of constructor as a function for a type-checker
    | Bottom loc                      -- ^ value of any type that means failed programm.
    deriving (Show, Eq, Functor, Foldable, Traversable, Data)

-- | Case alternatives
data CaseAlt loc v a = CaseAlt
  { caseAlt'loc   :: loc
  -- ^ source code location
  , caseAlt'tag   :: v
  -- ^ tag of the constructor
  , caseAlt'args  :: [Typed loc v (loc, v)]
  -- ^ arguments of the pattern matching
  , caseAlt'constrType :: Type loc v
  -- ^ type of the result expression, they should be the same for all cases
  , caseAlt'rhs   :: a
  -- ^ right-hand side of the case-alternative
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Data)

-- | Local variable definition.
--
-- > let lhs = rhs in ...
data Bind loc var r = Bind
  { bind'loc :: loc             -- ^ Source code location
  , bind'lhs :: var             -- ^ Variable name
  , bind'rhs :: r               -- ^ Definition (right-hand side)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Data)

$(deriveShow1 ''TermF)
$(deriveEq1   ''TermF)
$(deriveOrd1  ''TermF)
$(deriveShow1 ''Bind)
$(deriveEq1   ''Bind)
$(deriveOrd1  ''Bind)
$(deriveShow1 ''CaseAlt)
$(deriveEq1   ''CaseAlt)
$(deriveOrd1  ''CaseAlt)

-- | The type of terms.
newtype Term prim loc v = Term { unTerm :: Fix (TermF prim loc v) }
  deriving (Show, Eq, Data)

instance Functor (Term prim loc) where
  fmap f (Term x) =  Term $ foldFix go x
    where
      go = \case
        Var loc v    -> Fix $ Var loc (f v)
        Prim loc p   -> Fix $ Prim loc p
        App loc a b  -> Fix $ App loc a b
        Lam loc v a  -> Fix $ Lam loc (f v) a
        Let loc v a  -> Fix $ Let loc (v { bind'lhs = f $ bind'lhs v }) a
        LetRec loc vs a -> Fix $ LetRec loc (fmap (\b ->  b { bind'lhs = f $ bind'lhs b }) vs) a
        AssertType loc r sig -> Fix $ AssertType loc r (fmap f sig)
        Case loc a alts -> Fix $ Case loc a $ fmap (mapAlt f) alts
        Constr loc ty v -> Fix $ Constr loc (fmap f ty) (f v)
        Bottom loc -> Fix $ Bottom loc

      mapAlt g alt@CaseAlt{..} = alt
        { caseAlt'tag  = f caseAlt'tag
        , caseAlt'args = fmap (mapTyped g) caseAlt'args
        , caseAlt'constrType = fmap f caseAlt'constrType
        }

      mapTyped g Typed{..} = Typed (fmap f typed'type) (second g typed'value)

-- | 'varE' @loc x@ constructs a variable whose name is @x@ with source code at @loc@.
varE :: loc -> var -> Term prim loc var
varE loc = Term . Fix . Var loc

primE :: loc -> prim -> Term prim loc var
primE loc = Term . Fix . Prim loc

-- | 'appE' @loc a b@ constructs an application of @a@ to @b@ with source code at @loc@.
appE :: loc -> Term prim loc v -> Term prim loc v -> Term prim loc v
appE loc (Term l) (Term r) = Term $ Fix $ App loc l r

-- | 'lamE' @loc x e@ constructs an abstraction of @x@ over @e@ with source code at @loc@.
lamE :: loc -> v -> Term prim loc v -> Term prim loc v
lamE loc x (Term e) = Term $ Fix $ Lam loc x e

-- | 'letE' @loc binds e@ constructs a binding of @binds@ in @e@ with source code at @loc@.
-- No recursive bindings.
letE :: loc -> Bind loc v (Term prim loc v) -> Term prim loc v -> Term prim loc v
letE loc bind (Term e) = Term $ Fix $ Let loc (fmap unTerm bind) e

-- | 'letRecE' @loc binds e@ constructs a recursive binding of @binds@ in @e@ with source code at @loc@.
letRecE :: loc -> [Bind loc v (Term prim loc v)] -> Term prim loc v -> Term prim loc v
letRecE loc binds (Term e) = Term $ Fix $ LetRec loc (fmap (fmap unTerm) binds) e

-- | 'assertTypeE' @loc term ty@ constructs assertion of the type @ty@ to @term@.
assertTypeE :: loc -> Term prim loc v -> Type loc v -> Term prim loc v
assertTypeE loc (Term a) ty = Term $ Fix $ AssertType loc a ty

-- | 'caseE' @loc expr alts@ constructs case alternatives expression.
caseE :: loc -> Term prim loc v -> [CaseAlt loc v (Term prim loc v)] -> Term prim loc v
caseE loc (Term e) alts = Term $ Fix $ Case loc e $ fmap (fmap unTerm) alts

-- | 'constrE' @loc ty tag arity@ constructs constructor tag expression.
constrE :: loc -> Type loc v -> v -> Term prim loc v
constrE loc ty tag = Term $ Fix $ Constr loc ty tag

-- | 'bottomE' @loc@ constructs bottom value.
bottomE :: loc -> Term prim loc v
bottomE loc = Term $ Fix $ Bottom loc

--------------------------------------------------------------------------------

instance HasLoc (Term prim loc v) where
  type Loc (Term prim loc v) = loc

  getLoc (Term (Fix x)) = case x of
    Var loc _   -> loc
    Prim loc _  -> loc
    App loc _ _ -> loc
    Lam loc _ _ -> loc
    Let loc _ _ -> loc
    LetRec loc _ _ -> loc
    AssertType loc _ _ -> loc
    Constr loc _ _ -> loc
    Case loc _ _ -> loc
    Bottom loc -> loc

instance LocFunctor (Term prim) where
  mapLoc f (Term x) = Term $ foldFix go x
    where
      go = \case
        Var loc v    -> Fix $ Var (f loc) v
        Prim loc p   -> Fix $ Prim (f loc) p
        App loc a b  -> Fix $ App (f loc) a b
        Lam loc v a  -> Fix $ Lam (f loc) v a
        Let loc v a  -> Fix $ Let (f loc) (v { bind'loc = f $ bind'loc v }) a
        LetRec loc vs a -> Fix $ LetRec (f loc) (fmap (\b ->  b { bind'loc = f $ bind'loc b }) vs) a
        AssertType loc r sig -> Fix $ AssertType (f loc) r (mapLoc f sig)
        Constr loc ty v -> Fix $ Constr (f loc) (mapLoc f ty) v
        Case loc e alts -> Fix $ Case (f loc) e (fmap mapAlt alts)
        Bottom loc -> Fix $ Bottom (f loc)

      mapAlt alt@CaseAlt{..} = alt
        { caseAlt'loc  = f caseAlt'loc
        , caseAlt'args = fmap mapTyped caseAlt'args
        , caseAlt'constrType = mapLoc f caseAlt'constrType
        }

      mapTyped (Typed ty val) = Typed (mapLoc f ty) (first f val)

-- | Get free variables of the term.
freeVars :: Ord v => Term lprim oc v -> Set v
freeVars = foldFix go . unTerm
  where
    go = \case
      Var    _ v          -> S.singleton v
      Prim   _ _          -> mempty
      App    _ a b        -> mappend a b
      Lam    _ v a        -> S.delete v a
      Let    _ bind body  -> let lhs = S.singleton $ bind'lhs bind
                             in  mappend (bind'rhs bind)
                                         (body `S.difference` lhs)
      LetRec _ binds body -> let lhs = S.fromList $ fmap bind'lhs binds
                             in  (mappend (freeBinds binds) body) `S.difference` lhs
      AssertType _ a _    -> a
      Case _ e alts       -> mappend e (foldMap freeVarAlts alts)
      Constr _ _ _        -> mempty
      Bottom _            -> mempty

    freeBinds = foldMap bind'rhs

    freeVarAlts CaseAlt{..} = caseAlt'rhs `S.difference` (S.fromList $ fmap (snd . typed'value) caseAlt'args)

instance TypeFunctor (Term prim) where
  mapType f (Term term) = Term $ foldFix go term
    where
      go = \case
        Constr loc ty cons       -> Fix $ Constr loc (f ty) cons
        Case loc e alts          -> Fix $ Case loc e $ fmap applyAlt alts
        other                    -> Fix other

      applyAlt alt@CaseAlt{..} = alt
        { caseAlt'args       = fmap applyTyped caseAlt'args
        , caseAlt'constrType = f caseAlt'constrType
        }

      applyTyped ty@Typed{..} = ty { typed'type = f typed'type }

instance CanApply (Term prim) where
  apply subst term = mapType (apply subst) term

