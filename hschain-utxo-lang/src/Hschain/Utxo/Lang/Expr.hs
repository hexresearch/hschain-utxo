{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module defines AST for the language
module Hschain.Utxo.Lang.Expr(
    Loc
  , noLoc
  , VarName(..)
  , ConsName(..)
  , consToVarName
  , varToConsName
  , Lang
  , E(..)
  , Type
  , TypeError
  , Signature
  , CaseExpr(..)
  , Binds(..)
  , Bind(..)
  , Alt(..)
  , Pat(..)
  , patNames
  , Rhs(..)
  , Guard(..)
  , QuoteType(..)
  , TypeContext
  , sortBinds
  , freeVars
  , freeVarsPat
  , freeVarsAlt
  , bindNames
  , bindNamesLhs
  , bindAlts
  , getBindsNames
  , secretVar
  , BoolExprResult(..)
  , mapDeclsM
  , fromParserLoc
  , emptyTypeContext
  , ToLang(..)
) where

import Hex.Common.Aeson

import Control.Applicative

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Data
import Data.Fix
import Data.Function (on)
import Data.Foldable
import Data.Graph
import Data.Int
import Data.Map.Strict (Map)
import Data.String
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))
import GHC.Generics

import Text.Show.Deriving

import HSChain.Crypto.Classes (ByteRepr(..))
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types              (Script(..))
import Hschain.Utxo.Lang.Core.Types         (Prim(..))
import qualified Type.Check.HM as H
import qualified Language.Haskell.Exts.SrcLoc as Hask

import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Vector as V

type Loc = Hask.SrcSpanInfo
type Type = H.Type Loc Text
type TypeError = H.TypeError Loc Text
type Signature = H.Signature Loc Text

fromParserLoc :: Hask.SrcLoc -> Loc
fromParserLoc loc = Hask.toSrcInfo loc [] loc

instance H.DefLoc Hask.SrcSpanInfo where
  defLoc = Hask.noSrcSpan

-- | Unknown source code location.
noLoc :: Loc
noLoc = Hask.noSrcSpan

-- | Name of the variable.
data VarName = VarName
  { varName'loc   :: !Loc   -- ^ source code location
  , varName'name  :: !Text  -- ^ variable name
  } deriving (Show, Data, Typeable)

instance Eq VarName where
  (==) = (==) `on` varName'name

instance Ord VarName where
  compare = compare `on` varName'name

instance IsString VarName where
  fromString = VarName noLoc . fromString

-- | Name of the constructor
data ConsName = ConsName
  { consName'loc  :: !Loc   -- ^ source code location
  , consName'name :: !Text  -- ^ constructor name
  } deriving (Show, Data, Typeable)

instance Eq ConsName where
  (==) = (==) `on` consName'name

instance Ord ConsName where
  compare = compare `on` consName'name


instance IsString ConsName where
  fromString = ConsName noLoc . fromString

-- | Convert constructor name to variable name
consToVarName :: ConsName -> VarName
consToVarName (ConsName loc name) = VarName loc name

-- | Convert variable name to constructor name
varToConsName :: VarName -> ConsName
varToConsName VarName{..} = ConsName varName'loc varName'name

-- | Pattern matching elements (in the arguments or in cases)
data Pat
  = PVar Loc VarName          -- ^ simple variable (anything matches)
  | PPrim Loc Prim            -- ^ constant value (match if equals)
  | PCons Loc ConsName [Pat]  -- ^ concrete constructor with argument patterns
  | PTuple Loc [Pat]          -- ^ tuple with list of arguments
  | PWildCard Loc             -- ^ wildcard (anything matches and value is discarded after match)
  deriving (Show, Eq, Ord, Data, Typeable)

instance IsString Pat where
  fromString = PVar noLoc . fromString

-- | Type context for inference algorithm
type TypeContext = H.Context Loc Text

emptyTypeContext :: TypeContext
emptyTypeContext = H.Context mempty

-- | Alternatives for declarations right-hand-sides.
-- Because of pattern matching we can have several alternatives
-- for a single declaration.
data Alt a = Alt
  { alt'pats  :: [Pat]               -- ^ arguments of the function
  , alt'expr  :: Rhs a               -- ^ right-hand side of the declaration
  , alt'where :: Maybe (Binds a)     -- ^ 'where'-declarations (definitions local to the function)
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

-- | Right-hand side of the function definition.
data Rhs a
  = UnguardedRhs a         -- ^ No-guards
  | GuardedRhs [Guard a]   -- ^ with guards
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

-- | Guard for right hand-side. RHs is executed if guard's predicate evaluates to True.
data Guard a = Guard
  { guard'predicate :: a  -- ^ guard predicate expression
  , guard'rhs       :: a  -- ^ right-hand side expression
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

data Binds a = Binds
  { binds'types :: Map VarName Signature
  , binds'decls :: [Bind a]
  }
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable, Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid (Binds a)

mapDeclsM :: Monad m => (Bind a -> m (Bind b)) -> Binds a -> m (Binds b)
mapDeclsM f bs = do
  decls <- mapM f $ binds'decls bs
  return $ bs { binds'decls = decls }

-- | Bind of variable
data Bind a
  = FunBind
      { bind'name  :: VarName          -- ^ name of the value
      , bind'alts  :: [Alt a]          -- ^ definitions of the value
      }
  | PatBind
      { bind'pat   :: Pat
      , bind'alt   :: Alt a
      }
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

bindNamesLhs :: Bind a -> Set VarName
bindNamesLhs = \case
  FunBind v _ -> Set.singleton v
  PatBind p _ -> freeVarsPat p

-- | Main tpye for expressions.
-- It's defined in fix-point style (See package data-fix).
type Lang = Fix E

-- | Functor for expression.
data E a
  -- lambda calculus
  = Var Loc VarName
  -- ^ variables
  | Apply Loc a a
  -- ^ function application (@f arg@)
  | InfixApply Loc a VarName a
  -- ^ infix binary function application (@a + b@)
  | Lam Loc Pat a
  -- ^ lambda-abstraction (@\pat -> expr@)
  | LamList Loc [Pat] a
  -- ^ lambda abstraction with list of arguments (@\pat1 pat2 pat3 -> expr@)
  | Let Loc (Binds a) a
  -- ^ local bindings or let-expression: (@let v = a in expr@)
  | PrimLet Loc [(VarName, a)] a
  -- ^ Simplified Let-expression. All binding right hand sides are rendered to a single expression
  -- (functions are grouped, pattern-matches transformed to simple case-expressions)
  | Ascr Loc a Signature
  -- ^ Type specification in the body of expression (@a :: Type@)
  -- case
  | Cons Loc ConsName (Vector a)
  -- ^ Constructor application to the list of arguments (@Cons a as@)
  | CaseOf Loc a [CaseExpr a]
  -- ^ Case-expression (@case expr of alternatives@)
  -- records
  | RecConstr Loc ConsName [(VarName, a)]
  -- ^ Record constructor with named fields (@Record { field1 = expr1; field2 = expr2 }@)
  | RecUpdate Loc a [(VarName, a)]
  -- ^ Record field modifier (@rec { field = val }@)
  -- Alternatives
  | AltE Loc a a
  -- ^ Low-level representation for case-alternatives (not visible to user)
  | FailCase Loc
  -- ^ Low-level representation for case
  -- or pattern matching failure (not visible to user)
  -- primitives
  | PrimE Loc Prim
  -- ^ primitive values (constants of the language)
  -- logic
  | If Loc a a a
  -- ^ if-expressions (@if cond then a else b@)
  | Tuple Loc (Vector a)
  -- ^ Tuple constructor with list of arguments (@(a, b, c)@)
  | List Loc (Vector a)
  -- ^ List constructor with list of arguments (@[a, b, c]@)
  | NegApp Loc a
  -- ^ unary negation sign
  | AntiQuote Loc (Maybe QuoteType) VarName
  -- ^ reference to external vriables (used in quasi quoting)
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

-- | Types that we can inline to quasi-quoted code
data QuoteType
  = IntQ
  | BoolQ
  | TextQ
  | BytesQ
  | SigmaQ
  | PublicKeyQ
  | ScriptQ
  | ListQ QuoteType
  | TupleQ [QuoteType]
  deriving (Show, Eq, Data)

-- | Case-alternative expression
data CaseExpr a
  = CaseExpr
      { caseExpr'lhs :: Pat  -- ^ pattern to check
      , caseExpr'rhs :: a    -- ^ right-hand side expression
      }
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

-- | Hack to define special names (like record fields or modifiers, or constants for type-inference)
secretVar :: Text -> Text
secretVar = flip mappend "___"

-- | Result of the script can be boolean constant or sigma-expression
-- that user have to prove.
data BoolExprResult
  = ConstBool Bool
  | SigmaResult (Sigma ProofInput)
  deriving (Show, Eq)

instance ToJSON BoolExprResult where
  toJSON = \case
    ConstBool b -> object ["bool"  .= b]
    SigmaResult s -> object ["sigma" .= s]

instance FromJSON BoolExprResult where
  parseJSON = withObject "BoolExprResult" $ \obj ->
        (ConstBool <$> obj .: "bool")
    <|> (SigmaResult <$> obj .: "sigma")

--------------------------------
-- instances

instance H.HasLoc VarName where
  type Loc VarName = Loc
  getLoc (VarName loc _) = loc

instance H.HasLoc ConsName where
  type Loc ConsName = Loc
  getLoc (ConsName loc _) = loc

instance H.HasLoc Lang where
  type Loc Lang = Loc
  getLoc (Fix expr) = H.getLoc expr

instance H.HasLoc Pat where
  type Loc Pat = Loc
  getLoc = \case
    PVar loc _ -> loc
    PPrim loc _ -> loc
    PCons loc _ _ -> loc
    PTuple loc _ -> loc
    PWildCard loc -> loc

instance Show a => H.HasLoc (E a) where
  type Loc (E a) = Loc
  getLoc = \case
    Var loc _ -> loc
    Apply loc _ _ -> loc
    InfixApply loc _ _ _ -> loc
    Lam loc _ _ -> loc
    LamList loc _ _ -> loc
    Let loc _ _ -> loc
    PrimLet loc _ _ -> loc
    Ascr loc _ _ -> loc
    -- case-expr
    Cons loc _ _ -> loc
    CaseOf loc _ _ -> loc
    -- record
    RecConstr loc _ _ -> loc
    RecUpdate loc _ _ -> loc
    -- primitives
    PrimE loc _ -> loc
    -- logic
    If loc _ _ _ -> loc
    -- tuples
    Tuple loc _ -> loc
    -- lists
    List loc _ -> loc
    -- unary negation
    NegApp loc _ -> loc
    -- operations
    AltE loc _ _ -> loc
    FailCase loc -> loc
    AntiQuote loc _ _ -> loc

instance H.HasLoc a => H.HasLoc (Alt a) where
  type Loc (Alt a) = H.Loc a
  getLoc = H.getLoc . alt'expr

instance H.HasLoc a => H.HasLoc (Rhs a) where
  type Loc (Rhs a) = H.Loc a
  getLoc = \case
    UnguardedRhs a -> H.getLoc a
    GuardedRhs  as -> case as of
      a:_ -> H.getLoc $ guard'rhs a
      []  -> error "Empty guard"

instance  H.HasLoc (Bind a) where
  type Loc (Bind a) = Loc
  getLoc = H.getLoc . bind'name

-------------------------------------------------------------------

-- | Get free0variables for expression
freeVars :: Lang -> Set VarName
freeVars = foldFix $ \case
  Var _ v         -> Set.singleton v
  InfixApply _ a v b -> Set.singleton v <> a <> b
  Apply _ a b      -> a <> b
  Lam _ v a        -> a `Set.difference`  freeVarsPat v
  LamList _ vs a   -> a `Set.difference` (foldMap freeVarsPat vs)
  Let _ bg a       -> freeLet bg a
  PrimLet _ bg a   -> (a `Set.difference` getPrimBgNames bg) <> freeVarsPrimBg bg
  Ascr _ a _       -> a
  Cons _ _ vs      -> mconcat $ V.toList vs
  CaseOf _ a alts  -> mappend a (foldMap freeCaseExpr alts)
  RecConstr _ _ ts -> mconcat $ fmap snd ts
  RecUpdate _ a ts -> mconcat $ a : fmap snd ts
  PrimE _ _        -> Set.empty
  If _ a b c       -> mconcat [a, b, c]
  Tuple _ vs       -> fold $ V.toList vs
  List _ vs        -> fold $ V.toList vs
  NegApp _ a       -> a
  AltE _ a b       -> mappend a b
  FailCase _       -> Set.empty
  AntiQuote _ _ v  -> Set.singleton v
  where
    freeLet bg a = (a `Set.difference` letBounds) <> letFrees
      where
        go (frees, bounds) bind =
          (frees <> (rhs `Set.difference` bounds), bounds <> lhs)
          where
            lhs = bindNamesLhs bind
            rhs = bindFreeVars bind

        (letFrees, letBounds) = L.foldl' go (Set.empty, Set.empty) (binds'decls bg)

    getPrimBgNames :: [(VarName, a)] -> Set VarName
    getPrimBgNames bs = Set.fromList $ fmap fst bs

    freeCaseExpr CaseExpr{..} = caseExpr'rhs `Set.difference` (freeVarsPat caseExpr'lhs)

    freeVarsPrimBg = foldMap snd

freeVarsBinds :: Binds (Set VarName) -> Set VarName
freeVarsBinds bg = (foldMap (foldMap freeVarsAlt . bindAlts) . binds'decls) bg

bindFreeVars :: Bind (Set VarName) -> Set VarName
bindFreeVars b = foldMap freeVarsAlt $ bindAlts b

getBindsNames :: Binds a -> Set VarName
getBindsNames = foldMap bindNamesLhs . binds'decls

freeVarsRhs :: Rhs (Set VarName) -> Set VarName
freeVarsRhs = \case
  UnguardedRhs a -> a
  GuardedRhs as -> foldMap freeVarsGuard as
  where
    freeVarsGuard Guard{..} = guard'predicate <> guard'rhs

bindNames :: Bind a -> [VarName]
bindNames = \case
  FunBind{..} -> [bind'name]
  PatBind{..} -> patNames bind'pat

bindAlts :: Bind a -> [Alt a]
bindAlts = \case
  FunBind{..} -> bind'alts
  PatBind{..} -> [bind'alt]

patNames :: Pat -> [VarName]
patNames = Set.toList . freeVarsPat

freeVarsPat :: Pat -> Set VarName
freeVarsPat = \case
  PVar _ name -> Set.singleton name
  PPrim _ _ -> Set.empty
  PCons _ _ vs -> foldMap freeVarsPat vs
  PTuple _ vs -> foldMap freeVarsPat vs
  PWildCard _ -> Set.empty

freeVarsAlt :: Alt (Set VarName) -> Set VarName
freeVarsAlt Alt{..} =
  ((freeVarsRhs alt'expr <> foldMap freeVarsBinds alt'where)
  `Set.difference` foldMap getBindsNames alt'where)
  `Set.difference` (foldMap freeVarsPat alt'pats)

-------------------------------------------------------------------

-- | Reorders binds by dependencies. First go binds with no deps then those
-- that are dependent on them and so forth.
sortBinds :: Binds Lang -> Binds Lang
sortBinds b = b { binds'decls = onDecls $ binds'decls b }
   where
     onDecls = L.nub . (flattenSCC =<<) . stronglyConnComp . (toNode =<<)

     toNode s = case s of
       FunBind{..} -> pure $ (s, bind'name, Set.toList $ foldMap (freeVarsAlt . fmap freeVars) bind'alts)
       PatBind{..} -> fmap (\name -> (s, name, Set.toList $ (freeVarsAlt . fmap freeVars) bind'alt )) $ Set.toList $ freeVarsPat bind'pat

-------------------------------------------------------------------

class ToLang a where
  toLang :: Loc -> a -> Lang

  toLangExpr :: Loc -> a -> E Lang
  toLangExpr loc a = unFix $ toLang loc a

instance ToLang Text where
  toLang loc txt = toPrim loc $ PrimText txt

instance ToLang ByteString where
  toLang loc bs = toPrim loc $ PrimBytes bs

instance ToLang PublicKey where
  toLang loc key = toPrim loc $ PrimBytes $ encodeToBS key

instance ToLang Script where
  toLang loc (Script bs) = toPrim loc $ PrimBytes bs

instance ToLang Bool where
  toLang loc b = toPrim loc $ PrimBool b

instance ToLang (Sigma ProofInput) where
  toLang loc sig = toPrim loc $ PrimSigma sig

instance ToLang Int where
  toLang loc n = toPrim loc $ PrimInt $ fromIntegral n

instance ToLang Int64 where
  toLang loc n = toPrim loc $ PrimInt n

instance ToLang a => ToLang [a] where
  toLang loc vals = Fix $ List loc $ V.fromList $ fmap (toLang loc) vals

instance (ToLang a, ToLang b) => ToLang (a, b) where
  toLang loc (a, b) = Fix $ Tuple loc $ V.fromList [toLang loc a, toLang loc b]

instance (ToLang a, ToLang b, ToLang c) => ToLang (a, b, c) where
  toLang loc (a, b, c) = Fix $ Tuple loc $ V.fromList [toLang loc a, toLang loc b, toLang loc c]

instance (ToLang a, ToLang b, ToLang c, ToLang d) => ToLang (a, b, c, d) where
  toLang loc (a, b, c, d) = Fix $ Tuple loc $ V.fromList [toLang loc a, toLang loc b, toLang loc c, toLang loc d]

toPrim :: Loc -> Prim -> Lang
toPrim loc p = Fix $ PrimE loc p

-------------------------------------------------------------------

$(deriveEq1   ''Alt)
$(deriveOrd1  ''Alt)
$(deriveShow1 ''Alt)
$(deriveEq1   ''Rhs)
$(deriveOrd1  ''Rhs)
$(deriveShow1 ''Rhs)
$(deriveEq1   ''Guard)
$(deriveOrd1  ''Guard)
$(deriveShow1 ''Guard)
$(deriveEq1   ''Bind)
$(deriveOrd1  ''Bind)
$(deriveShow1 ''Bind)
$(deriveEq1   ''Binds)
$(deriveOrd1  ''Binds)
$(deriveShow1 ''Binds)
$(deriveEq1   ''CaseExpr)
$(deriveOrd1  ''CaseExpr)
$(deriveShow1 ''CaseExpr)
$(deriveEq1   ''E)
$(deriveShow1 ''E)


