{-# LANGUAGE QuantifiedConstraints #-}
-- | This module defines AST for the language
module Hschain.Utxo.Lang.Expr where

import Hex.Common.Text

import Control.Applicative
import Control.DeepSeq (NFData)

import Codec.Serialise

import Data.Aeson

import Data.Fix
import Data.Function (on)
import Data.Foldable
import Data.Graph
import Data.Int
import Data.Map.Strict (Map)
import Data.Maybe
import Data.String
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Generics

import Text.Show.Deriving

import HSChain.Crypto.Classes.Hash (CryptoHashable(..),genericHashStep)
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Sigma.EllipticCurve (hashDomain)

import qualified Language.HM as H
import qualified Language.Haskell.Exts.SrcLoc as Hask

import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Const as Const

type Loc = Hask.SrcSpanInfo
type Type = H.Type Loc Text
type TypeError = H.TypeError Loc Text
type Signature = H.Signature Loc Text

-- | Unknown source code location.
noLoc :: Loc
noLoc = Hask.noSrcSpan

-- | Context of user-defined types
data UserTypeCtx = UserTypeCtx
  { userTypeCtx'types      :: Map VarName  UserType          -- ^ User-defined types
  , userTypeCtx'constrs    :: Map ConsName ConsInfo          -- ^ Map from constructor names to it's low-level data, for further compilation
  , userTypeCtx'recConstrs :: Map ConsName RecordFieldOrder  -- ^ Order of fields for records
  , userTypeCtx'recFields  :: Map Text     (ConsName, RecordFieldOrder)  -- ^ Maps record single field to the full lists of fields
  } deriving (Show, Eq)

instance Semigroup UserTypeCtx where
  UserTypeCtx a1 b1 c1 d1 <> UserTypeCtx a2 b2 c2 d2 = UserTypeCtx (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Monoid UserTypeCtx where
  mempty = UserTypeCtx mempty mempty mempty mempty

setupUserTypeInfo :: UserTypeCtx -> UserTypeCtx
setupUserTypeInfo = setupConsInfo . setupUserRecords

setupConsInfo :: UserTypeCtx -> UserTypeCtx
setupConsInfo ctx = ctx { userTypeCtx'constrs = getConsInfoMap $ userTypeCtx'types ctx }
  where
    getConsInfoMap  :: Map VarName UserType -> Map ConsName ConsInfo
    getConsInfoMap = foldMap getConsInfo . M.elems

    getConsInfo :: UserType -> Map ConsName ConsInfo
    getConsInfo ut@UserType{..} = M.fromList $ fmap (toInfo ut) $ zip [0..] $ M.toList userType'cases

    toInfo :: UserType -> (Int, (ConsName, ConsDef)) -> (ConsName, ConsInfo)
    toInfo userT (tagId, (name, def)) = (name, info)
      where
        ty = userType'name userT
        tyArgs = userType'args userT

        info = ConsInfo
                { consInfo'tagId = tagId
                , consInfo'arity = arity
                , consInfo'type  = consTy
                , consInfo'def   = userT
                }

        arity = V.length consArgs

        consTy = getConsType (toResultType ty tyArgs) consArgs

        consArgs = getConsTypes def

    getConsType :: Type -> Vector Type -> Type
    getConsType rhs args = V.foldr (\arg r -> H.arrowT noLoc arg r) rhs args

    toResultType :: VarName -> [VarName] -> Type
    toResultType name args = H.conT noLoc (varName'name name) (fmap (H.varT noLoc . varName'name) args)

getConsDefArity :: ConsDef -> Int
getConsDefArity = V.length . getConsTypes

setupUserRecords :: UserTypeCtx -> UserTypeCtx
setupUserRecords = setupRecFields . setupRecConstrs

-- | Fills record field order from user defined types.
setupRecConstrs :: UserTypeCtx -> UserTypeCtx
setupRecConstrs ctx = ctx { userTypeCtx'recConstrs = recConstrs }
  where
    recConstrs = M.fromList $ mapMaybe getConstr . M.toList . userType'cases =<< types

    getConstr (cons, def) = case def of
      ConsDef _ -> Nothing
      RecordCons fields -> Just $ (cons, RecordFieldOrder $
          fmap (varName'name . recordField'name) $ V.toList fields)

    types = M.elems $ userTypeCtx'types ctx

-- | Fills record fields to complete set of fields map for all record fields.
setupRecFields :: UserTypeCtx -> UserTypeCtx
setupRecFields ctx = ctx { userTypeCtx'recFields = recFields }
  where
    getFieldMap cons fieldOrder@(RecordFieldOrder fields) = fmap (\field -> (field, (cons, fieldOrder))) fields

    recFields = M.fromList $ uncurry getFieldMap =<< (M.toList $ userTypeCtx'recConstrs ctx)


-- | User-defined type
data UserType = UserType
  { userType'name       :: !VarName                -- ^ Type name
  , userType'args       :: ![VarName]              -- ^ type arguments
  , userType'cases      :: !(Map ConsName ConsDef) -- ^ List of constructors
  } deriving (Show, Eq)

getConsTypes :: ConsDef -> Vector Type
getConsTypes = \case
  ConsDef ts        -> ts
  RecordCons fields -> fmap recordField'type fields

-- | Constructor definition.
data ConsDef
  = ConsDef (Vector Type)            -- ^ Simple constructor with collection of type-arguments
  | RecordCons (Vector RecordField)  -- ^ Record-constructor with named fields
  deriving (Show, Eq)

-- | Record named field.
data RecordField = RecordField
  { recordField'name :: VarName   -- ^ Name of the field
  , recordField'type :: Type      -- ^ Type of the field
  } deriving (Show, Eq)

-- | Data for low-level rendering of type constructor
-- We need to know it's type, arity and integer tag that is unique within
-- its group of constructor for a given type (global uniqueness is not needed)
data ConsInfo = ConsInfo
  { consInfo'type  :: !Type      -- ^ type of the constructor as a function
  , consInfo'tagId :: !Int       -- ^ unique integer identifier (within the type scope)
  , consInfo'arity :: !Int       -- ^ arity of constructor
  , consInfo'def   :: !UserType  -- ^ definition where constructor is defined
  } deriving (Show, Eq)

-- | Order of names in the record constructor.
-- For constructor
--
-- > User { name :: Text, age :: Int }
--
-- It's going to be
--
-- ["name", "age"]
newtype RecordFieldOrder = RecordFieldOrder
  { unRecordFieldOrder :: [Text]
  } deriving (Show, Eq)

-- | Type synonym for money values
type Money = Int64

-- | Type for expression of our language that has type.
--
-- This is phantom type for covenience of type-checker.
newtype Expr a = Expr Lang
  deriving (Show, Eq)

-- | Name of the variable.
data VarName = VarName
  { varName'loc   :: !Loc   -- ^ source code location
  , varName'name  :: !Text  -- ^ variable name
  } deriving (Show)

instance IsString VarName where
  fromString = VarName noLoc . fromString

{-
instance H.IsVar Text where
  intToVar n = mappend "$$" (showt n)
  prettyLetters = fmap fromString $ [1..] >>= flip replicateM ['a'..'z']

instance H.HasPrefix Text where
  getFixity = const Nothing
-}

-- | Name of the constructor
data ConsName = ConsName
  { consName'loc  :: !Loc   -- ^ source code location
  , consName'name :: !Text  -- ^ constructor name
  } deriving (Show)

instance IsString ConsName where
  fromString = ConsName noLoc . fromString

-- | Convert constructor name to variable name
consToVarName :: ConsName -> VarName
consToVarName (ConsName loc name) = VarName loc name

-- | Convert variable name to constructor name
varToConsName :: VarName -> ConsName
varToConsName VarName{..} = ConsName varName'loc varName'name

-- | Argument for script in the transaction
--
-- It's Key-Value map from argument-names to primitive constant values.
data Args = Args
  { args'ints  :: Vector Int64
  , args'bools :: Vector Bool
  , args'texts :: Vector Text
  } deriving (Show, Eq, Ord, Generic, NFData, Serialise)

-- | Identifier of the box. Box holds value protected by the script.
newtype BoxId = BoxId { unBoxId :: Text }
  deriving newtype  (Show, Eq, Ord, NFData, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving stock    (Generic)
  deriving anyclass (Serialise)

-- | Type for script that goes over the wire.
newtype Script = Script { unScript :: Text }
  deriving newtype  (Show, Eq, Ord, NFData, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving stock    (Generic)
  deriving anyclass (Serialise)

-- | Box holds the value protected by the script.
-- We use boxes as inputs for transaction and create new output boxes
-- when script is correct.
data Box = Box
  { box'id     :: !BoxId    -- ^ box identifier
  , box'value  :: !Money    -- ^ Value of the box
  , box'script :: !Script   -- ^ Protecting script
  , box'args   :: !Args     -- ^ arguments for the script
  }
  deriving (Show, Eq, Ord, Generic, Serialise, NFData)

-- | Pattern matching elements (in the arguments or in cases)
data Pat
  = PVar Loc VarName          -- ^ simple variable (anything matches)
  | PPrim Loc Prim            -- ^ constant value (match if equals)
  | PCons Loc ConsName [Pat]  -- ^ concrete constructor with argument patterns
  | PTuple Loc [Pat]          -- ^ tuple with list of arguments
  | PWildCard Loc             -- ^ wildcard (anything matches and value is discarded after match)
  deriving (Show, Eq, Ord)

instance IsString Pat where
  fromString = PVar noLoc . fromString

-- | The type represents modules.
data Module = Module
  { module'loc       :: !Loc              -- ^ source code location
  , module'userTypes :: !UserTypeCtx      -- ^ user-defined types
  , module'binds     :: !(BindGroup Lang) -- ^ values (functions)
  } deriving (Show)

-- | Type context for inference algorithm
type TypeContext = H.Context Loc Text

-- | Context for execution (reduction) of expressions of the language
newtype ExecCtx = ExecCtx
  { execCtx'vars  :: Map VarName Lang  -- ^ bindings for free variables, outer scope of the execution
  } deriving newtype (Show, Eq)

instance Semigroup ExecCtx where
  ExecCtx a1 <> ExecCtx a2 = ExecCtx (a1 <> a2)

instance Monoid ExecCtx where
  mempty = ExecCtx mempty

-- | Type-inference context.
data InferCtx = InferCtx
  { inferCtx'binds :: TypeContext  -- ^ Already derived signatures for
                                   -- all free variables in the expression
  , inferCtx'types :: UserTypeCtx  -- ^ User-defined types
  } deriving (Show, Eq)

instance Semigroup InferCtx where
  a <> b = InferCtx
      { inferCtx'binds = inferCtx'binds a <> inferCtx'binds b
      , inferCtx'types = inferCtx'types a <> inferCtx'types b
      }

instance Monoid InferCtx where
  mempty = InferCtx mempty mempty

-- | Evaluated module
data ModuleCtx = ModuleCtx
  { moduleCtx'types  :: !InferCtx
  , moduleCtx'exprs  :: !ExecCtx
  } deriving (Show, Eq)

getModuleCtxNames :: ModuleCtx -> [Text]
getModuleCtxNames = M.keys . H.unContext . inferCtx'binds . moduleCtx'types

instance Semigroup ModuleCtx where
  (<>) a b = ModuleCtx
    { moduleCtx'types = moduleCtx'types a <> moduleCtx'types b
    , moduleCtx'exprs = moduleCtx'exprs a <> moduleCtx'exprs b
    }

instance Monoid ModuleCtx where
  mempty = ModuleCtx
    { moduleCtx'types = mempty
    , moduleCtx'exprs = mempty
    }

-- | Alternatives for declarations right-hand-sides.
-- Because of pattern matching we can have several alternatives
-- for a single declaration.
data Alt a = Alt
  { alt'pats  :: [Pat]      -- ^ arguments of the function
  , alt'expr  :: Rhs a      -- ^ right-hand side of the declaration
  , alt'where :: Maybe (BindGroup a)  -- ^ 'where'-declarations (definitions local to the function)
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

-- | Right-hand side of the function definition.
data Rhs a
  = UnguardedRhs a         -- ^ No-guards
  | GuardedRhs [Guard a]   -- ^ with guards
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

-- | Guard for right hand-side. RHs is executed if guard's predicate evaluates to True.
data Guard a = Guard
  { guard'predicate :: a  -- ^ guard predicate expression
  , guard'rhs       :: a  -- ^ right-hand side expression
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

-- | List of binds or value definitions.
type BindGroup a = [Bind a]

-- | Value definition
data Bind a = Bind
  { bind'name  :: VarName          -- ^ name of the value
  , bind'type  :: Maybe Signature  -- ^ user provided type signature
  , bind'alts  :: [Alt a]          -- ^ definitions of the value
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

-- | Main tpye for expressions
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
  | Let Loc (BindGroup a) a
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
  | Pk Loc a
  -- ^ private key ownership (@pk publicKey@)
  -- tuples
  | Tuple Loc (Vector a)
  -- ^ Tuple constructor with list of arguments (@(a, b, c)@)
  -- operations
  | UnOpE Loc UnOp a
  -- ^ Application of built-in unary operator to arguments
  | BinOpE Loc BinOp a a
  -- ^ Application of built-in binary operator to arguments
  -- environment
  | GetEnv Loc (EnvId a)
  -- ^ query some item by id in blockchain environment (@getEnvField@)
  -- vectors
  | VecE Loc (VecExpr a)
  -- ^ Vector expression
  -- text
  | TextE Loc (TextExpr a)
  -- ^ Text expression
  -- boxes
  | BoxE Loc (BoxExpr a)
  -- ^ Box-expression
  -- debug
  | Trace Loc a a
  -- ^ Trace print for debug of execution (@trace printMessage value@)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Built-in unary operators
data UnOp
  = Not   -- ^ logical not
  | Neg   -- ^ numeric negation
  | TupleAt Int Int  -- ^ tuple field accessor. Arguments: @TupleAt tupleSize, field number@.
  deriving (Show, Eq)

-- | Built-in binary operators
data BinOp
  = And                  -- ^ boolean AND
  | Or                   -- ^ boolean OR
  | Plus                 -- ^ numeric addition
  | Minus                -- ^ numeric substraction
  | Times                -- ^ numeric multiplication
  | Div                  -- ^ numeric integer division
  | Equals               -- ^ equality test
  | NotEquals            -- ^ non-equality test
  | LessThan             -- ^ @<@
  | GreaterThan          -- ^ @>@
  | LessThanEquals       -- ^ @<=@
  | GreaterThanEquals    -- ^ @>=@
  deriving (Show, Eq)

-- | Case-alternative expression
data CaseExpr a
  = CaseExpr
      { caseExpr'lhs :: Pat  -- ^ pattern to check
      , caseExpr'rhs :: a    -- ^ right-hand side expression
      }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Expressions that operate on boxes.
data BoxExpr a
  = PrimBox Loc Box          -- ^ Primitive constant box
  | BoxAt Loc a (BoxField a) -- ^ Box field getter
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | It defines which values we can get from the box
data BoxField a
  = BoxFieldId
  -- ^ Get box identifier
  | BoxFieldValue
  -- ^ Get box value (or money)
  | BoxFieldScript
  -- ^ Get box script
  | BoxFieldArgList ArgType
  -- ^ Get box argument. It should be primitive value stored in the vector.
  -- We get the vector of primitive values stored by primitive-value tag.
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Types that we can store as arguments in transactions.
-- We store lists of them.
data ArgType = IntArg | TextArg | BoolArg
  deriving (Show, Eq)

argTypes :: [ArgType]
argTypes = [IntArg, TextArg, BoolArg]

argTagToType :: ArgType -> Type
argTagToType = \case
  IntArg  -> intT
  TextArg -> textT
  BoolArg -> boolT

getBoxArgVar :: ArgType -> Text
getBoxArgVar ty = mconcat ["getBox", showt ty, "s"]

-- | Hack to define special names (like record fields or modifiers, or constants for type-inference)
secretVar :: Text -> Text
secretVar = flip mappend "___"

-- | Expressions that operate on vectors
data VecExpr a
  = NewVec Loc (Vector a)
  -- ^ Vector conxtructor from the list of values (@[a, b, c]@)
  | VecAppend Loc a a
  -- ^ Append two vectors (@as ++ bs@)
  | VecAt Loc a a
  -- ^ Get value from the vector by index (@as !! n@)
  | VecLength Loc
  -- ^ Get length of the vector (@length as@)
  | VecMap Loc
  -- ^ map vector with the function (@map f as@)
  | VecFold Loc
  -- ^ Left-fold vector with function and accumulator (@foldl f z as@)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Tag for values to convert to to text
data TextTypeTag
  = IntToText
  -- ^ convert int to text
  | BoolToText
  -- ^ convert boolean to text
  | ScriptToText
  -- ^ convert script to text
  deriving (Eq, Show)

-- | Expressions that operate on texts.
data TextExpr a
  = TextAppend Loc a a
  -- ^ Append text values (@a <> b@)
  | ConvertToText Loc TextTypeTag
  -- ^ Convert some value to text (@showType a@)
  | TextLength Loc
  -- ^ Get textlength (@lengthText a@)
  | TextHash Loc HashAlgo
  -- ^ Get hash-value of the given text (sevral algorithms are supported)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Hashing algorithm tag
data HashAlgo
  = Sha256
  | Blake2b256
  deriving (Eq, Show)

-- | Primitive values of the language (constants).
data Prim
  = PrimInt     Int64
  -- ^ Numeric values
  | PrimString  Text
  -- ^ Text values
  | PrimBool    Bool
  -- ^ Booleans
  | PrimSigma   (Sigma PublicKey)
  -- ^ Sigma-expressions
  deriving (Show, Eq, Ord, Generic, Serialise, NFData)

-- | Environment fields. Info that we can query from blockchain state
data EnvId a
  = Height Loc
  -- ^ Get blockchain height
  | Input Loc  a
  -- ^ Get input box of the script by index
  | Output Loc a
  -- ^ Get output box of the script by index
  -- (those boxes that are created if transaction is comitted with success)
  | Self Loc
  -- ^ Get box of the current script
  | Inputs Loc
  -- ^ Get list of all input boxes
  | Outputs Loc
  -- ^ Get list of all output boxes
  | GetVar Loc ArgType
  -- ^ Get argument of the transaction by name
  deriving (Show, Eq, Functor, Foldable, Traversable)

getEnvVarName :: ArgType -> Text
getEnvVarName ty = Const.getArgs $ argTypeName ty

argTypeName :: ArgType -> Text
argTypeName = \case
  IntArg  -> "Int"
  TextArg -> "Text"
  BoolArg -> "Bool"

instance ToJSON Prim where
  toJSON x = object $ pure $ case x of
    PrimInt n      -> "int"    .= n
    PrimString txt -> "text"   .= txt
    PrimBool b     -> "bool"   .= b
    PrimSigma s    -> "sigma"  .= toJSON s

-- todo: rewrite this instance
-- to distinguish between numeric types of int, double and money
instance FromJSON Prim where
  parseJSON = withObject "prim" $ \v ->
        fmap PrimInt    (v .: "int")
    <|> fmap PrimString (v .: "text")
    <|> fmap PrimBool   (v .: "bool")
    <|> (fmap PrimSigma . parseJSON =<< (v .: "sigma"))

---------------------------------
-- type constants

intT, boolT, boxT, scriptT, textT :: Type

intT = intT' noLoc
boolT = boolT' noLoc
boxT  = boxT' noLoc
scriptT = scriptT' noLoc
textT = textT' noLoc

constType :: Text -> Loc -> Type
constType name loc = H.conT loc name []

boxT' :: Loc -> Type
boxT' = constType "Box"

textT' :: Loc -> Type
textT' = constType "Text"

intT' :: Loc -> Type
intT' = constType "Int"

boolT' :: Loc -> Type
boolT' = constType "Bool"

scriptT' :: Loc -> Type
scriptT' = constType "Script"

vectorT :: Type -> Type
vectorT = vectorT' noLoc

vectorT' :: Loc -> Type -> Type
vectorT' loc a = H.listT loc a


tupleT :: [Type] -> Type
tupleT = tupleT' noLoc

tupleT' ::  Loc -> [Type] -> Type
tupleT' loc ts = H.tupleT loc ts

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
    Pk loc _ -> loc
    -- tuples
    Tuple loc _ -> loc
    -- operations
    UnOpE loc _ _ -> loc
    BinOpE loc _ _ _ -> loc
    -- environment
    GetEnv loc _ -> loc
    -- vectors
    VecE loc _ -> loc
    -- text
    TextE loc _ -> loc
    -- boxes
    BoxE loc _ -> loc
    -- debug
    Trace loc _ _ -> loc
    AltE loc _ _ -> loc
    FailCase loc -> loc

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

instance H.HasLoc (Bind a) where
  type Loc (Bind a) = Loc
  getLoc = H.getLoc . bind'name

instance H.HasLoc (BindGroup Lang) where
  type Loc (BindGroup Lang) = Loc
  getLoc = \case
    []  -> noLoc
    a:_ -> H.getLoc a

-------------------------------------------------------------------
-- unique instances for Eq and Ord (ingnores source location)
--

instance Eq VarName where
  (==) = (==) `on` varName'name

instance Ord VarName where
  compare = compare `on` varName'name

instance Eq ConsName where
  (==) = (==) `on` consName'name

instance Ord ConsName where
  compare = compare `on` consName'name

-------------------------------------------------------------------

-- | Get free0variables for expression
freeVars :: Lang -> Set VarName
freeVars = cata $ \case
  Var _ v         -> Set.singleton v
  InfixApply _ a _ b -> a <> b
  Apply _ a b      -> a <> b
  Lam _ v a        -> a `Set.difference`  freeVarsPat v
  LamList _ vs a   -> a `Set.difference` (foldMap freeVarsPat vs)
  Let _ bg a       -> (a `Set.difference` getBgNames bg) <> freeVarsBg bg
  PrimLet _ bg a   -> (a `Set.difference` getPrimBgNames bg) <> freeVarsPrimBg bg
  Ascr _ a _       -> a
  Cons _ _ vs      -> mconcat $ V.toList vs
  CaseOf _ a alts  -> mappend a (foldMap freeCaseExpr alts)
  RecConstr _ _ ts -> mconcat $ fmap snd ts
  RecUpdate _ a ts -> mconcat $ a : fmap snd ts
  PrimE _ _        -> Set.empty
  If _ a b c       -> mconcat [a, b, c]
  Pk _ a           -> a
  Tuple _ vs       -> fold $ V.toList vs
  UnOpE _ _ a      -> a
  BinOpE _ _ a b   -> mconcat [a, b]
  GetEnv _ env     -> fold env
  VecE _ vec       -> fold vec
  TextE _ txt      -> fold txt
  BoxE _ box       -> fold box
  Trace _ a b      -> mconcat [a, b]
  AltE _ a b       -> mappend a b
  FailCase _       -> Set.empty
  where
    getBgNames :: BindGroup a -> Set VarName
    getBgNames bs = Set.fromList $ fmap bind'name bs

    getPrimBgNames :: [(VarName, a)] -> Set VarName
    getPrimBgNames bs = Set.fromList $ fmap fst bs

    freeVarsBg = foldMap (foldMap localFreeVarsAlt . bind'alts)

    localFreeVarsAlt Alt{..} =
      (freeVarsRhs alt'expr <> foldMap getBgNames alt'where)
      `Set.difference` (foldMap freeVarsPat alt'pats)

    freeVarsPrimBg = foldMap snd

    freeCaseExpr CaseExpr{..} = caseExpr'rhs `Set.difference` (freeVarsPat caseExpr'lhs)

freeVarsRhs :: Rhs (Set VarName) -> Set VarName
freeVarsRhs = \case
  UnguardedRhs a -> a
  GuardedRhs as -> foldMap freeVarsGuard as
  where
    freeVarsGuard Guard{..} = guard'predicate <> guard'rhs


freeVarsPat :: Pat -> Set VarName
freeVarsPat = \case
  PVar _ name -> Set.singleton name
  PPrim _ _ -> Set.empty
  PCons _ _ vs -> foldMap freeVarsPat vs
  PTuple _ vs -> foldMap freeVarsPat vs
  PWildCard _ -> Set.empty

freeVarsAlt :: Alt Lang -> Set VarName
freeVarsAlt Alt{..} =
  freeVarsRhs (fmap freeVars alt'expr) `Set.difference` foldMap freeVarsPat alt'pats

-------------------------------------------------------------------

-- | Reorders binds by dependencies. First go binds with no deps then those
-- that are dependent on them and so forth.
sortBindGroups :: BindGroup Lang -> BindGroup Lang
sortBindGroups = (flattenSCC =<<) . stronglyConnComp . fmap toNode
   where
     toNode s = (s, bind'name s, Set.toList $ foldMap freeVarsAlt $ bind'alts s)

-------------------------------------------------------------------

$(deriveShow1 ''Alt)
$(deriveShow1 ''Rhs)
$(deriveShow1 ''Guard)
$(deriveShow1 ''Bind)
$(deriveShow1 ''E)
$(deriveShow1 ''EnvId)
$(deriveShow1 ''CaseExpr)
$(deriveShow1 ''BoxField)
$(deriveShow1 ''TextExpr)
$(deriveShow1 ''VecExpr)
$(deriveShow1 ''BoxExpr)

instance (forall k. CryptoHashable k => CryptoHashable (f k)) => CryptoHashable (Fix f) where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Prim where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Script where
  hashStep = genericHashStep hashDomain

instance CryptoHashable BoxId where
  hashStep = genericHashStep hashDomain

instance CryptoHashable Box where
  hashStep = genericHashStep hashDomain
