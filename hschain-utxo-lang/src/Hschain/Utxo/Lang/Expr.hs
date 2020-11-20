{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module defines AST for the language
module Hschain.Utxo.Lang.Expr where

import Hex.Common.Aeson
import Hex.Common.Text

import Control.Applicative
import Control.DeepSeq (NFData)

import Codec.Serialise

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Data
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
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))
import GHC.Generics

import Text.Show.Deriving

import HSChain.Crypto.Classes (ByteRepr(..), ViaBase58(..))
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Core.Types         (TypeCore(..), argsTuple, Name)
import Hschain.Utxo.Lang.Types              (Args(..), ArgType(..), argTypes, Script(..))
import Hschain.Utxo.Lang.Core.Compile.Expr  (PrimOp(..))
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

deriving instance Data (H.Type Loc Text)
deriving instance Data (H.TypeF Loc Text (Fix (H.TypeF Loc Text)))
deriving instance Data (H.Signature Loc Text)
deriving instance Data (H.SignatureF Loc Text (Fix (H.SignatureF Loc Text)))

instance H.DefLoc Hask.SrcSpanInfo where
  defLoc = Hask.noSrcSpan

-- | Unknown source code location.
noLoc :: Loc
noLoc = Hask.noSrcSpan

-- | Context of user-defined types
data UserTypeCtx = UserTypeCtx
  { userTypeCtx'types      :: Map VarName  UserType          -- ^ User-defined types
  , userTypeCtx'constrs    :: Map ConsName ConsInfo          -- ^ Map from constructor names to it's low-level data, for further compilation
  , userTypeCtx'recConstrs :: Map ConsName RecordFieldOrder  -- ^ Order of fields for records
  , userTypeCtx'recFields  :: Map Text     (ConsName, RecordFieldOrder)  -- ^ Maps record single field to the full lists of fields
  }
  deriving (Show, Eq, Generic, Data, Typeable)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid UserTypeCtx


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
  } deriving (Show, Eq, Data, Typeable)

getConsTypes :: ConsDef -> Vector Type
getConsTypes = \case
  ConsDef ts        -> ts
  RecordCons fields -> fmap recordField'type fields

-- | Constructor definition.
data ConsDef
  = ConsDef (Vector Type)            -- ^ Simple constructor with collection of type-arguments
  | RecordCons (Vector RecordField)  -- ^ Record-constructor with named fields
  deriving (Show, Eq, Data, Typeable)

-- | Record named field.
data RecordField = RecordField
  { recordField'name :: VarName   -- ^ Name of the field
  , recordField'type :: Type      -- ^ Type of the field
  } deriving (Show, Eq, Data, Typeable)

-- | Data for low-level rendering of type constructor
-- We need to know it's type, arity and integer tag that is unique within
-- its group of constructor for a given type (global uniqueness is not needed)
data ConsInfo = ConsInfo
  { consInfo'type  :: !Type      -- ^ type of the constructor as a function
  , consInfo'tagId :: !Int       -- ^ unique integer identifier (within the type scope)
  , consInfo'arity :: !Int       -- ^ arity of constructor
  , consInfo'def   :: !UserType  -- ^ definition where constructor is defined
  } deriving (Show, Eq, Data, Typeable)

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
  } deriving (Show, Eq, Data, Typeable)

-- | Type for expression of our language that has type.
--
-- This is phantom type for covenience of type-checker.
newtype Expr a = Expr Lang
  deriving (Show, Eq)

-- | Name of the variable.
data VarName = VarName
  { varName'loc   :: !Loc   -- ^ source code location
  , varName'name  :: !Text  -- ^ variable name
  } deriving (Show, Data, Typeable)

instance IsString VarName where
  fromString = VarName noLoc . fromString

-- | Name of the constructor
data ConsName = ConsName
  { consName'loc  :: !Loc   -- ^ source code location
  , consName'name :: !Text  -- ^ constructor name
  } deriving (Show, Data, Typeable)

instance IsString ConsName where
  fromString = ConsName noLoc . fromString

-- | Convert constructor name to variable name
consToVarName :: ConsName -> VarName
consToVarName (ConsName loc name) = VarName loc name

-- | Convert variable name to constructor name
varToConsName :: VarName -> ConsName
varToConsName VarName{..} = ConsName varName'loc varName'name

-- | Construct args that contain only integers
intArgs :: [Int64] -> Args
intArgs xs = Args
  { args'ints  = V.fromList xs
  , args'bools = mempty
  , args'texts = mempty
  , args'bytes = mempty
  }

-- | Construct args that contain only booleans
boolArgs :: [Bool] -> Args
boolArgs xs = Args
  { args'ints  = mempty
  , args'bools = V.fromList xs
  , args'texts = mempty
  , args'bytes = mempty
  }

-- | Construct args that contain only texts
textArgs :: [Text] -> Args
textArgs xs = Args
  { args'ints  = mempty
  , args'bools = mempty
  , args'texts = V.fromList xs
  , args'bytes = mempty
  }

-- | Construct args that contain only bytestrings
byteArgs :: [ByteString] -> Args
byteArgs xs = Args
  { args'ints  = mempty
  , args'bools = mempty
  , args'texts = mempty
  , args'bytes = V.fromList xs
  }

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

-- | The type represents modules.
data Module = Module
  { module'loc       :: !Loc          -- ^ source code location
  , module'userTypes :: !UserTypeCtx  -- ^ user-defined types
  , module'binds     :: ![Bind Lang]  -- ^ values (functions)
  } deriving (Show, Data, Typeable)

-- | Type context for inference algorithm
type TypeContext = H.Context Loc Text

-- | Context for execution (reduction) of expressions of the language
newtype ExecCtx = ExecCtx
  { execCtx'vars  :: Map VarName Lang  -- ^ bindings for free variables, outer scope of the execution
  } deriving newtype (Show, Eq, Semigroup, Monoid)

-- | Type-inference context.
data InferCtx = InferCtx
  { inferCtx'binds :: TypeContext  -- ^ Already derived signatures for
                                   -- all free variables in the expression
  , inferCtx'types :: UserTypeCtx  -- ^ User-defined types
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid InferCtx

-- | Evaluated module
data ModuleCtx = ModuleCtx
  { moduleCtx'types  :: !InferCtx
  , moduleCtx'exprs  :: !ExecCtx
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid ModuleCtx

getModuleCtxNames :: ModuleCtx -> [Text]
getModuleCtxNames = M.keys . H.unContext . inferCtx'binds . moduleCtx'types

-- | Alternatives for declarations right-hand-sides.
-- Because of pattern matching we can have several alternatives
-- for a single declaration.
data Alt a = Alt
  { alt'pats  :: [Pat]      -- ^ arguments of the function
  , alt'expr  :: Rhs a      -- ^ right-hand side of the declaration
  , alt'where :: Maybe [Bind a]  -- ^ 'where'-declarations (definitions local to the function)
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

-- | Value definition
data Bind a = Bind
  { bind'name  :: VarName          -- ^ name of the value
  , bind'type  :: Maybe Signature  -- ^ user provided type signature
  , bind'alts  :: [Alt a]          -- ^ definitions of the value
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Typeable)

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
  | Let Loc [Bind a] a
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
  -- operations
  | UnOpE Loc UnOp a
  -- ^ Application of built-in unary operator to arguments
  | BinOpE Loc BinOp a a
  -- ^ Application of built-in binary operator to arguments
  -- environment
  | GetEnv Loc (EnvId a)
  -- ^ query some item by id in blockchain environment (@getEnvField@)
  -- vectors
  | SigmaE Loc (SigmaExpr a)
  -- ^ Sigma-expressions
  | VecE Loc (VecExpr a)
  -- ^ Vector expression
  -- text
  | TextE Loc (TextExpr a)
  -- ^ Text expression
  -- Bytes
  | BytesE Loc (BytesExpr a)
  -- ^ bytes expression
  -- boxes
  | BoxE Loc (BoxExpr a)
  -- ^ Box-expression
  | CheckSig Loc a a
  -- ^ check signature. Arguments are: public key as byte string and index of boxInput'sigs vector (of signatures)
  | CheckMultiSig Loc a a a
  -- ^ check multi-signature M out of N. Arguments are: number of signatures o be valid, list of public keys as texts, list of indices to boxInput'sigs vector (of signatures)
  -- debug
  | Trace Loc a a
  -- ^ Trace print for debug of execution (@trace printMessage value@)
  | AntiQuote Loc (Maybe ArgType) VarName
  -- ^ reference to external vriables (used in quasi quoting)
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

-- | Built-in unary operators
data UnOp
  = Not   -- ^ logical not
  | Neg   -- ^ numeric negation
  | TupleAt Int Int  -- ^ tuple field accessor. Arguments: @TupleAt tupleSize, field number@.
  deriving (Show, Eq, Data, Typeable)

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
  deriving (Show, Eq, Data, Typeable)

-- | Case-alternative expression
data CaseExpr a
  = CaseExpr
      { caseExpr'lhs :: Pat  -- ^ pattern to check
      , caseExpr'rhs :: a    -- ^ right-hand side expression
      }
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

-- | Expressions that operate on boxes.
data BoxExpr a
  = BoxAt Loc a BoxField -- ^ Box field getter
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

-- | It defines which values we can get from the box
data BoxField
  = BoxFieldId
  -- ^ Get box identifier
  | BoxFieldValue
  -- ^ Get box value (or money)
  | BoxFieldScript
  -- ^ Get box script
  | BoxFieldArgList ArgType
  -- ^ Get box argument. It should be primitive value stored in the vector.
  -- We get the vector of primitive values stored by primitive-value tag.
  | BoxFieldPostHeight
  -- ^ Get time at which box was posted. It's useful to create relative time bounds
  deriving (Show, Eq, Data, Typeable)

argTagToType :: ArgType -> Type
argTagToType = \case
  IntArg   -> intT
  TextArg  -> textT
  BoolArg  -> boolT
  BytesArg -> bytesT

argTagToType' :: Loc -> ArgType -> Type
argTagToType' loc = \case
  IntArg   -> intT' loc
  TextArg  -> textT' loc
  BoolArg  -> boolT' loc
  BytesArg -> bytesT' loc

getBoxArgVar :: ArgType -> Text
getBoxArgVar ty = mconcat ["getBox", showt ty, "s"]

-- | Hack to define special names (like record fields or modifiers, or constants for type-inference)
secretVar :: Text -> Text
secretVar = flip mappend "___"

-- | Type tag for type-safe construction
data SigmaBool

-- | Sigma-expressions
data SigmaExpr a
  = Pk Loc a         -- ^ key ownership
  | SAnd Loc a a     -- ^ sigma and
  | SOr Loc a a      -- ^ sigma or
  | SPrimBool Loc a  -- ^ constant bool
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

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
  | VecAndSigma Loc
  -- ^ and of vector of sigma expressions
  | VecOrSigma Loc
  -- ^ or of vector of sigma expressions
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

-- | Tag for values to convert to to text
data TextTypeTag
  = IntToText
  -- ^ convert int to text
  | BoolToText
  -- ^ convert boolean to text
  | ScriptToText
  -- ^ convert script to text
  deriving (Eq, Show, Data, Typeable)

-- | Expressions that operate on texts.
data TextExpr a
  = TextAppend Loc a a
  -- ^ Append text values (@a <> b@)
  | ConvertToText Loc TextTypeTag
  -- ^ Convert some value to text (@showType a@)
  | TextLength Loc
  -- ^ Get textlength (@lengthText a@)
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

data BytesExpr a
  = BytesAppend Loc a a
  -- ^ append bytes
  | BytesLength Loc a
  -- ^ size of byteString
  | SerialiseToBytes Loc ArgType a
  -- ^ serialise primitive types to bytes
  | DeserialiseFromBytes Loc ArgType a
  -- ^ deserialise values from bytes
  | BytesHash Loc HashAlgo a
  -- ^ get hash for the given ByteString.
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

-- | Hashing algorithm tag
data HashAlgo
  = Sha256
  deriving (Eq, Show, Data, Typeable)

-- | Primitive values of the language (constants).
data Prim
  = PrimInt     Int64
  -- ^ Numeric values
  | PrimString  Text
  -- ^ Text values
  | PrimBool    Bool
  -- ^ Booleans
  | PrimSigma   (Sigma ByteString)
  -- ^ Sigma-expressions
  | PrimBytes ByteString
  deriving (Show, Eq, Ord, Generic, Serialise, NFData, Data, Typeable)

-- | Result of the script can be boolean constant or sigma-expression
-- that user have to prove.
data BoolExprResult
  = ConstBool Bool
  | SigmaResult (Sigma PublicKey)
  deriving (Show, Eq)

instance ToJSON BoolExprResult where
  toJSON = \case
    ConstBool b -> object ["bool"  .= b]
    SigmaResult s -> object ["sigma" .= s]

instance FromJSON BoolExprResult where
  parseJSON = withObject "BoolExprResult" $ \obj ->
        (ConstBool <$> obj .: "bool")
    <|> (SigmaResult <$> obj .: "sigma")


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
  | DataInputs Loc
  -- ^ Get list of all data-input boxes
  | GetVar Loc ArgType
  -- ^ Get argument of the transaction by name
  deriving (Show, Eq, Functor, Foldable, Traversable, Data, Typeable)

getEnvVarName :: ArgType -> Text
getEnvVarName ty = Const.getArgs $ argTypeName ty

argTypeName :: ArgType -> Text
argTypeName = \case
  IntArg   -> "Int"
  TextArg  -> "Text"
  BoolArg  -> "Bool"
  BytesArg -> "Bytes"

instance ToJSON Prim where
  toJSON x = object $ pure $ case x of
    PrimInt n      -> "int"    .= n
    PrimString txt -> "text"   .= txt
    PrimBool b     -> "bool"   .= b
    PrimSigma s    -> "sigma"  .= toJSON s
    PrimBytes s    -> "bytes"  .= toJSON (ViaBase58 s)

-- todo: rewrite this instance
-- to distinguish between numeric types of int, double and money
instance FromJSON Prim where
  parseJSON = withObject "prim" $ \v ->
        fmap PrimInt    (v .: "int")
    <|> fmap PrimString (v .: "text")
    <|> fmap (\(ViaBase58 s :: ViaBase58 "Prim" ByteString) -> PrimBytes s) (v .: "bytes")
    <|> fmap PrimBool   (v .: "bool")
    <|> (fmap PrimSigma . parseJSON =<< (v .: "sigma"))

---------------------------------
-- type constants

intT, boolT, boxT, scriptT, textT, sigmaT, bytesT :: (IsString v, H.DefLoc loc) => H.Type loc v
intT    = intT'    H.defLoc
boolT   = boolT'   H.defLoc
bytesT  = bytesT'  H.defLoc
boxT    = boxT'    H.defLoc
scriptT = scriptT' H.defLoc
textT   = textT'   H.defLoc
sigmaT  = sigmaT'  H.defLoc

tupleT :: H.DefLoc loc => [H.Type loc v] -> H.Type loc v
tupleT = tupleT' H.defLoc

listT :: H.DefLoc loc => H.Type loc v -> H.Type loc v
listT = listT' H.defLoc

arrowT :: H.DefLoc loc => H.Type loc v -> H.Type loc v -> H.Type loc v
arrowT = H.arrowT H.defLoc

varT :: H.DefLoc loc => v -> H.Type loc v
varT = H.varT H.defLoc

funT :: H.DefLoc loc => [H.Type loc v] -> H.Type loc v -> H.Type loc v
funT args resT = foldr arrowT resT args

argsT :: (IsString v, H.DefLoc loc) => H.Type loc v
argsT = typeCoreToType argsTuple


constType :: v -> loc -> H.Type loc v
constType name loc = H.conT loc name []

intT', boolT', boxT', scriptT', textT', sigmaT', bytesT' :: (IsString v, H.DefLoc loc) => loc -> H.Type loc v
boxT'    = constType "Box"
textT'   = constType "Text"
bytesT'  = constType "Bytes"
intT'    = constType "Int"
boolT'   = constType "Bool"
sigmaT'  = constType "Sigma"
scriptT' = constType "Script"

listT' :: loc -> H.Type loc v -> H.Type loc v
listT' loc a = H.listT loc a

tupleT' :: loc -> [H.Type loc v] -> H.Type loc v
tupleT' loc ts = H.tupleT loc ts

arrowT' :: loc -> H.Type loc v -> H.Type loc v -> H.Type loc v
arrowT' = H.arrowT

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
    -- operations
    UnOpE loc _ _ -> loc
    BinOpE loc _ _ _ -> loc
    -- environment
    GetEnv loc _ -> loc
    -- sigmas
    SigmaE loc _ -> loc
    -- vectors
    VecE loc _ -> loc
    -- text
    TextE loc _ -> loc
    -- bytes
    BytesE loc _ -> loc
    -- boxes
    BoxE loc _ -> loc
    -- BTC-style signatures
    CheckSig loc _ _ -> loc
    CheckMultiSig loc _ _ _ -> loc
    -- debug
    Trace loc _ _ -> loc
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

instance H.HasLoc (Bind a) where
  type Loc (Bind a) = Loc
  getLoc = H.getLoc . bind'name


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
  InfixApply _ a v b -> Set.singleton v <> a <> b
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
  Tuple _ vs       -> fold $ V.toList vs
  UnOpE _ _ a      -> a
  BinOpE _ _ a b   -> mconcat [a, b]
  GetEnv _ env     -> fold env
  SigmaE _ sigma   -> fold sigma
  VecE _ vec       -> fold vec
  TextE _ txt      -> fold txt
  BytesE _ bs      -> fold bs
  BoxE _ box       -> fold box
  Trace _ a b      -> mconcat [a, b]
  AltE _ a b       -> mappend a b
  CheckSig _ a b   -> a <> b
  CheckMultiSig _ a b c -> a <> b <> c
  FailCase _       -> Set.empty
  AntiQuote _ _ v  -> Set.singleton v
  where
    getBgNames :: [Bind a] -> Set VarName
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
sortBindGroups :: [Bind Lang] -> [Bind Lang]
sortBindGroups = (flattenSCC =<<) . stronglyConnComp . fmap toNode
   where
     toNode s = (s, bind'name s, Set.toList $ foldMap freeVarsAlt $ bind'alts s)

typeCoreToType :: (H.DefLoc loc, IsString v) => TypeCore -> H.Type loc v
typeCoreToType = \case
  IntT      -> intT
  BoolT     -> boolT
  BytesT    -> bytesT
  TextT     -> textT
  SigmaT    -> sigmaT
  BoxT      -> boxT
  a :-> b   -> (arrowT `on` typeCoreToType) a b
  ListT a   -> listT (typeCoreToType a)
  TupleT xs -> tupleT $ typeCoreToType <$> xs

----------------------------------------------------------------
-- Names
----------------------------------------------------------------

-- | Name of monomorphic primop which is used in high level language
monoPrimopName :: PrimOp a -> Maybe Name
monoPrimopName = \case
  OpAdd         -> Just "+"
  OpSub         -> Just "-"
  OpMul         -> Just "*"
  OpDiv         -> Just "/"
  OpNeg         -> Just "negate"
  --
  OpBoolAnd     -> Just "&&"
  OpBoolOr      -> Just "||"
  OpBoolXor     -> Just "^^"
  OpBoolNot     -> Just "not"
  --
  OpSigAnd       -> Just Const.sigmaAnd
  OpSigOr        -> Just Const.sigmaOr
  OpSigPK        -> Just "pk"
  OpSigBool      -> Just "toSigma"
  OpSigListAnd   -> Just "andSigma"
  OpSigListOr    -> Just "orSigma"
  OpSigListAll _ -> Nothing
  OpSigListAny _ -> Nothing
  --
  OpCheckSig      -> Just Const.checkSig
  OpCheckMultiSig -> Just Const.checkMultiSig
  --
  OpSHA256      -> Just Const.sha256
  OpTextLength  -> Just Const.lengthText
  OpBytesLength -> Just Const.lengthBytes
  OpTextAppend  -> Just Const.appendText
  OpBytesAppend -> Just Const.appendBytes
  OpToBytes   t -> Just $ Const.serialiseBytes $ argTypeName t
  OpFromBytes t -> Just $ Const.deserialiseBytes $ argTypeName t
  --
  OpArgs t       -> Just $ "get" <> argTypeName t <> "Args"
  OpGetBoxId     -> Just Const.getBoxId
  OpGetBoxScript -> Just Const.getBoxScript
  OpGetBoxValue  -> Just Const.getBoxValue
  OpGetBoxArgs t -> Just $ Const.getBoxArgs $ argTypeName t
  OpGetBoxPostHeight -> Just $ Const.getBoxPostHeight
  --
  OpEnvGetHeight  -> Just Const.getHeight
  OpEnvGetSelf    -> Just Const.getSelf
  OpEnvGetInputs  -> Just Const.getInputs
  OpEnvGetOutputs -> Just Const.getOutputs
  OpEnvGetDataInputs -> Just Const.getDataInputs
  -- Polymorphic functions
  OpShow _ -> Nothing
  OpEQ _   -> Nothing
  OpNE _   -> Nothing
  OpGT _   -> Nothing
  OpGE _   -> Nothing
  OpLT _   -> Nothing
  OpLE _   -> Nothing
  --
  OpListMap{}    -> Nothing
  OpListAt{}     -> Nothing
  OpListAppend{} -> Nothing
  OpListLength{} -> Nothing
  OpListFoldr{}  -> Nothing
  OpListFoldl{}  -> Nothing
  OpListFilter{} -> Nothing
  OpListSum      -> Just "sum"
  OpListAnd      -> Just "and"
  OpListOr       -> Just "or"
  OpListAll{}    -> Nothing
  OpListAny{}    -> Nothing

polyPrimOpName :: PrimOp a -> Maybe Name
polyPrimOpName = \case
  OpShow _ -> Just "show"
  OpEQ _   -> Just "=="
  OpNE _   -> Just "/="
  OpGT _   -> Just ">"
  OpGE _   -> Just ">="
  OpLT _   -> Just "<"
  OpLE _   -> Just "<="
  --
  OpListMap{}    -> Just "map"
  OpListAt{}     -> Just "listAt"
  OpListAppend{} -> Just "++"
  OpListLength{} -> Just "length"
  OpListFoldr{}  -> Just "foldr"
  OpListFoldl{}  -> Just "foldl"
  OpListFilter{} -> Just "filter"
  OpListAll{}    -> Just "all"
  OpListAny{}    -> Just "any"
  _              -> Nothing


-- | List of all monomorphic primops
monomorphicPrimops :: [PrimOp a]
monomorphicPrimops =
  [ OpAdd, OpSub, OpMul, OpDiv, OpNeg
  , OpBoolAnd, OpBoolOr, OpBoolXor, OpBoolNot
  , OpSigAnd, OpSigOr, OpSigPK, OpSigBool, OpSigListAnd, OpSigListOr
  , OpCheckSig, OpCheckMultiSig
  , OpSHA256, OpTextLength, OpBytesLength, OpTextAppend, OpBytesAppend
  , OpEnvGetHeight, OpEnvGetSelf, OpEnvGetInputs, OpEnvGetOutputs, OpEnvGetDataInputs
  , OpGetBoxId, OpGetBoxScript, OpGetBoxValue, OpGetBoxPostHeight
  , OpListSum
  , OpListAnd
  , OpListOr
  ]
  ++ (OpToBytes <$> argTypes)
  ++ (OpFromBytes <$> argTypes)
  ++ (OpGetBoxArgs <$> argTypes)
  ++ (OpArgs <$> argTypes)

-- | Name map for substitution of monomorphic primops
monoPrimopNameMap :: M.Map Name (PrimOp a)
monoPrimopNameMap = M.fromList
  [ (nm,op) | op      <- monomorphicPrimops
            , Just nm <- [ monoPrimopName op ]
            ]


-------------------------------------------------------------------

class ToLang a where
  toLang :: Loc -> a -> Lang

  toLangExpr :: Loc -> a -> E Lang
  toLangExpr loc a = unFix $ toLang loc a

instance ToLang Text where
  toLang loc txt = toPrim loc $ PrimString txt

instance ToLang ByteString where
  toLang loc bs = toPrim loc $ PrimBytes bs

instance ToLang PublicKey where
  toLang loc key = toPrim loc $ PrimBytes $ encodeToBS key

instance ToLang Script where
  toLang loc (Script bs) = toPrim loc $ PrimBytes bs

instance ToLang Int where
  toLang loc n = toPrim loc $ PrimInt $ fromIntegral n

instance ToLang Int64 where
  toLang loc n = toPrim loc $ PrimInt n

instance ToLang a => ToLang [a] where
  toLang loc vals = Fix $ VecE loc $ NewVec loc $ V.fromList $ fmap (toLang loc) vals

instance (ToLang a, ToLang b) => ToLang (a, b) where
  toLang loc (a, b) = Fix $ Tuple loc $ V.fromList [toLang loc a, toLang loc b]

instance (ToLang a, ToLang b, ToLang c) => ToLang (a, b, c) where
  toLang loc (a, b, c) = Fix $ Tuple loc $ V.fromList [toLang loc a, toLang loc b, toLang loc c]

instance (ToLang a, ToLang b, ToLang c, ToLang d) => ToLang (a, b, c, d) where
  toLang loc (a, b, c, d) = Fix $ Tuple loc $ V.fromList [toLang loc a, toLang loc b, toLang loc c, toLang loc d]

toPrim :: Loc -> Prim -> Lang
toPrim loc p = Fix $ PrimE loc p

-------------------------------------------------------------------

$(deriveShow1 ''Alt)
$(deriveShow1 ''Rhs)
$(deriveShow1 ''Guard)
$(deriveShow1 ''Bind)
$(deriveShow1 ''E)
$(deriveShow1 ''EnvId)
$(deriveShow1 ''CaseExpr)
$(deriveShow1 ''TextExpr)
$(deriveShow1 ''BytesExpr)
$(deriveShow1 ''SigmaExpr)
$(deriveShow1 ''VecExpr)
$(deriveShow1 ''BoxExpr)

instance H.IsVar Text where
  intToVar = H.stringIntToVar
  prettyLetters = H.stringPrettyLetters
