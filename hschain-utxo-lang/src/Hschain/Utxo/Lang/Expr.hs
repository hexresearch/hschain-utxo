{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module defines AST for the language
module Hschain.Utxo.Lang.Expr where

import Hex.Common.Aeson
import Hex.Common.Text

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad.State.Strict

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
import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))
import GHC.Generics

import Text.Show.Deriving

import HSChain.Crypto.Classes (ByteRepr(..), ViaBase58(..))
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Core.Types         (TypeCore(..), argsTuple, Name)
import Hschain.Utxo.Lang.Types              (Script(..))
import Hschain.Utxo.Lang.Core.Compile.Expr  (PrimOp(..))
import qualified Type.Check.HM as H
import qualified Language.Haskell.Exts.SrcLoc as Hask

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Vector as V

import qualified Hschain.Utxo.Lang.Const as Const

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

-- | Context of user-defined types
data UserTypeCtx = UserTypeCtx
  { userTypeCtx'types      :: Map VarName  UserType          -- ^ User-defined types
  , userTypeCtx'constrs    :: Map ConsName ConsInfo          -- ^ Map from constructor names to it's low-level data, for further compilation
  , userTypeCtx'recConstrs :: Map ConsName RecordFieldOrder  -- ^ Order of fields for records
  , userTypeCtx'recFields  :: Map Text     (ConsName, RecordFieldOrder)  -- ^ Maps record single field to the full lists of fields
  , userTypeCtx'core       :: UserCoreTypeCtx
  }
  deriving (Generic, Data, Typeable)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid UserTypeCtx

data UserCoreTypeCtx = UserCoreTypeCtx
  { userCoreTypeCtx'types   :: Map Name (H.Signature () Name)
  , userCoreTypeCtx'constrs :: Map Name CoreConsDef
  }
  deriving (Show, Eq, Generic, Data, Typeable)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid UserCoreTypeCtx

userCoreTypeMap :: Map Name UserType -> UserCoreTypeCtx
userCoreTypeMap ts = execState (mapM_ go $ M.toList ts) (UserCoreTypeCtx mempty mempty)
  where
    go :: (Name, UserType) -> State UserCoreTypeCtx ()
    go (name, def)
      | isBaseType name = return ()
      | otherwise       = do
          st <- get
          case M.lookup name $ userCoreTypeCtx'types st of
            Just _   -> return ()
            Nothing  -> do
              t <- convertUserType def
              modify' $ \env -> env { userCoreTypeCtx'types = M.insert name t $ userCoreTypeCtx'types env }

    isBaseType x = Set.member x baseTypes
    baseTypes = Set.fromList ["Maybe"]

    convertUserType UserType{..} = fmap toSig $ do
      mapM_ addCons constrs
      case constrs of
        []         -> pure unitT
        [(_, def)] -> defToTuple def
        _          -> fmap (H.conT () ("Sum" <> showt (length constrs))) $ mapM (defToTuple . snd) constrs
      where
        toSig t = foldr (\v ty -> H.forAllT () (varName'name v) ty) (H.monoT t) userType'args

        constrs = M.toList userType'cases
        constrOrderMap = M.fromList $ zipWith (\n (name, _) -> (name, n)) [0..] constrs

        getOrder n = M.lookup n constrOrderMap

        defToTuple x = case V.toList $ getConsTypes x of
          []  -> pure unitT
          [t] -> convertType $ eraseLoc t
          tys -> fmap tupleT $ mapM (convertType . eraseLoc) tys

        getSumTs
          | length constrs < 2 = pure Nothing
          | otherwise          = fmap (Just . V.fromList) $ mapM (toSumArg . snd) constrs

        toSumArg :: ConsDef -> State UserCoreTypeCtx (H.Type () Name)
        toSumArg def
          | arity == 0 = pure unitT
          | arity == 1 = convertType $ eraseLoc $ V.head tys
          | otherwise  = fmap (tupleT . V.toList) $ mapM (convertType . eraseLoc) tys
          where
            arity = V.length tys
            tys = getConsTypes def

        toTup = mapM (convertType . eraseLoc) . getConsTypes

        addCons (name, def) = do
          sumTs <- getSumTs
          tupleTs <- toTup def
          let coreDef = CoreConsDef
                { coreConsDef'sum   = liftA2 (,) (getOrder name) sumTs
                , coreConsDef'tuple = tupleTs
                }
          modify' $ \st -> st { userCoreTypeCtx'constrs = M.insert (consName'name name) coreDef $ userCoreTypeCtx'constrs st }

    convertType :: H.Type () Name -> State UserCoreTypeCtx (H.Type () Name)
    convertType (H.Type x) = fmap H.Type $ foldFixM tfm x
      where
        tfm = \case
          H.ConT loc con args | isUserType con -> do
            st <- get
            case M.lookup con $ userCoreTypeCtx'types st of
              Just t  -> pure $ H.unType $ H.closeSignature (fmap H.Type args) t
              Nothing -> case M.lookup con ts of
                           Just defn -> do
                             t <- convertUserType defn
                             modify' $ \env -> env { userCoreTypeCtx'types = M.insert con t $ userCoreTypeCtx'types env }
                             return (H.unType $ H.closeSignature (fmap H.Type args) t)
                           Nothing -> pure $ Fix $ H.ConT loc con args
          other               -> pure $ Fix $ other

    isUserType x = Set.member x allTypeNames
    allTypeNames = Set.fromList $ M.keys ts

    eraseLoc = H.setLoc ()

recFieldExecCtx :: UserTypeCtx -> ExecCtx
recFieldExecCtx UserTypeCtx{..} =
  ExecCtx $ M.mapKeys (VarName noLoc) $ M.mapWithKey recSelectorExpr userTypeCtx'recFields

recSelectorExpr :: Text -> (ConsName, RecordFieldOrder) -> Lang
recSelectorExpr field (cons, RecordFieldOrder fields) =
  Fix $ Lam noLoc (PCons noLoc cons args) $ Fix $ Var noLoc v
  where
    v = VarName noLoc "$v"
    args = fmap (\x -> if (field == x) then (PVar noLoc v) else PWildCard noLoc) fields


setupUserTypeInfo :: UserTypeCtx -> UserTypeCtx
setupUserTypeInfo = setupCoreTypeInfo . setupConsInfo . setupUserRecords

setupCoreTypeInfo :: UserTypeCtx -> UserTypeCtx
setupCoreTypeInfo t = t { userTypeCtx'core = userCoreTypeMap $ M.mapKeys varName'name $ userTypeCtx'types t }

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
                { consInfo'tagId   = tagId
                , consInfo'arity   = arity
                , consInfo'type    = consTy
                , consInfo'def     = userT
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

-- | Core representation of user data-types with constructors.
newtype CoreUserType = CoreUserType
  { coreUserType'cases  :: Map ConsName CoreConsDef
  } deriving (Show, Eq, Data)

-- | Every user constructor is transformed
-- to combination of tuple and sum type constructors
data CoreConsDef = CoreConsDef
  { coreConsDef'sum   :: Maybe (Int, Vector (H.Type () Name)) -- ^ sum type constructor spec
                                                              --  sometimes we do not need sum nad tuple is enough
                                                              --  then it's omited (value Nothing)
  , coreConsDef'tuple :: Vector (H.Type () Name)              -- ^ Multiple arguments are enocded as tuples
  } deriving (Show, Eq, Data)


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
  { consInfo'type    :: !Type        -- ^ type of the constructor as a function
  , consInfo'tagId   :: !Int         -- ^ unique integer identifier (within the type scope)
  , consInfo'arity   :: !Int         -- ^ arity of constructor
  , consInfo'def     :: !UserType    -- ^ definition where constructor is defined
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

-- | The type represents modules.
data Module = Module
  { module'loc       :: !Loc          -- ^ source code location
  , module'userTypes :: !UserTypeCtx  -- ^ user-defined types
  , module'binds     :: !(Binds Lang) -- ^ values (functions)
  } deriving (Data, Typeable)

-- | Type context for inference algorithm
type TypeContext = H.Context Loc Text

-- | Context for execution (reduction) of expressions of the language
newtype ExecCtx = ExecCtx
  { execCtx'vars  :: Map VarName Lang  -- ^ bindings for free variables, outer scope of the execution
  } deriving newtype (Semigroup, Monoid)

-- | Type-inference context.
data InferCtx = InferCtx
  { inferCtx'binds :: TypeContext  -- ^ Already derived signatures for
                                   -- all free variables in the expression
  , inferCtx'types :: UserTypeCtx  -- ^ User-defined types
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid InferCtx

-- | Evaluated module
data ModuleCtx = ModuleCtx
  { moduleCtx'types  :: !InferCtx
  , moduleCtx'exprs  :: !ExecCtx
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid ModuleCtx

getModuleCtxNames :: ModuleCtx -> [Text]
getModuleCtxNames = M.keys . H.unContext . inferCtx'binds . moduleCtx'types

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

-- | Type tag for type-safe construction
data SigmaBool

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

intT, boolT, boxT, scriptT, textT, sigmaT, bytesT, unitT :: (IsString v, H.DefLoc loc) => H.Type loc v
intT    = intT'    H.defLoc
boolT   = boolT'   H.defLoc
bytesT  = bytesT'  H.defLoc
boxT    = boxT'    H.defLoc
scriptT = scriptT' H.defLoc
textT   = textT'   H.defLoc
sigmaT  = sigmaT'  H.defLoc
unitT   = unitT'   H.defLoc

tupleT :: H.DefLoc loc => [H.Type loc v] -> H.Type loc v
tupleT = tupleT' H.defLoc

sumT :: IsString v => H.DefLoc loc => [H.Type loc v] -> H.Type loc v
sumT = sumT' H.defLoc

listT :: H.DefLoc loc => H.Type loc v -> H.Type loc v
listT = listT' H.defLoc

maybeT :: IsString v => H.DefLoc loc => H.Type loc v -> H.Type loc v
maybeT = maybeT' H.defLoc

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

intT', boolT', boxT', scriptT', textT', sigmaT', bytesT', unitT' :: (IsString v, H.DefLoc loc) => loc -> H.Type loc v
boxT'    = constType "Box"
textT'   = constType "Text"
bytesT'  = constType "Bytes"
intT'    = constType "Int"
boolT'   = constType "Bool"
sigmaT'  = constType "Sigma"
scriptT' = constType "Script"
unitT'   = constType "()"

listT' :: loc -> H.Type loc v -> H.Type loc v
listT' loc a = H.listT loc a

maybeT' :: IsString v => loc -> H.Type loc v -> H.Type loc v
maybeT' loc a = H.conT loc "Maybe" [a]

tupleT' :: loc -> [H.Type loc v] -> H.Type loc v
tupleT' loc ts = H.tupleT loc ts

sumT' :: IsString v => loc -> [H.Type loc v] -> H.Type loc v
sumT' loc ts = H.conT loc "Sum" ts

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
  Let _ bg a       -> let bgNames = getBindsNames bg
                      in  (a `Set.difference` bgNames) <> freeVarsBinds bg
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
    getPrimBgNames :: [(VarName, a)] -> Set VarName
    getPrimBgNames bs = Set.fromList $ fmap fst bs

    freeCaseExpr CaseExpr{..} = caseExpr'rhs `Set.difference` (freeVarsPat caseExpr'lhs)

    freeVarsPrimBg = foldMap snd

freeVarsBinds :: Binds (Set VarName) -> Set VarName
freeVarsBinds bg = (foldMap (foldMap freeVarsAlt . bindAlts) . binds'decls) bg

getBindsNames :: Binds a -> Set VarName
getBindsNames = foldMap getNames . binds'decls
  where
    getNames = \case
      FunBind{..} -> Set.singleton bind'name
      PatBind{..} -> freeVarsPat bind'pat

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
  UnitT     -> unitT
  MaybeT a  -> maybeT (typeCoreToType a)
  SumT ts   -> sumT (fmap typeCoreToType ts)

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
  OpToBytes   _ -> Nothing
  OpFromBytes _ -> Nothing
  --
  OpArgs _       -> Nothing
  OpGetBoxId     -> Just Const.getBoxId
  OpGetBoxScript -> Just Const.getBoxScript
  OpGetBoxValue  -> Just Const.getBoxValue
  OpGetBoxArgs _ -> Nothing
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
  OpListProduct  -> Just "product"
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
  OpFromBytes _ -> Just Const.deserialiseBytes
  OpToBytes   _ -> Just Const.serialiseBytes
  --
  OpArgs _       -> Just $ Const.getArgs
  OpGetBoxArgs _ -> Just $ Const.getBoxArgs
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
  , OpBoolAnd, OpBoolOr, OpBoolNot
  , OpSigAnd, OpSigOr, OpSigPK, OpSigBool, OpSigListAnd, OpSigListOr
  , OpCheckSig, OpCheckMultiSig
  , OpSHA256, OpTextLength, OpBytesLength, OpTextAppend, OpBytesAppend
  , OpEnvGetHeight, OpEnvGetSelf, OpEnvGetInputs, OpEnvGetOutputs, OpEnvGetDataInputs
  , OpGetBoxId, OpGetBoxScript, OpGetBoxValue, OpGetBoxPostHeight
  , OpListSum
  , OpListProduct
  , OpListAnd
  , OpListOr
  ]

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

instance ToLang Bool where
  toLang loc b = toPrim loc $ PrimBool b

instance ToLang (Sigma PublicKey) where
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

deriving newtype instance Show ExecCtx
deriving newtype instance Eq   ExecCtx
deriving stock   instance Show ModuleCtx
deriving stock   instance Eq   ModuleCtx
deriving stock   instance Show UserTypeCtx
deriving stock   instance Eq   UserTypeCtx
deriving stock   instance Show InferCtx
deriving stock   instance Eq   InferCtx
deriving stock   instance Show Module
deriving stock   instance Show (Expr a)
deriving stock   instance Eq   (Expr a)
