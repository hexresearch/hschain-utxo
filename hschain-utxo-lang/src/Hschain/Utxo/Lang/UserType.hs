-- | User type definitions
module Hschain.Utxo.Lang.UserType(
    UserType(..)
  , UserTypeCtx(..)
  , UserCoreTypeCtx(..)
  , ConsDef(..)
  , ConsInfo(..)
  , CoreConsDef(..)
  , getConsTypes
  , getConsDefArity
  , RecordFieldOrder(..)
  , RecordField(..)
  , setupUserTypeInfo
  , selectorNameVar
  , recordUpdateVar
) where

import Hex.Common.Text

import Control.Applicative
import Control.Monad.State.Strict

import Data.Data
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))

import GHC.Generics

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Core.Types         (Name)

import qualified Type.Check.HM as H
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hschain.Utxo.Lang.Const as Const


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
        []         -> pure Const.unitT
        [(_, def)] -> defToTuple def
        _          -> fmap (H.conT () ("Sum" <> showt (length constrs))) $ mapM (defToTuple . snd) constrs
      where
        toSig t = foldr (\v ty -> H.forAllT () (varName'name v) ty) (H.monoT t) userType'args

        constrs = M.toList userType'cases
        constrOrderMap = M.fromList $ zipWith (\n (name, _) -> (name, n)) [0..] constrs

        getOrder n = M.lookup n constrOrderMap

        defToTuple x = case V.toList $ getConsTypes x of
          []  -> pure Const.unitT
          [t] -> convertType $ eraseLoc t
          tys -> fmap Const.tupleT $ mapM (convertType . eraseLoc) tys

        getSumTs
          | length constrs < 2 = pure Nothing
          | otherwise          = fmap (Just . V.fromList) $ mapM (toSumArg . snd) constrs

        toSumArg :: ConsDef -> State UserCoreTypeCtx (H.Type () Name)
        toSumArg def
          | arity == 0 = pure Const.unitT
          | arity == 1 = convertType $ eraseLoc $ V.head tys
          | otherwise  = fmap (Const.tupleT . V.toList) $ mapM (convertType . eraseLoc) tys
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

--------------------------------------------------------------------------------
-- names for type-checker to convert cases to simple lambda-calculus

selectorNameVar :: ConsName -> Int -> T.Text
selectorNameVar cons n = secretVar $ mconcat ["sel_", consName'name cons, "_", showt n]

recordUpdateVar :: VarName -> Text
recordUpdateVar field = secretVar $ mconcat ["update_", varName'name field]


-----------------------------------------------------------
-- instances

deriving stock   instance Show UserTypeCtx
deriving stock   instance Eq   UserTypeCtx

