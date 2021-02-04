-- | Module level definitions and evaluation contexts.
module Hschain.Utxo.Lang.Module(
    Module(..)
  , getModuleName
  , ModuleHead(..)
  , ExportList(..)
  , ExportItem(..)
  , Import(..)
  , ImportList(..)
  , ImportItem(..)
  , ModuleCtx(..)
  , ExecCtx(..)
  , InferCtx(..)
  , recFieldExecCtx
  , getModuleCtxNames
  , userTypesToInferCtx
  , mainExprModule
  , simpleModule
) where

import GHC.Generics
import Data.Data
import Data.Fix
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.UserType

import qualified Data.Map.Strict as M
import qualified Type.Check.HM as H

-- | The type represents modules.
data Module = Module
  { module'loc       :: !Loc                 -- ^ source code location
  , module'head      :: !(Maybe ModuleHead)  -- ^ module header
  , module'imports   :: ![Import]            -- ^ list of imports
  , module'userTypes :: !UserTypeCtx         -- ^ user-defined types
  , module'binds     :: !(Binds Lang)        -- ^ values (functions)
  } deriving (Data, Typeable)

-- | Evaluated module
data ModuleCtx = ModuleCtx
  { moduleCtx'types  :: !InferCtx
  , moduleCtx'exprs  :: !ExecCtx
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid ModuleCtx

-- | Header of the module
data ModuleHead = ModuleHead
  { moduleHead'name    :: !VarName             -- ^ module name
  , moduleHead'exports :: !(Maybe ExportList)  -- ^ export list
  }
  deriving stock (Eq, Show, Generic, Data, Typeable)

getModuleName :: Module -> VarName
getModuleName Module{..} = maybe (VarName noLoc "Main") moduleHead'name module'head

-- | Export list
data ExportList = ExportList Loc [ExportItem]
  deriving stock (Eq, Show, Generic, Data, Typeable)

-- | Export list items.
data ExportItem
  = ExportVar VarName                   -- ^ export variable
  | ExportTypeAbs VarName               -- ^ export type or type synonym
  | ExportTypeWith VarName [VarName]    -- ^ export type with some of it constructors
  | ExportModule VarName                -- ^ module re-export
  deriving stock (Eq, Show, Generic, Data, Typeable)

-- | Module import
data Import = Import
  { import'name      :: !VarName               -- ^ module name
  , import'loc       :: !Loc                   -- ^ source code location
  , import'qualified :: !Bool                  -- ^ is module imported qualified
  , import'as        :: !(Maybe VarName)       -- ^ as synonym for module name
  , import'list      :: !(Maybe ImportList)    -- ^ explicit import list
  }
  deriving stock (Eq, Show, Generic, Data, Typeable)

-- | Import list
data ImportList = ImportList
  { importList'loc    :: Loc             -- ^ source code location
  , importList'hides  :: Bool            -- ^ if True import hides items otherwise imports
  , importList'items  :: [ImportItem]    -- ^ list of items for import or exclusion
  }
  deriving stock (Eq, Show, Generic, Data, Typeable)

-- | Import item for the import list.
data ImportItem
  = ImportVar VarName                 -- ^ import variable
  | ImportAbs VarName                 -- ^ import type or typesymomym by name only
  | ImportTypeAll VarName             -- ^ import datatype with all constructors, i.e. @T(..)@
  | ImportTypeWith VarName [VarName]  -- ^ import type with some contructors
  deriving (Eq, Show, Generic, Data, Typeable)

getModuleCtxNames :: ModuleCtx -> [Text]
getModuleCtxNames = M.keys . H.unContext . inferCtx'binds . moduleCtx'types

-- | Type-inference context.
data InferCtx = InferCtx
  { inferCtx'binds :: TypeContext  -- ^ Already derived signatures for
                                   -- all free variables in the expression
  , inferCtx'types :: UserTypeCtx  -- ^ User-defined types
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid InferCtx

userTypesToInferCtx :: UserTypeCtx -> InferCtx
userTypesToInferCtx ctx = InferCtx emptyTypeContext ctx

-- | Context for execution (reduction) of expressions of the language
newtype ExecCtx = ExecCtx
  { execCtx'vars  :: Map VarName Lang  -- ^ bindings for free variables, outer scope of the execution
  } deriving newtype (Semigroup, Monoid)

recFieldExecCtx :: UserTypeCtx -> ExecCtx
recFieldExecCtx UserTypeCtx{..} =
  ExecCtx $ M.mapKeys (VarName noLoc) $ M.mapWithKey recSelectorExpr userTypeCtx'recFields

recSelectorExpr :: Text -> (ConsName, RecordFieldOrder) -> Lang
recSelectorExpr field (cons, RecordFieldOrder fields) =
  Fix $ Lam noLoc (PCons noLoc cons args) $ Fix $ Var noLoc v
  where
    v = VarName noLoc "$v"
    args = fmap (\x -> if (field == x) then (PVar noLoc v) else PWildCard noLoc) fields

-------------------------------------------------

mainExprModule :: Lang -> Module
mainExprModule expr = simpleModule (bind "main" expr)

simpleModule :: Binds Lang -> Module
simpleModule = Module noLoc Nothing [] mempty

bind :: Text -> Lang -> Binds Lang
bind name expr = simpleBind (VarName noLoc name) expr

simpleBind :: VarName -> Lang -> Binds Lang
simpleBind v a = Binds mempty [FunBind v [Alt [] (UnguardedRhs a) Nothing]]

------------------------------------------------------
-- instances

deriving newtype instance Show ExecCtx
deriving newtype instance Eq   ExecCtx
deriving stock   instance Show ModuleCtx
deriving stock   instance Eq   ModuleCtx
deriving stock   instance Show InferCtx
deriving stock   instance Eq   InferCtx
deriving stock   instance Show Module
