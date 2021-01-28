-- | Module level definitions and evaluation contexts.
module Hschain.Utxo.Lang.Module(
    Module(..)
  , ModuleCtx(..)
  , ExecCtx(..)
  , InferCtx(..)
  , recFieldExecCtx
  , getModuleCtxNames
  , userTypesToInferCtx
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
  { module'loc       :: !Loc          -- ^ source code location
  , module'userTypes :: !UserTypeCtx  -- ^ user-defined types
  , module'binds     :: !(Binds Lang) -- ^ values (functions)
  } deriving (Data, Typeable)

-- | Evaluated module
data ModuleCtx = ModuleCtx
  { moduleCtx'types  :: !InferCtx
  , moduleCtx'exprs  :: !ExecCtx
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid ModuleCtx

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

------------------------------------------------------
-- instances

deriving newtype instance Show ExecCtx
deriving newtype instance Eq   ExecCtx
deriving stock   instance Show ModuleCtx
deriving stock   instance Eq   ModuleCtx
deriving stock   instance Show InferCtx
deriving stock   instance Eq   InferCtx
deriving stock   instance Show Module
