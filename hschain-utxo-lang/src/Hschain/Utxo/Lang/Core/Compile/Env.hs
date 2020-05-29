-- | Environment for compilation of supercombinators
-- includes information on argument list and offsets in the result code.
module Hschain.Utxo.Lang.Core.Compile.Env(
    Env
  , initEnv
  , lookupEnv
  , argOffset
  , getArity
  , insertEnv
) where

import Data.Map.Strict (Map)
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Map.Strict as M
import qualified Data.Vector     as V

-- | Environment for compilation of supercombinator
data Env = Env
  { env'offsets :: Map Name Int
  -- ^ map of offsets for arguments of supercombinator
  , env'arity   :: !Int
  -- ^ arity of supercombinator
  }

-- | Initialize environment from list of argument names.
initEnv :: Vector Name -> Env
initEnv args = Env
  { env'offsets = M.fromList $ zip (V.toList args) [0..]
  , env'arity   = V.length args
  }

-- | Lookup the offset for a given variable.
-- If it is not in the domain we return @Nothing@.
lookupEnv :: Name -> Env -> Maybe Int
lookupEnv name Env{..} =
  M.lookup name env'offsets

-- | Add fixed number to all offsets.
argOffset :: Int -> Env -> Env
argOffset n env =
  env { env'offsets = fmap (+ n) $ env'offsets env }

-- | Get arity of the arg-list.
getArity :: Env -> Int
getArity = env'arity

insertEnv :: Name -> Int -> Env -> Env
insertEnv name n e =
  e { env'offsets = M.insert name n $ env'offsets e }

