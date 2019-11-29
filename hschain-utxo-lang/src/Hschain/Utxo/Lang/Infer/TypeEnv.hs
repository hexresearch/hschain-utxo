module Hschain.Utxo.Lang.Infer.TypeEnv (
  TypeEnv(..),
  empty,
  lookup,
  remove,
  extend,
  extends,
  merge,
  mergeEnvs,
  singleton,
  keys,
  fromList,
  toList,
) where

import Prelude hiding (lookup)

import Hschain.Utxo.Lang.Expr

import Data.Monoid
import Data.Foldable hiding (toList)
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

newtype TypeEnv = TypeEnv { unTypeEnv :: Map.Map VarName Scheme }
  deriving (Eq, Show, Semigroup, Monoid)

empty :: TypeEnv
empty = TypeEnv Map.empty

extend :: TypeEnv -> (VarName, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

remove :: TypeEnv -> VarName -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: TypeEnv -> [(VarName, Scheme)] -> TypeEnv
extends (TypeEnv env) xs = TypeEnv $ Map.union (Map.fromList xs) env

lookup :: VarName -> TypeEnv -> Maybe Scheme
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: TypeEnv -> TypeEnv -> TypeEnv
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [TypeEnv] -> TypeEnv
mergeEnvs = foldl' merge empty

singleton :: VarName -> Scheme -> TypeEnv
singleton x y = TypeEnv (Map.singleton x y)

keys :: TypeEnv -> [VarName]
keys (TypeEnv env) = Map.keys env

fromList :: [(VarName, Scheme)] -> TypeEnv
fromList xs = TypeEnv (Map.fromList xs)

toList :: TypeEnv -> [(VarName, Scheme)]
toList (TypeEnv env) = Map.toList env

