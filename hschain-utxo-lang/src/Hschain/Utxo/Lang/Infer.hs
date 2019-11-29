module Hschain.Utxo.Lang.Infer(
    inferExpr
  , constraintsExpr
) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Fix
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Text (Text)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Infer.Constraint
import Hschain.Utxo.Lang.Infer.Monad
import Hschain.Utxo.Lang.Infer.Subst
import Hschain.Utxo.Lang.Infer.TypeEnv

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: TypeEnv -> Lang -> Either TypeError Scheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: TypeEnv -> Lang -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty

