-- | Abstracts all free variables in lambda-expressions. And wraps them with let-expressions
--
-- > f (g x y) ==> f ((let sc = \x y -> g x y in sc) x y)
module Hschain.Utxo.Lang.Compile.LambdaLifting.Abstract(
  abstract
) where

import Data.Fix
import Data.Set (Set)

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.List       as L
import qualified Data.Set        as S

-- | Gives name SC to all lambda-abstractions (short for supercombinator)
-- and abstracts away all free variables in sub-expressions.
abstract :: AnnProg (Set Name) Name -> CoreProg
abstract = fmap (fmap abstractExpr)
  where
    abstractExpr :: AnnExpr (Set Name) Name -> Expr Name
    abstractExpr = cata $ \Ann{..} -> case ann'value of
      ELam args body -> lam ann'note args body
      other          -> Fix other

    lam freeVars args body =
      L.foldl (\f a -> Fix $ EAp f (Fix $ EVar a)) sc freeArgs
      where
        sc = Fix $ ELet [(scName, scRhs)] (Fix $ EVar scName)

        freeArgs = S.toList freeVars

        scName = "sc"
        scRhs  = Fix $ ELam (freeArgs ++ args) body




