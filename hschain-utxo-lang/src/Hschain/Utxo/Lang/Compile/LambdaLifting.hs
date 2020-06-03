-- | Implements lambda-lifting
--
-- It is implemented by the algorithm in the book
-- "Implementing Fun tional Languages: a tutorial" by Simon L Peyton Jones and David R Lester
-- (see Chapter 6).
--
-- It does not implement step with full-laziness.
-- Implemented up to mark 3 (complete lambda-lifting with optimizations).
module Hschain.Utxo.Lang.Compile.LambdaLifting(
    lambdaLifting
  , fuseLams
) where

import Data.Fix

import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.LambdaLifting.Abstract
import Hschain.Utxo.Lang.Compile.LambdaLifting.Collect
import Hschain.Utxo.Lang.Compile.LambdaLifting.FreeVars
import Hschain.Utxo.Lang.Compile.LambdaLifting.Rename

lambdaLifting :: CoreProg -> CoreProg
lambdaLifting = collect . rename . abstract . annotateFreeVars . fuseLams

-- | Fusion for lambdas and arguments of combinators
--
-- fusion of arguments of combinators:
--
-- >  f = \x -> g x  ===> f x = g x
--
-- fusion of sequential lambdas (accumulation of arguments)
--
-- > f = \x -> \y -> g x y ===> f = \x y -> g x y
fuseLams :: CoreProg -> CoreProg
fuseLams = fmap (fuseLamCombArgs . fmap fuseLamExpr)

fuseLamCombArgs :: Comb Name -> Comb Name
fuseLamCombArgs def@Def{..} =
  case unFix def'body of
    ELam args body -> fuseLamCombArgs $ def
                        { def'args = def'args ++ args
                        , def'body = body
                        }
    _              -> def

fuseLamExpr :: Expr Name -> Expr Name
fuseLamExpr = cata $ \case
  ELam args1 (Fix (ELam args2 body)) -> Fix $ ELam (args1 ++ args2) body
  other                              -> Fix other

