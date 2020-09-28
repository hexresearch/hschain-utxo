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

import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.LambdaLifting.Abstract
import Hschain.Utxo.Lang.Compile.LambdaLifting.Collect
import Hschain.Utxo.Lang.Compile.LambdaLifting.FreeVars
import Hschain.Utxo.Lang.Compile.LambdaLifting.Rename

-- | Lambda-lifting eliminates all lambda-expressions and substitutes
-- them with global functions (they are called supercombinators).
--
-- It compiles program of extended lambda-calculus to the representation
-- suitable for evaluation on G-machine.
lambdaLifting :: LamProg -> LamProg
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
fuseLams :: LamProg -> LamProg
fuseLams (LamProg prog) = LamProg $ fmap (fuseLamCombArgs . fmap fuseLamExpr) prog

fuseLamCombArgs :: Comb Name -> Comb Name
fuseLamCombArgs def@Def{..} =
  case unFix def'body of
    ELam _ args body -> fuseLamCombArgs $ def
                         { def'args = def'args ++ args
                         , def'body = body
                         }
    _              -> def

fuseLamExpr :: ExprLam Name -> ExprLam Name
fuseLamExpr = cata $ \case
  ELam loc args1 (Fix (ELam _ args2 body)) -> Fix $ ELam loc (args1 ++ args2) body
  other                                    -> Fix other

