-- | Simplifies lambda-calculus expressions
module Hschain.Utxo.Lang.Desugar.Lambda(
    joinLamArgs
  , joinLetBinds
  , removeInfixApply
  , removeAscr
  , simplifyLet
  , desugarLambdaCalculus
) where

import Data.Fix

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Desugar.PatternCompiler

-- | Single function that brings together several lambda-calculus transformations.
--
-- * Joins arguments of lambda abstractions
-- * Joins Let-bindings to single one
-- * Transforms infix applications to prefix ones
-- * Removes explicit type-annotations
-- * Simplifies Let-expressions.
desugarLambdaCalculus :: MonadLang m => Lang -> m Lang
desugarLambdaCalculus =
  simplifyLet . removeAscr . removeInfixApply . joinLetBinds . joinLamArgs

-- | Aggregates all lambda arguments to lists. It converts:
--
-- \x -> \y -> f x y ===> \x y -> f x y
--
-- This step eliminates cases with single Lam argument completely
joinLamArgs :: Lang -> Lang
joinLamArgs = cata $ \case
  Lam     loc p1 (Fix (Lam     _ p2 body)) -> Fix $ LamList loc [p1, p2] body
  Lam     loc p1 (Fix (LamList _ p2 body)) -> Fix $ LamList loc (p1 : p2) body
  Lam     loc p1 body                      -> Fix $ LamList loc [p1] body
  LamList loc p1 (Fix (Lam     _ p2 body)) -> Fix $ LamList loc (p1 ++ [p2]) body
  LamList loc p1 (Fix (LamList _ p2 body)) -> Fix $ LamList loc (p1 ++ p2) body
  other                                    -> Fix other

-- | Aggregates let-bindings
-- collects all let bindings together
--
-- > let x = expr1 in let y = expr2 in f x y  ===> let { x = expr1; y = expr2 } in f x y
joinLetBinds :: Lang -> Lang
joinLetBinds = cata $ \case
  Let loc bg1 (Fix (Let _ bg2 body)) -> Fix $ Let loc (bg1 ++ bg2) body
  other                              -> Fix other

-- | substitutes infix application for regular prefix one.
--
-- > a + b ===> (+ a b)
removeInfixApply :: Lang -> Lang
removeInfixApply = cata $ \case
  InfixApply loc a var b -> Fix (Apply loc (Fix (Apply loc (Fix $ Var loc var) a)) b)
  other                  -> Fix other

-- | Removes explicit type-annotations provided by user
--
-- > (a :: Int) ===> a
removeAscr :: Lang -> Lang
removeAscr = cata $ \case
  Ascr _ a _ -> a
  other      -> Fix other

-- | converts let-bindings to simple bindings like
--
-- > name = expr
simplifyLet :: MonadLang m => Lang -> m Lang
simplifyLet = cataM $ \case
  Let loc binds body -> do
    binds' <- mapM simplify (sortBindGroups binds)
    return $ Fix $ PrimLet loc binds' body
  other             -> pure $ Fix other
  where
    simplify Bind{..} = fmap (bind'name, ) $ altGroupToExpr bind'alts


