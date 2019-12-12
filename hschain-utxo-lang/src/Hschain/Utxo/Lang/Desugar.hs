module Hschain.Utxo.Lang.Desugar(
    unfoldLamList
  , unfoldLetArg
  , unfoldInfixApply
  , singleLet
  , explToImpl
  , app2
  , app3
  , altToExpr
) where

import Data.Fix

import Type.Loc
import Type.Type
import Hschain.Utxo.Lang.Expr

import qualified Data.List as L

unfoldLamList :: Loc -> [VarName] -> Lang -> Lang
unfoldLamList loc vars a = L.foldl' (\z a -> z . Fix . Lam loc a) id vars a

unfoldLetArg :: Loc -> VarName -> [VarName] -> Lang -> Lang -> Lang
unfoldLetArg loc v args a = singleLet loc v (Fix $ LamList loc args a)

singleLet :: Loc -> VarName -> Lang -> Lang -> Lang
singleLet loc v body expr = Fix $ Let loc bg expr
  where
    bg = BindGroup expl impl

    expl = []

    impl = [[Impl (fromVarName v) [Alt [] body]]]

explToImpl :: Expl a -> Impl a
explToImpl Expl{..} = Impl expl'name expl'alts

unfoldInfixApply :: Loc -> Lang -> VarName -> Lang -> Lang
unfoldInfixApply loc a v b = app2 (Fix $ Var loc v) a b
-- Fix $ Apply loc (Fix (Apply loc (Fix $ Var loc v) a)) b

moduleToMainExpr :: Module -> Lang
moduleToMainExpr = undefined

app2 :: Lang -> Lang -> Lang -> Lang
app2 f a b = Fix (Apply (getLoc f) (Fix (Apply (getLoc a) f a)) b)

app3 :: Lang -> Lang -> Lang -> Lang -> Lang
app3 f a b c = Fix $ Apply (getLoc f) (app2 f a b) c

altToExpr :: Alt Lang -> Lang
altToExpr Alt{..} = case alt'pats of
  []   -> alt'expr
  pats -> Fix $ LamList (getLoc alt'expr) (fmap toArg pats) $ alt'expr
  where
    toArg (PVar _ var) = toVarName var



