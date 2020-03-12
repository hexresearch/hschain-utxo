module Hschain.Utxo.Lang.Desugar(
    unfoldLamList
  , unfoldLetArg
  , unfoldInfixApply
  , singleLet
  , app2
  , app3
  , altToExpr
  , moduleToMainExpr
  , bindGroupToLet
) where

import Control.Applicative

import Data.Fix

import Language.HM (getLoc)

import Hschain.Utxo.Lang.Expr

import qualified Data.List as L
import qualified Data.List.Extra as L

unfoldLamList :: Loc -> [VarName] -> Lang -> Lang
unfoldLamList loc vars a = L.foldl' (\z a -> z . Fix . Lam loc a) id vars a

unfoldLetArg :: Loc -> VarName -> [VarName] -> Lang -> Lang -> Lang
unfoldLetArg loc v args a = singleLet loc v (Fix $ LamList loc args a)

singleLet :: Loc -> VarName -> Lang -> Lang -> Lang
singleLet loc v body expr = Fix $ Let loc bg expr
  where
    bg = [Bind v Nothing [Alt [] body]]

unfoldInfixApply :: Loc -> Lang -> VarName -> Lang -> Lang
unfoldInfixApply loc a v b = app2 (Fix $ Var loc v) a b
-- Fix $ Apply loc (Fix (Apply loc (Fix $ Var loc v) a)) b

bindGroupToLet :: [BindGroup Lang] -> Lang -> Lang
bindGroupToLet bgs expr = foldr go expr bgs
  where
    go x res = Fix $ Let noLoc x res

moduleToMainExpr :: Module -> Either String Lang
moduleToMainExpr prog = case findMain prog of
  Nothing   -> Left "There is no main expression defined in the module"
  Just main -> Right $ bindGroupToLet (module'binds $ rmMain prog) (addBoolTypeCheck main)
  where
    findMain :: Module -> Maybe Lang
    findMain Module{..} = L.firstJust onBg module'binds
      where
        onBg bs = foldr (<|>) Nothing $ fmap getMain bs
          where
            getMain Bind{..}
              | isMain bind'name = altToLam bind'alts
              | otherwise        = Nothing

    altToLam alts = case alts of
      [alt] -> Just $ altToExpr alt
      _     -> Nothing

    addBoolTypeCheck :: Lang -> Lang
    addBoolTypeCheck expr = Fix $ Ascr (getLoc expr) expr boolT

    rmMain :: Module -> Module
    rmMain m@Module{..} = m { module'binds = fmap rm module'binds }
      where
        rm = filter noMain

        noMain = not . isMain . bind'name

    isMain :: VarName -> Bool
    isMain = (== "main") . varName'name


app2 :: Lang -> Lang -> Lang -> Lang
app2 f a b = Fix (Apply (getLoc f) (Fix (Apply (getLoc a) f a)) b)

app3 :: Lang -> Lang -> Lang -> Lang -> Lang
app3 f a b c = Fix $ Apply (getLoc f) (app2 f a b) c

altToExpr :: Alt Lang -> Lang
altToExpr Alt{..} = case alt'pats of
  []   -> alt'expr
  pats -> Fix $ LamList (getLoc alt'expr) (fmap toArg pats) $ alt'expr
  where
    toArg (PVar _ var) = var


