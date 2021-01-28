-- | This module defines simple transformations that
-- remove redundant language constructions.
--
-- They can be convenient to use in the code but
-- can be expressed through the more compact core expressions.
module Hschain.Utxo.Lang.Desugar(
    desugar
  , unfoldLamList
  , unfoldInfixApply
  , singleLet
  , app1
  , app2
  , bindBodyToExpr
  , simpleBind
  , module Hschain.Utxo.Lang.Desugar.FreshVar
  , module Hschain.Utxo.Lang.Desugar.PatternCompiler
  , module Hschain.Utxo.Lang.Desugar.Records
) where

import Data.Fix
import Data.Map.Strict (Map)

import Type.Check.HM (getLoc)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.UserType
import Hschain.Utxo.Lang.Desugar.FreshVar
import Hschain.Utxo.Lang.Desugar.PatternCompiler
import Hschain.Utxo.Lang.Desugar.Records

import qualified Data.List as L
import qualified Data.Map.Strict as M

desugar :: MonadLang m => UserTypeCtx -> Lang -> m Lang
desugar ctx expr = removeRecordCons ctx expr

unfoldLamList :: Loc -> [Pat] -> Lang -> Lang
unfoldLamList loc pats a = L.foldl' (\z x -> z . Fix . Lam loc x) id pats a

singleLet :: Loc -> VarName -> Lang -> Lang -> Lang
singleLet loc v body expr = Fix $ Let loc (simpleBind v body) expr

unfoldInfixApply :: Loc -> Lang -> VarName -> Lang -> Lang
unfoldInfixApply loc a v b = app2 (Fix $ Var loc v) a b

app1 :: Lang -> Lang -> Lang
app1 f a = Fix (Apply (getLoc a) f a)

app2 :: Lang -> Lang -> Lang -> Lang
app2 f a b = Fix (Apply (getLoc f) (Fix (Apply (getLoc a) f a)) b)

bindBodyToExpr :: MonadLang m => Map VarName Signature -> Bind Lang -> m Lang
bindBodyToExpr typeMap = \case
  FunBind{..} -> fmap (addSignatureCheck bind'name) $ altGroupToExpr bind'alts
  PatBind{..} -> unexpected "Pattern bind"
  where
    addSignatureCheck var = maybe id (\ty x -> Fix $ Ascr (getLoc ty) x ty) $ M.lookup var typeMap

simpleBind :: VarName -> Lang -> Binds Lang
simpleBind v a = Binds mempty [FunBind v [Alt [] (UnguardedRhs a) Nothing]]

-----------------------------------------------------------------

