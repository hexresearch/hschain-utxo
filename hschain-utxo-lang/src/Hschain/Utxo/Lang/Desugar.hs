-- | This module defines simple transformations that
-- remove redundant language constructions.
--
-- They can be convenient to use in the code but
-- can be expressed through the more compact core expressions.
module Hschain.Utxo.Lang.Desugar(
    desugar
  , unfoldLamList
  , unfoldLetArg
  , unfoldInfixApply
  , singleLet
  , app1
  , app2
  , app3
  , bindBodyToExpr
  , bindGroupToLet
  , simpleBind
  , desugarRecordUpdate
  , recordFieldUpdateFunName
  , module Hschain.Utxo.Lang.Desugar.FreshVar
  , module Hschain.Utxo.Lang.Desugar.PatternCompiler
  , module Hschain.Utxo.Lang.Desugar.Records
) where


import Data.Fix

import Type.Check.HM (getLoc)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Desugar.FreshVar
import Hschain.Utxo.Lang.Desugar.PatternCompiler
import Hschain.Utxo.Lang.Desugar.Records

import qualified Data.List as L

desugar :: MonadLang m => UserTypeCtx -> Lang -> m Lang
desugar ctx expr = removeRecordCons ctx expr

unfoldLamList :: Loc -> [Pat] -> Lang -> Lang
unfoldLamList loc pats a = L.foldl' (\z x -> z . Fix . Lam loc x) id pats a

unfoldLetArg :: Loc -> VarName -> [VarName] -> Lang -> Lang -> Lang
unfoldLetArg loc v args a = singleLet loc v (Fix $ LamList loc (fmap varToPat args) a)

varToPat :: VarName -> Pat
varToPat v = PVar (getLoc v) v

singleLet :: Loc -> VarName -> Lang -> Lang -> Lang
singleLet loc v body expr = Fix $ Let loc [simpleBind v body] expr

unfoldInfixApply :: Loc -> Lang -> VarName -> Lang -> Lang
unfoldInfixApply loc a v b = app2 (Fix $ Var loc v) a b

bindGroupToLet :: [Bind Lang] -> Lang -> Lang
bindGroupToLet bgs expr = Fix $ Let noLoc bgs expr


app1 :: Lang -> Lang -> Lang
app1 f a = Fix (Apply (getLoc a) f a)

app2 :: Lang -> Lang -> Lang -> Lang
app2 f a b = Fix (Apply (getLoc f) (Fix (Apply (getLoc a) f a)) b)

app3 :: Lang -> Lang -> Lang -> Lang -> Lang
app3 f a b c = Fix $ Apply (getLoc f) (app2 f a b) c

bindBodyToExpr :: MonadLang m => Bind Lang -> m Lang
bindBodyToExpr Bind{..} = fmap addSignatureCheck $ altGroupToExpr bind'alts
  where
    addSignatureCheck = maybe id (\ty x -> Fix $ Ascr (getLoc ty) x ty) bind'type

simpleBind :: VarName -> Lang -> Bind Lang
simpleBind v a = Bind v Nothing [Alt [] (UnguardedRhs a) Nothing]

-----------------------------------------------------------------

desugarRecordUpdate :: VarName -> Lang -> Lang -> Lang
desugarRecordUpdate field val expr =
  app2 (Fix $ Var (getLoc field) $ recordFieldUpdateFunName field) val expr

recordFieldUpdateFunName :: VarName -> VarName
recordFieldUpdateFunName VarName{..} = VarName
  { varName'loc  = varName'loc
  , varName'name = secretVar $ mappend "update_" varName'name
  }

