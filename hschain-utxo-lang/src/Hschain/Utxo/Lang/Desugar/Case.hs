-- | We transform case-patterns so that they can contain
-- only constructors or tuples. We eliminate all catch-all
-- variables patterns, wildcards and primitives
module Hschain.Utxo.Lang.Desugar.Case(
  desugarCase
) where

import Data.Fix

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Exec.Subst
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Desugar

import qualified Language.HM as H

-- | TODO we need to transform patterns in similiar way as Pattern-compiler
-- prior to further transformations!

desugarCase :: MonadLang m => Module -> m Module
desugarCase m = do
  binds <- mapM (mapM (desugarCaseExpr $ module'userTypes m)) $ module'binds m
  return $ m { module'binds = binds }

desugarCaseExpr :: MonadLang m => UserTypeCtx -> Lang -> m Lang
desugarCaseExpr ctx = cataM $ \case
  CaseOf loc e alts -> do
    substForVar e $ (\v -> substAlts ctx loc v alts)
  other             -> pure $ Fix other

substForVar :: MonadLang m => Lang -> (Lang -> m Lang) -> m Lang
substForVar e cont = case e of
  Fix (Var _ _) -> cont e
  expr          -> do
    let loc = H.getLoc expr
    v <- getFreshVar loc
    let toLet body = Fix $ Let loc [simpleBind v expr] body
    fmap toLet $ cont (Fix $ Var loc v)

data LegalConsPat = ConsPat | TuplePat
  deriving (Show, Eq)

substAlts :: MonadLang m => UserTypeCtx -> Loc -> Lang -> [CaseExpr Lang] -> m Lang
substAlts ctx loc expr alts
  | allCons alts = return $ simpleCase loc expr alts
  | otherwise    = case alts of
    []   -> throwError $ PatError NoCasesLeft
    a:as -> case caseExpr'lhs a of
      PPrim ploc p   -> fromPrim ploc p (caseExpr'rhs a) as
      PVar  _ v      -> fromVar  v (caseExpr'rhs a)
      PWildCard ploc -> fromWildCard ploc (caseExpr'rhs a)
      PCons _ _ _    -> collectCons ConsPat  [a] as
      PTuple _ _     -> collectCons TuplePat [a] as
  where
    allCons = all (isConsOrTuple . caseExpr'lhs)

    isConsOrTuple = \case
      PCons _ _ _ -> True
      PTuple _ _  -> True
      _           -> False

    fromPrim ploc p rhs rest = case rest of
      []   -> return $ ifPrim $ Fix $ FailCase ploc
      a:as -> case caseExpr'lhs a of
                PPrim ploc2 p2 -> fmap ifPrim $ fromPrim ploc2 p2 (caseExpr'rhs a) as
                PVar _ v       -> return $ ifPrim $ subst rhs v expr
                PWildCard _    -> return $ ifPrim expr
                other          -> wrongPatPrimMixture $ H.getLoc other
      where
        ifPrim e = Fix $ If ploc (Fix $ BinOpE ploc Equals (Fix $ PrimE ploc p) expr) rhs e



    fromVar v rhs = return $ subst rhs v expr

    fromWildCard ploc rhs = do
      v <- getFreshVar ploc
      return $ Fix $ CaseOf loc expr [CaseExpr (PVar ploc v) rhs]

    collectCons legalConsPat res rest = case rest of
      []   -> throwError $ PatError NoCasesLeft
      a:as -> case caseExpr'lhs a of
        PCons _ _ _     | isCons  -> collectCons ConsPat (a:res) as
        PCons ploc _ _            -> wrongPatConsMixture ploc
        PTuple _ _      | isTuple -> collectCons TuplePat (a:res) as
        PTuple ploc _             -> wrongPatConsMixture ploc
        PPrim ploc _              -> wrongPatPrimMixture ploc
        PVar _ v                  -> completeVarCases ctx v (caseExpr'rhs a) (reverse res)
        PWildCard _               -> completeWildCardCases ctx (reverse res)
        where
          isCons  = legalConsPat == ConsPat
          isTuple = legalConsPat == TuplePat

    completeVarCases ctx v rhs conses = undefined
    completeWildCardCases = undefined

simpleCase :: Loc -> Lang -> [CaseExpr Lang] -> Lang
simpleCase loc expr alts = Fix $ CaseOf loc expr alts



