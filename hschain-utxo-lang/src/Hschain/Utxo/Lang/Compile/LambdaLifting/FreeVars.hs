-- | Annotates expression tree with information on free-variables
-- for all sub-expressions. So that we know which variables are
-- candidates for abstractions
module Hschain.Utxo.Lang.Compile.LambdaLifting.FreeVars(
  annotateFreeVars
) where

import Data.Fix
import Data.Set (Set)

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Set        as S

-- | Annotates expression tree with free vars.
annotateFreeVars :: LamProg -> AnnLamProg (Set Name) Name
annotateFreeVars (LamProg prog) = AnnLamProg $ fmap onDef prog
  where
    onDef def@Def{..} = fmap (getFreeVars initLocals) def
      where
        initLocals = S.fromList def'args

getLocals :: AnnExprLam (Set Name) Name -> Set Name
getLocals = ann'note . unFix

getFreeVars :: Set Name -> ExprLam Name -> AnnExprLam (Set Name) Name
getFreeVars localVars (Fix x) = case x of
  EPrim loc p -> prim loc p
  EPrimOp loc op -> primOp loc op
  EVar loc v  -> var loc v
  EAp loc a b -> app loc a b
  ELam loc as e -> lam loc as e
  EIf loc a b c -> iff loc a b c
  EConstr loc ty m n -> constr loc ty m n
  ECase loc e alts -> cases loc e alts
  ELet loc binds body -> letExpr loc binds body
  EAssertType loc e ty -> assertType loc e ty
  EBottom loc -> bottom loc
  where

    prim loc p = Fix $ Ann S.empty (EPrim loc p)
    primOp loc op = Fix $ Ann S.empty (EPrimOp loc op)

    var loc v = Fix $
      if (S.member v localVars)
        then Ann (S.singleton v) (EVar loc v)
        else Ann S.empty (EVar loc v)

    app loc a b = Fix $ Ann (getLocals ea <> getLocals eb) (EAp loc ea eb)
      where
        ea = getFreeVars localVars a
        eb = getFreeVars localVars b

    lam loc args e = Fix $ Ann (getLocals ebody S.\\ argVars) (ELam loc args ebody)
      where
        ebody   = getFreeVars newVars e
        newVars = localVars <> argVars
        argVars = S.fromList args

    iff loc a b c = Fix $ Ann (getLocals ea <> getLocals eb <> getLocals ec) (EIf loc ea eb ec)
      where
        ea = getFreeVars localVars a
        eb = getFreeVars localVars b
        ec = getFreeVars localVars c

    constr loc ty m n = Fix $ Ann S.empty (EConstr loc ty m n)

    cases loc expr alts = Fix $ Ann (getLocals expr') $ ECase loc expr' $ fmap (freeVarAlts localVars) alts
      where
        expr' = getFreeVars localVars expr

    letExpr loc binds body = Fix $ Ann (defnsFree <> bodyFree) (ELet loc ebinds ebody)
      where
        rhss = fmap (getFreeVars localVars) $ fmap snd binds
        binders = fmap fst binds
        binderSet = S.fromList binders
        bodyLocals = binderSet <> localVars
        ebinds = zip binders rhss

        freeInValues = foldMap getLocals rhss
        defnsFree = freeInValues
        ebody = getFreeVars bodyLocals body
        bodyFree = getLocals ebody S.\\ binderSet

    assertType loc e ty = Fix $ Ann (getLocals ea) $ EAssertType loc ea ty
      where
        ea = getFreeVars localVars e

    bottom loc = Fix $ Ann mempty $ EBottom loc

freeVarAlts :: Set Name -> CaseAlt Name (ExprLam Name) -> CaseAlt Name (AnnExprLam (Set Name) Name)
freeVarAlts localVars alt@CaseAlt{..} =
  alt { caseAlt'rhs = Fix $ Ann (getLocals ebody S.\\ argVars) (ann'value $ unFix ebody) }
  where
    ebody   = getFreeVars newVars caseAlt'rhs
    newVars = localVars <> argVars
    argVars = S.fromList $ fmap typed'value caseAlt'args



