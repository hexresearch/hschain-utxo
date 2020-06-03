module Hschain.Utxo.Lang.Compile.LambdaLifting.FreeVars(
  annotateFreeVars
) where

import Data.Fix
import Data.Set (Set)

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Set        as S

-- | Annotates expression tree with free vars.
annotateFreeVars :: CoreProg -> AnnProg (Set Name) Name
annotateFreeVars = fmap onDef
  where
    onDef def@Def{..} = fmap (getFreeVars initLocals) def
      where
        initLocals = S.fromList def'args

getLocals :: AnnExpr (Set Name) Name -> Set Name
getLocals = ann'note . unFix

getFreeVars :: Set Name -> Expr Name -> AnnExpr (Set Name) Name
getFreeVars localVars (Fix x) = case x of
  EPrim p -> prim p
  EVar v  -> var v
  EAp a b -> app a b
  ELam as e -> lam as e
  EIf a b c -> iff a b c
  EConstr ty m n -> constr ty m n
  ECase e alts -> cases e alts
  ELet binds body -> letExpr binds body
  where

    prim p = Fix $ Ann S.empty (EPrim p)

    var v = Fix $
      if (S.member v localVars)
        then Ann (S.singleton v) (EVar v)
        else Ann S.empty (EVar v)

    app a b = Fix $ Ann (getLocals ea <> getLocals eb) (EAp ea eb)
      where
        ea = getFreeVars localVars a
        eb = getFreeVars localVars b

    lam args e = Fix $ Ann (getLocals ebody S.\\ argVars) (ELam args ebody)
      where
        ebody   = getFreeVars newVars e
        newVars = localVars <> argVars
        argVars = S.fromList args

    iff a b c = Fix $ Ann (getLocals ea <> getLocals eb <> getLocals ec) (EIf ea eb ec)
      where
        ea = getFreeVars localVars a
        eb = getFreeVars localVars b
        ec = getFreeVars localVars c

    constr ty m n = Fix $ Ann S.empty (EConstr ty m n)

    cases expr alts = Fix $ Ann (getLocals expr') $ ECase expr' $ fmap (freeVarAlts localVars) alts
      where
        expr' = getFreeVars localVars expr

    letExpr binds body = Fix $ Ann (defnsFree <> bodyFree) (ELet ebinds ebody)
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

freeVarAlts :: Set Name -> CaseAlt (Expr Name) -> CaseAlt (AnnExpr (Set Name) Name)
freeVarAlts localVars alt@CaseAlt{..} =
  alt { caseAlt'rhs = Fix $ Ann (getLocals ebody S.\\ argVars) (ann'value $ unFix ebody) }
  where
    ebody   = getFreeVars newVars caseAlt'rhs
    newVars = localVars <> argVars
    argVars = S.fromList caseAlt'args



