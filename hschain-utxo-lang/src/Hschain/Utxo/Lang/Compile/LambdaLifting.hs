module Hschain.Utxo.Lang.Compile.LambdaLifting(
  lambdaLifting
) where

import Hex.Common.Text

import Control.Monad.State.Strict

import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Strict.Tuple (Pair(..))
import Data.Set (Set)

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.List       as L
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

lambdaLifting :: CoreProg -> CoreProg
lambdaLifting = rename . abstract . annotateFreeVars

-- | Annotates expression tree with free vars.
annotateFreeVars :: CoreProg -> AnnProg (Set Name) Name
annotateFreeVars = fmap onDef
  where
    onDef def@Def{..} = fmap (getFreeVars initLocals) def
      where
        initLocals = S.fromList def'args

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
        getLocals = ann'note . unFix

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

        cases = undefined

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

-- | Gives name SC to all lambda-abstractions (showrt for supercombinator)
abstract :: AnnProg (Set Name) Name -> CoreProg
abstract = fmap (fmap abstractExpr)
  where
    abstractExpr :: AnnExpr (Set Name) Name -> Expr Name
    abstractExpr = cata $ \Ann{..} -> case ann'value of
      ELam args body -> lam ann'note args body
      other          -> Fix other

    lam freeVars args body =
      L.foldl (\f a -> Fix $ EAp f (Fix $ EVar a)) sc freeArgs
      where
        sc = Fix $ ELet [(scName, scRhs)] (Fix $ EVar scName)

        freeArgs = S.toList freeVars

        scName = "sc"
        scRhs  = Fix $ ELam (freeArgs ++ args) body

type FreshId = Int

type RenameM a = State FreshId a


runRenameM :: RenameM a -> a
runRenameM a = evalState a 0

-- | Rename all local variables in the functions.
-- We allocate new names for all top-level argument variables,
-- let bindings, case-bindings, lambda expression arguments
rename :: CoreProg -> CoreProg
rename prog = runRenameM $ mapM renameComb prog

renameComb :: Comb Name -> RenameM (Comb Name)
renameComb def@Def{..} = do
  (args', env) <- allocNames def'args
  body' <- renameExpr env def'body
  return $ def
    { def'args = args'
    , def'body = body'
    }

renameExpr :: Map Name Name -> Expr Name -> RenameM (Expr Name)
renameExpr env (Fix expr) =
  case expr of
    EVar v         -> var v
    EPrim p        -> prim p
    EAp f a        -> app f a
    EIf a b c      -> iff a b c
    ELam args e    -> lam args e
    ELet binds e   -> letExpr binds e
    EConstr ty m n -> constr ty m n
    ECase e alts   -> caseExpr e alts
  where
    var v = return $ Fix $ EVar $ fromMaybe v $ M.lookup v env

    prim p = return $ Fix $ EPrim p

    app f a = do
      f' <- renameExpr env f
      a' <- renameExpr env a
      return $ Fix $ EAp f' a'

    iff a b c = do
      a' <- renameExpr env a
      b' <- renameExpr env b
      c' <- renameExpr env c
      return $ Fix $ EIf a' b' c'

    lam args body = do
      (args', env') <- allocNames args
      body' <- renameExpr (env' <> env) body
      return $ Fix $ ELam args' body'

    letExpr binds body = do
      (bindNames', env') <- allocNames $ fmap fst binds
      let bodyEnv = env' <> env
      body' <- renameExpr bodyEnv body
      rhss' <- mapM (renameExpr bodyEnv . snd) binds
      return $ Fix $ ELet (zip bindNames' rhss') body'

    constr ty m n = return $ Fix $ EConstr ty m n

    caseExpr e alts = do
      e'    <- renameExpr env e
      alts' <- mapM (renameCaseAlts env) alts
      return $ Fix $ ECase e' alts'

renameCaseAlts :: Map Name Name -> CaseAlt (Expr Name) -> RenameM (CaseAlt (Expr Name))
renameCaseAlts env CaseAlt{..} = do
  (args', env') <- allocNames caseAlt'args
  rhs' <- renameExpr (env' <> env) caseAlt'rhs
  return $ CaseAlt caseAlt'tag args' rhs'

allocNames :: [Name] -> RenameM ([Name], Map Name Name)
allocNames oldNames = do
  freshId <- get
  let (freshId' :!: nameMap, newNames) = L.mapAccumL alloc (freshId :!: M.empty) oldNames
  put freshId'
  return (newNames, nameMap)
  where
    alloc (freshId :!: nameMap) oldName =
      (freshId + 1 :!: M.insert oldName newName nameMap, newName)
      where
        newName = toNewName freshId

    toNewName freshId = mappend "v" (showt freshId)

