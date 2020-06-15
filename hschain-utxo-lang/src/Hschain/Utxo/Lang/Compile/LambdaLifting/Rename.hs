-- | Defines function rename to give new fresh names to all local variables.
-- It prepares for safe movement of local labda-expressions to top-level supercombinators.
module Hschain.Utxo.Lang.Compile.LambdaLifting.Rename(
  rename
) where

import Control.Monad.State.Strict

import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Strict.Tuple (Pair(..))

import Hex.Common.Text

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.List       as L
import qualified Data.Map.Strict as M

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
    EBottom        -> pure $ Fix $ EBottom
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

renameCaseAlts :: Map Name Name -> CaseAlt Name (Expr Name) -> RenameM (CaseAlt Name (Expr Name))
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

    toNewName freshId = mappend "$v" (showt freshId)


