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
rename :: LamProg -> LamProg
rename (LamProg prog) = LamProg $ runRenameM $ mapM renameComb prog

renameComb :: Comb Name -> RenameM (Comb Name)
renameComb def@Def{..} = do
  (args', env) <- allocNames def'args
  body' <- renameExpr env def'body
  return $ def
    { def'args = args'
    , def'body = body'
    }

renameExpr :: Map Name Name -> ExprLam Name -> RenameM (ExprLam Name)
renameExpr env (Fix expr) =
  case expr of
    EVar loc v          -> var loc v
    EPrim loc p         -> prim loc p
    EAp loc f a         -> app loc f a
    EIf loc a b c       -> iff loc a b c
    ELam loc args e     -> lam loc args e
    ELet loc binds e    -> letExpr loc binds e
    EConstr loc ty m n  -> constr loc ty m n
    ECase loc e alts    -> caseExpr loc e alts
    EAssertType loc e t -> assertType loc e t
    EBottom loc         -> pure $ Fix $ EBottom loc
  where
    var loc v = return $ Fix $ EVar loc $ fromMaybe v $ M.lookup v env

    prim loc p = return $ Fix $ EPrim loc p

    app loc f a = do
      f' <- renameExpr env f
      a' <- renameExpr env a
      return $ Fix $ EAp loc f' a'

    iff loc a b c = do
      a' <- renameExpr env a
      b' <- renameExpr env b
      c' <- renameExpr env c
      return $ Fix $ EIf loc a' b' c'

    lam loc args body = do
      (args', env') <- allocNames args
      body' <- renameExpr (env' <> env) body
      return $ Fix $ ELam loc args' body'

    letExpr loc binds body = do
      (bindNames', env') <- allocNames $ fmap fst binds
      let bodyEnv = env' <> env
      body' <- renameExpr bodyEnv body
      rhss' <- mapM (renameExpr bodyEnv . snd) binds
      return $ Fix $ ELet loc (zip bindNames' rhss') body'

    assertType loc e t = do
      e' <- renameExpr env e
      return $ Fix $ EAssertType loc e' t

    constr loc ty m n = return $ Fix $ EConstr loc ty m n

    caseExpr loc e alts = do
      e'    <- renameExpr env e
      alts' <- mapM (renameCaseAlts env) alts
      return $ Fix $ ECase loc e' alts'

renameCaseAlts :: Map Name Name -> CaseAlt Name (ExprLam Name) -> RenameM (CaseAlt Name (ExprLam Name))
renameCaseAlts env CaseAlt{..} = do
  (args', env') <- allocTypedNames caseAlt'args
  rhs' <- renameExpr (env' <> env) caseAlt'rhs
  return $ CaseAlt caseAlt'loc caseAlt'tag args' caseAlt'constrType rhs'

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

allocTypedNames :: [Typed Name] -> RenameM ([Typed Name], Map Name Name)
allocTypedNames tyNames = do
  (newNames, env) <- allocNames names
  return (zipWith Typed newNames tys, env)
  where
    tys   = fmap typed'type  tyNames
    names = fmap typed'value tyNames

