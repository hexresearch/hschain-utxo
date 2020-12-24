-- | Simplifies lambda-calculus expressions
module Hschain.Utxo.Lang.Desugar.Lambda(
    joinLamArgs
  , joinLetBinds
  , removeInfixApply
  , simplifyLet
  , substLamPats
  , desugarLambdaCalculus
) where

import Data.Fix

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Desugar.PatternCompiler

import qualified Type.Check.HM as H
import qualified Data.Map.Strict as M
import qualified Data.Vector as V


-- | Single function that brings together several lambda-calculus transformations.
--
-- * Substitutes let-binds with patterns to case expressions
-- * Joins arguments of lambda abstractions
-- * Joins Let-bindings to single one
-- * Transforms infix applications to prefix ones
-- * Removes explicit type-annotations
-- * Simplifies Let-expressions.
-- * Substitute patterns in lambda-functions for variables and let bindings
--
-- Note that order of trasformation matters
desugarLambdaCalculus :: MonadLang m => Lang -> m Lang
desugarLambdaCalculus =
  fmap (removeInfixApply) . substLamPats . joinLamArgs <=< simplifyLet . joinLetBinds

-- | Aggregates all lambda arguments to lists. It converts:
--
-- \x -> \y -> f x y ===> \x y -> f x y
--
-- This step eliminates cases with single Lam argument completely
joinLamArgs :: Lang -> Lang
joinLamArgs = foldFix $ \case
  Lam     loc p1 (Fix (Lam     _ p2 body)) -> Fix $ LamList loc [p1, p2] body
  Lam     loc p1 (Fix (LamList _ p2 body)) -> Fix $ LamList loc (p1 : p2) body
  Lam     loc p1 body                      -> Fix $ LamList loc [p1] body
  LamList loc p1 (Fix (Lam     _ p2 body)) -> Fix $ LamList loc (p1 ++ [p2]) body
  LamList loc p1 (Fix (LamList _ p2 body)) -> Fix $ LamList loc (p1 ++ p2) body
  other                                    -> Fix other

-- | Aggregates let-bindings
-- collects all let bindings together
--
-- > let x = expr1 in let y = expr2 in f x y  ===> let { x = expr1; y = expr2 } in f x y
joinLetBinds :: Lang -> Lang
joinLetBinds = foldFix $ \case
  Let loc bg1 (Fix (Let _ bg2 body)) -> Fix $ Let loc (bg1 <> bg2) body
  other                              -> Fix other

-- | substitutes infix application for regular prefix one.
--
-- > a + b ===> (+ a b)
removeInfixApply :: Lang -> Lang
removeInfixApply = foldFix $ \case
  InfixApply loc a var b -> Fix (Apply loc (Fix (Apply loc (Fix $ Var loc var) a)) b)
  other                  -> Fix other

-- | converts let-bindings to simple bindings like
-- also removes pattern-binds
--
-- > name = expr
simplifyLet :: MonadLang m => Lang -> m Lang
simplifyLet = foldFixM $ \case
  Let loc binds body -> do
    let (Binds types decls) = sortBinds binds
    go types loc body [] decls
  other             -> pure $ Fix other
  where
    go types loc body prevBinds xs = case xs of
      []   -> case prevBinds of
                [] -> pure body
                _  -> pure $ toPrimLet body
      b:bs -> case b of
        FunBind{..} -> do
          simpleBind <- fmap ((bind'name, ) . restrictType bind'name) $ altGroupToTupleExpr bind'alts
          go types loc body (simpleBind : prevBinds) bs
        PatBind{..} -> do
                  body1 <- go types loc body [] bs
                  rhs <- altGroupToTupleExpr [bind'alt]
                  return $ toPrimLet $ Fix $ CaseOf loc rhs [CaseExpr bind'pat body1]
      where
        restrictType name expr = maybe expr (\ty -> Fix $ Ascr (H.getLoc name) expr ty) $ M.lookup name types

        toPrimLet e = Fix $ PrimLet loc (reverse prevBinds) e

-- | Substitutes pattersn in lambda arguments for case+let
-- do this step after elimination of single Lams so
-- that we can consider only cases with LamList
substLamPats :: MonadLang m => Lang -> m Lang
substLamPats = foldFixM $ \case
  LamList loc ps body -> fromLamList loc ps body
  other               -> return $ Fix other
  where
    fromLamList loc patterns body = do
      (as, ps) <- fmap (\(args, pats) -> (reverse args, reverse pats)) $ foldM collectPats ([], []) patterns
      return $ case ps of
        []  -> Fix $ LamList loc as body
        [(v, p)] ->
               let ploc = H.getLoc p
               in  Fix $ LamList loc as $ Fix $ CaseOf ploc (toVarArg v) [CaseExpr p body]
        p:_      ->
               let ploc = H.getLoc $ fst p
               in  Fix $ LamList loc as $ Fix $ CaseOf ploc (toTupleArg $ fmap fst ps) [CaseExpr (toTuplePat $ fmap snd ps) body]

    collectPats (args, pats) p = case p of
      PVar _ _ -> return (p : args, pats)
      other    -> do
        let loc = H.getLoc other
        v <- getFreshVar loc
        return (PVar loc v : args, (v, p) : pats)

    toTupleArg vs = Fix $ Tuple (H.getLoc $ head vs) $ fmap toVarArg $ V.fromList vs
    toVarArg v = Fix $ Var (H.getLoc v) v
    toTuplePat ps = PTuple (H.getLoc $ head ps) ps

