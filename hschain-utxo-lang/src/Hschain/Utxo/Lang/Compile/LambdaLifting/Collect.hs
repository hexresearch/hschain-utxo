module Hschain.Utxo.Lang.Compile.LambdaLifting.Collect(
  collect
) where

import Control.Monad.Writer.Strict

import Data.Fix
import Data.Foldable
import Data.Sequence (Seq)

import Hschain.Utxo.Lang.Expr (VarName(..))
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Sequence   as Seq

-- | Collects all lambdas to top-level supercombinators.
collect :: LamProg -> LamProg
collect (LamProg prog) = LamProg $ scs <> prog'
  where
    (prog', scs) = runCollectM $ mapM collectDef $ fmap (fmap fuseSingleLet) prog

type CollectM a = Writer (Seq (Comb Name)) a

runCollectM :: CollectM a -> (a, [Comb Name])
runCollectM a = (res, toList combs)
  where
    (res, combs) = runWriter a

collectDef :: Comb Name -> CollectM (Comb Name)
collectDef def@Def{..} =
  case unFix def'body of
    ELam _ args body -> collectDef $ def
                                { def'args = def'args ++ args
                                , def'body = body
                                }
    ELet _ [(var1, body)] (Fix (EVar _ var2)) | var1 == var2
                  -> collectDef $ def { def'body = body }
    _ -> mapM collectExpr def

collectExpr :: ExprLam Name -> CollectM (ExprLam Name)
collectExpr = cataM $ \case
  ELet loc binds body -> letExpr loc binds body
  other           -> pure $ Fix other
  where
    letExpr loc binds body = do
      tell (Seq.fromList scs)
      return $
        if null nonScs
          then body
          else Fix $ ELet loc nonScs body
      where
        (scs, nonScs) = partitionBy  getSc binds

    getSc :: (Name, ExprLam Name) -> Maybe (Comb Name)
    getSc (name, Fix x) = case x of
      ELam loc args body -> Just $ Def (VarName loc name) args body
      _                  -> Nothing


partitionBy :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionBy f xs = case xs of
  []   -> ([], [])
  y:ys ->
      let (bs, as) = partitionBy f ys
      in  case f y of
            Just b  -> (b:bs, as)
            Nothing -> (bs, y:as)

-- | Removes expressions like this
--
-- > let f = (let v = expr in v)
-- > in ...
--
-- substitutes to
--
-- > let f = expr
-- > in ...
--
-- It reduces the number of trivial let-bindings
fuseSingleLet :: ExprLam Name -> ExprLam Name
fuseSingleLet = cata $ \case
  ELet loc binds body -> Fix $ ELet loc (fmap procBinds binds) body
  other           -> Fix other
  where
    procBinds (var, expr) = case unFix expr of
      ELet _ [(v2, expr2)] (Fix (EVar _ v3)) | v2 == v3 -> (var, expr2)
      _                                             -> (var, expr)





