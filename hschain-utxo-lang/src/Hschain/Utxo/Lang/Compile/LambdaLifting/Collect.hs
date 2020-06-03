module Hschain.Utxo.Lang.Compile.LambdaLifting.Collect(
  collect
) where

import Control.Monad.Writer.Strict

import Data.Fix
import Data.Foldable
import Data.Sequence (Seq)

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.Sequence   as Seq

type CollectM a = Writer (Seq (Comb Name)) a

runCollectM :: CollectM a -> (a, CoreProg)
runCollectM a = (res, toList combs)
  where
    (res, combs) = runWriter a

collect :: CoreProg -> CoreProg
collect prog = scs <> prog'
  where
    (prog', scs) = runCollectM $ mapM (mapM collectExpr) prog

collectExpr :: Expr Name -> CollectM (Expr Name)
collectExpr = cataM $ \case
  ELet binds body -> letExpr binds body
  other           -> pure $ Fix other
  where
    letExpr binds body = do
      tell (Seq.fromList scs)
      return $
        if null nonScs
          then body
          else Fix $ ELet nonScs body
      where
        (scs, nonScs) = partitionBy  getSc binds

    getSc :: (Name, Expr Name) -> Maybe (Comb Name)
    getSc (name, Fix x) = case x of
      ELam args body -> Just $ Def name args body
      _              -> Nothing


partitionBy :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionBy f xs = case xs of
  []   -> ([], [])
  y:ys ->
      let (bs, as) = partitionBy f ys
      in  case f y of
            Just b  -> (b:bs, as)
            Nothing -> (bs, y:as)


