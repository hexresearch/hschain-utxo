module Hschain.Utxo.Lang.Desugar(
    unfoldLamList
  , unfoldLetArg
  , singleLet
  , explToImpl
) where

import Data.Fix

import Hschain.Utxo.Lang.Expr

import qualified Data.List as L

unfoldLamList :: [VarName] -> Lang -> Lang
unfoldLamList vars a = L.foldl' (\z a -> z . Fix . Lam a) id vars a

unfoldLetArg :: VarName -> [VarName] -> Lang -> Lang -> Lang
unfoldLetArg v args a = singleLet v (Fix $ LamList args a)

singleLet :: VarName -> Lang -> Lang -> Lang
singleLet v body expr = Fix $ Let bg expr
  where
    bg = BindGroup expl impl

    expl = []

    impl = [[Impl v [Alt [] body]]]

explToImpl :: Expl a -> Impl a
explToImpl Expl{..} = Impl expl'name expl'alts

