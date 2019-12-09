module Hschain.Utxo.Lang.Desugar(
    unfoldLamList
  , unfoldLetArg
  , singleLet
  , explToImpl
) where

import Data.Fix

import Type.Type
import Hschain.Utxo.Lang.Expr

import qualified Data.List as L

unfoldLamList :: Loc -> [VarName] -> Lang -> Lang
unfoldLamList loc vars a = L.foldl' (\z a -> z . Fix . Lam loc a) id vars a

unfoldLetArg :: Loc -> VarName -> [VarName] -> Lang -> Lang -> Lang
unfoldLetArg loc v args a = singleLet loc v (Fix $ LamList loc args a)

singleLet :: Loc -> VarName -> Lang -> Lang -> Lang
singleLet loc v body expr = Fix $ Let loc bg expr
  where
    bg = BindGroup expl impl

    expl = []

    impl = [[Impl (fromVarName v) [Alt [] body]]]

explToImpl :: Expl a -> Impl a
explToImpl Expl{..} = Impl expl'name expl'alts

