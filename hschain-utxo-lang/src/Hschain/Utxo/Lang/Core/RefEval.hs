-- |
module Hschain.Utxo.Lang.Core.RefEval
  ( Val(..)
  , evalProg
  ) where

import Data.Int
import qualified Data.Vector     as V
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy   as MapL

import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Data.Code
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.Primitives


data Val = ValP !Prim
         | ValBottom
         | ValF (Val -> Val)
         | ValCon Int [Val]


type GEnv = Map.Map Name Val
type LEnv = Map.Map Name Val


evalProg :: CoreProg -> Maybe Prim
evalProg (CoreProg prog) = do
  ValP p <- "main" `Map.lookup` genv
  return p
  where
    genv = MapL.fromList [ (scomb'name s, evalScomb genv s) | s <- prog ]

primVals :: Map.Map Name Val
primVals = fmap evalD builtInDiadic
  where
    evalD = \case
      Add -> arith2 (+)
      _   -> error "Not implemented"

arith2 :: (Int64 -> Int64 -> Int64) -> Val
arith2 f = ValF $ \case
  ValP (PrimInt a) -> ValF $ \case
    ValP (PrimInt b) -> ValP (PrimInt (f a b))
    _                -> ValBottom
  _ -> ValBottom

evalScomb :: GEnv -> Scomb -> Val
evalScomb genv Scomb{..} = buildArg Map.empty (V.toList scomb'args)
  where
    buildArg e (x:xs) = ValF $ \a -> buildArg (Map.insert (typed'value x) a e) xs
    buildArg e []     = evalExpr genv e $ typed'value scomb'body

evalExpr :: GEnv -> LEnv -> ExprCore -> Val
evalExpr genv = recur
  where
    evalVar lenv x
      | Just v <- x `Map.lookup` lenv     = v
      | Just v <- x `Map.lookup` genv     = v
      | Just v <- x `Map.lookup` primVals = v
      | otherwise                         = ValBottom
    recur lenv = \case
      EVar     x   -> evalVar lenv x
      EPolyVar x _ -> evalVar lenv x
      EPrim p -> ValP p
      EAp f x -> case recur lenv f of
                   ValF valF -> valF $ recur lenv x
                   _         -> ValBottom
      EIf e a b -> case recur lenv e of
        ValP (PrimBool f) -> recur lenv $ if f then a else b
        _                 -> ValBottom
      -- FIXME: Here we assume that let is completely nonrecursive
      --        (For simplicity)
      ELet binds body ->
        let lenv' = Map.fromList [ (nm, recur lenv e) | (nm,e) <- binds ]
                 <> lenv
        in recur lenv' body
      --
      ECase e alts -> case recur lenv e of
        ValCon tag fields -> match alts
          where
            match (CaseAlt{..} : cs)
              | tag == caseAlt'tag = recur (bindParams fields caseAlt'args lenv) caseAlt'rhs
              | otherwise          = match cs
            match [] = ValBottom
            --
            bindParams []             []             = id
            bindParams (v:vs) (Typed n _:ts) = bindParams vs ts . Map.insert n v
            bindParams _ _ = error "Type error in case"
        _ -> ValBottom
      EConstr _ tag arity    -> constr tag arity
      --
      EBottom{} -> ValBottom
    --

constr :: Int -> Int -> Val
constr tag arity = build
  (\v xs -> xs . (v:))
  (\f    -> ValCon tag (f []))
  id
  arity

build :: (Val -> a -> a) -> (a -> Val) -> a -> Int -> Val
build step fini = go
  where
    go a 0 = fini a
    go a i = ValF $ \v -> go (step v a) (i-1)
