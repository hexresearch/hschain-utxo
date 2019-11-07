{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Fix
import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build
import Data.Either

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

unExpr (Expr x) = x

lam' :: VarName -> (Expr a -> Expr b) -> Expr (a -> b)
lam' name fun = lam name (Fix BoolType) fun


id' :: Expr (a -> a)
id' = lam' "x" (\x -> x)

const' :: Expr (a -> b -> a)
const' = lam' "x" (\x -> lam' "y" $ \_ -> x)

flip' :: Expr (a -> b -> c) -> Expr (b -> a -> c)
flip' fun = lam' "x" (\x -> lam' "y" $ \y -> (app (app fun y) x))

e1 :: Expr Bool
e1 = app (lam' "x" $ \x -> x &&* pk "jack-pk") (10 <=* getHeight)

e2 :: Expr Bool
e2 = app (app fun (10 >* getHeight)) (pk "jack-pk")
  where
    fun = lam' "x" $ \x -> (lam' "y" $ \y -> (x ||* y))

e3 :: Expr Bool
e3 = app (app const' (10 >* getHeight)) (pk "jack-pk")

e4 :: Expr Bool
e4 = app (app (flip' const') (10 >* getHeight)) (pk "jack-pk")


e5 :: Expr Bool
e5 =
  def "const" const' $ \constFun ->
    app (app constFun (10 <=* getHeight)) (pk "jack-pk")

run :: Expr Bool -> IO ()
run (Expr a) = T.putStrLn $ either (T.pack . show) renderText $ runExec proof height ins outs $ execLang a
  where
    proof  = Proof $ S.fromList ["jack-pk"]
    height = 99
    ins    = mempty
    outs   = mempty



