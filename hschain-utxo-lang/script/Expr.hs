{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Fix
import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang
import Hschain.Utxo.Lang.Build
import Data.Either
import Data.Text (Text)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

unExpr (Expr x) = x

lam' :: Text -> (Expr a -> Expr b) -> Expr (a -> b)
lam' name fun = lam name fun

pk' :: PublicKey -> Expr Bool
pk' = pk . text . publicKeyToText

id' :: Expr (a -> a)
id' = lam' "x" (\x -> x)

const' :: Expr (a -> b -> a)
const' = lam' "x" (\x -> lam' "y" $ \_ -> x)

flip' :: Expr (a -> b -> c) -> Expr (b -> a -> c)
flip' fun = lam' "x" (\x -> lam' "y" $ \y -> (app (app fun y) x))

e1 :: PublicKey -> Expr Bool
e1 key = app (lam' "x" $ \x -> x &&* pk' key) (10 <=* getHeight)

e2 :: PublicKey -> Expr Bool
e2 key = app (app fun (10 >* getHeight)) (pk' key)
  where
    fun = lam' "x" $ \x -> (lam' "y" $ \y -> (x ||* y))

e3 :: PublicKey -> Expr Bool
e3 key = app (app const' (10 >* getHeight)) (pk' key)

e4 :: PublicKey -> Expr Bool
e4 key = app (app (flip' const') (10 >* getHeight)) (pk' key)


e5 :: PublicKey -> Expr Bool
e5 key =
  def "const" const' $ \constFun ->
    app (app constFun (10 <=* getHeight)) (pk' key)

run :: Expr Bool -> IO ()
run (Expr a) = T.putStrLn $ either (T.pack . show) renderText $ runExec M.empty height ins outs $ execLang a
  where
    height = 99
    ins    = mempty
    outs   = mempty



