-- | Example shows how to extend our limited language with
-- new primitives with context
module Main where

import Data.Text.Prettyprint.Doc

import Language.HM

import qualified Data.Map as M

varE' = varE ()
appE' = appE ()
absE' = absE ()
arrowT' = arrowT ()
conT' = conT ()
varT' = varT ()
appT' = appT ()

ctx = Context $ M.fromList
  [ ("+", monoT $ arrowT' intT (arrowT' intT intT))
  , ("negate", monoT $ arrowT' intT intT)
  , ("toDouble", monoT $ arrowT' intT doubleT)
  , ("length", forAllT () "a" $ monoT $ arrowT' (listT (varT' "a")) (varT' "a"))
  , ("int", monoT intT)
  , ("double", monoT doubleT)
  , ("intList", monoT $ listT intT)
  , ("toIntList", monoT $ arrowT' intT (listT intT))
  , ("singleton", forAllT () "a" $ monoT $ arrowT' (varT' "a") (listT (varT' "a")))
  , ("tuple2", forAllT () "a" $ forAllT () "b" $monoT $ arrowT' (varT' "a") (arrowT' (varT' "b") (tup2T (varT' "a") (varT' "b"))))
  , ("fst", forAllT () "a" $ forAllT () "b" $ monoT $ arrowT' (tup2T (varT' "a") (varT' "b")) (varT' "a"))
  , ("snd", forAllT () "a" $ forAllT () "b" $ monoT $ arrowT' (tup2T (varT' "a") (varT' "b")) (varT' "b"))
  ]
  where
    intT = conT' "Int"
    doubleT = conT' "Double"
    listT a = appT' (conT' "List") a
    tup2T a b = appT' (appT' (conT' "Tuple2") a) b

int = varE' "int"
double = varE' "double"
intList = varE' "intList"

app1 name a = appE' (varE' name) a
app2 name a b = appE' (appE' (varE' name) a) b

singleton = app1 "singleton"
plus a b = appE' (appE' (varE' "+") a) b
len = app1 "length"
tup = app2 "tuple2"
fst' = app1 "fst"
snd' = app1 "snd"

expr1 = absE' "x" $ appE' (varE' "singleton") $ appE' (varE' "negate") (appE' (appE' (varE' "+") int) int)
expr2 = singleton $ singleton $ len (varE' "intList")
expr3 = singleton $ tup int double

